{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
-- |
-- Evaluation of python expressions.
module Python.Internal.Eval
  ( -- * Evaluator
    runPy
  , unPy
    -- * Initialization
  , initializePython
  , finalizePython
  , withPython
    -- * PyObject wrapper
  , newPyObject
  , decref
    -- * Exceptions
  , convertHaskell2Py
  , convertPy2Haskell
  , throwPyError
  , throwPyConvesionFailed
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Cont
import Foreign.Concurrent        qualified as GHC
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Marshal
import Foreign.Storable
import System.Environment
import System.IO.Unsafe

import Language.C.Inline          qualified as C
import Language.C.Inline.Unsafe   qualified as CU

import Python.Internal.Types
import Python.Internal.Util
import Python.Internal.Program

----------------------------------------------------------------
C.context (C.baseCtx <> pyCtx)
C.include "<inline-python.h>"
----------------------------------------------------------------

-- NOTE: [Python and threading]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Interaction with python interpreter and haskell's threading drives
-- important design decisions:
--
--  1. Python is essentially single threaded. Before any interaction
--     with interpreter one must acquire GIL.
--
--  2. PyGILState_{Ensure,Release} use thread local state.
--
-- With single-threaded RTS this poses no problem. We can call
-- python's C API in any way we like. With multithreaded runtime it
-- turns into big and unwieldy problem. RTS migrates haskell's green
-- threads between OS threads at will. And we can't event acquire GIL
-- since it uses thread-local storage.
--
-- So only choice for multithreaded RTS is to create bound thread and
-- perform all python evaluation on it. So we need to separate plain
-- haskell IO and IO which calls C API for that reason we have `Py`
-- monad.
--
--
-- Running `Py` depends on RTS. In single-threaded we treat is plain
-- IO. In multithreaded RTS we create new `PyEvalReq` containing fresh
-- MVars and send it to worker thread. Then we await result from MVar.
--
-- For handling of exceptions see [Threading and exceptions]
--
--
-- Finally last round of problems is GC. We cannot simply call
-- Py_DECREF in any thread. So we have to be build list of Ptrs to be
-- DECREF'ed and worker thread would traverse that list when it gets
-- to it.



-- NOTE: [Threading and exceptions]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- In case of multithreaded runtime we execute python code on separate
-- thread and thus we have problem of juggling exception between
-- threads. Returning exception thrown by python code is simple:
-- result is returned as `MVar (Either SomeException a)`. Passing
-- exception to python much more complicated.
--
-- In single threaded case it's simple: we're in the middle of C call
-- and there's no one else to interrupt computation.
--
-- In multithreaded case haskell code which communicate with worker
-- thread could be killed by (async) exception. And this means we must
-- interrupt python. Consider following code:
--
-- > [py| while True: pass |]
--
-- Interrupting only haskell means it will continue running blocking
-- python interpreter forever and wasting CPU.
--
-- For each evaluation request we keep it status (EvalStatus). When
-- thread waiting for result of evaluation gets exception it tries to
-- cancel evaluation if it hasn't started yet. If it had started we
-- call `PyErr_SetInterrupt` which is equivalent of sending SIGINT to
-- python.



-- NOTE: [Async exceptions]
-- ~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Code interacting with python interpreter interleaves C calls and
-- haskell code. It's impossible to ensure invariant that python
-- interpreter demands from us if computation could be interrupted in
-- arbitrary points.
--
-- So solution is to execute such code with async exceptions masked.
-- `runPy` and friends should ensure that.
--
-- This may cause problems with callbacks to haskell. Async exceptions
-- won't reach worker thread, and python interpreter couldn't be
-- interrupted as well. We poll for that at beginning of callback.
-- We'll see whether it causes problems in practice.



----------------------------------------------------------------
-- Data types used for multithreaded RTS
----------------------------------------------------------------

-- | Status of evaluation request for python interprereter
data EvalStatus
  = Pending   -- ^ Request has not started evaluation
  | Running   -- ^ Evaluation is in progress
  | Cancelled -- ^ Request was cancelled
  | Done      -- ^ Request finished evaluation
  deriving stock Show

-- | Evaluation request sent to
data PyEvalReq = forall a. PyEvalReq
  { prog   :: (Py a)
  , result :: MVar (Either SomeException a)
  , status :: MVar EvalStatus
  }

-- | List of pointer. We use it instead of list in order to save on
--   allocations
data PyObjectList
  = Nil
  | Cons !(Ptr PyObject) PyObjectList


-- Python evaluator reads messages from this MVar
toPythonThread :: MVar PyEvalReq
toPythonThread = unsafePerformIO newEmptyMVar
{-# NOINLINE toPythonThread #-}

-- ThreadId of thread running python's interpreter
pythonInterpreter :: MVar ThreadId
pythonInterpreter = unsafePerformIO newEmptyMVar
{-# NOINLINE pythonInterpreter #-}

-- List of python object waiting for Py_DECREF
toDECREF :: MVar PyObjectList
toDECREF = unsafePerformIO $ newMVar Nil
{-# NOINLINE toDECREF #-}


----------------------------------------------------------------
-- Execution of Py monad
----------------------------------------------------------------

-- | Execute python action.
runPy :: Py a -> IO a
-- See NOTE: [Python and threading]
-- See NOTE: [Threading and exceptions]
runPy py
  -- Multithreaded RTS
  | rtsSupportsBoundThreads = do
      result <- newEmptyMVar
      status <- newMVar Pending
      let onExc :: SomeException -> IO b
          onExc e = do
            modifyMVar_ status $ \case
              Pending   -> pure Cancelled
              Cancelled -> pure Cancelled
              Done      -> pure Done
              Running   -> Cancelled <$ [CU.exp| void { PyErr_SetInterrupt() } |]
            throwIO e
      (do putMVar toPythonThread $ PyEvalReq{ prog=py, ..}
          takeMVar result >>= \case
            Left  e -> throwIO e
            Right a -> pure a
        ) `catch` onExc
  -- Single-threaded RTS
  --
  -- See NOTE: [Async exceptions]
  | otherwise = mask_ $ unPy py


-- | Execute python action. This function is unsafe and should be only
--   called in thread of interpreter.
unPy :: Py a -> IO a
unPy (Py io) = io



----------------------------------------------------------------
-- Initialization of interpreter
----------------------------------------------------------------

-- | Initialize python interpreter. It's safe call this function
--   multiple times.
initializePython :: IO ()
initializePython
  | rtsSupportsBoundThreads = tryReadMVar pythonInterpreter >>= \case
      Just _  -> pure ()
      Nothing -> do
        pid <- forkOSWithUnmask $ \unmask -> unmask $ do
          (doInializePython >> forever evalReq) `finally` doFinalizePython
        putMVar pythonInterpreter pid
  | otherwise = doInializePython
-- See NOTE: [Python and threading]

-- | Destroy python interpreter.
finalizePython :: IO ()
finalizePython
  | rtsSupportsBoundThreads = tryReadMVar pythonInterpreter >>= \case
      Just pid -> killThread pid
      Nothing  -> pure ()
  | otherwise = doFinalizePython
-- See NOTE: [Python and threading]

-- | Bracket which ensures that action is executed with properly
--   initialized interpreter
withPython :: IO a -> IO a
withPython = bracket_ initializePython finalizePython



doInializePython :: IO ()
doInializePython = do
  -- NOTE: I'd like more direct access to argv
  argv0 <- getProgName
  argv  <- getArgs
  let n_argv = fromIntegral $ length argv + 1
  -- FIXME: For some reason sys.argv is initialized incorrectly. No
  --        easy way to debug. Will do for now
  r <- evalContT $ do
    p_argv0  <- ContT $ withWCString argv0
    p_argv   <- traverse (ContT . withWCString) argv
    ptr_argv <- ContT $ withArray (p_argv0 : p_argv)
    liftIO [C.block| int {
      // Noop is interpreter is already initialized
      if( Py_IsInitialized() ) {
          return 0;
      }
      // Now fill config
      PyStatus status;
      PyConfig cfg;
      PyConfig_InitPythonConfig( &cfg );
      cfg.parse_argv = 0;
      //----------------
      status = PyConfig_SetBytesString(&cfg, &cfg.program_name, "XX");
      if( PyStatus_Exception(status) ) {
          goto error;
      }
      //----------------
      status = PyConfig_SetArgv(&cfg,
          $(int       n_argv),
          $(wchar_t** ptr_argv)
      );
      if( PyStatus_Exception(status) ) {
          goto error;
      };
      // Initialize interpreter
      status = Py_InitializeFromConfig(&cfg);
      if( PyStatus_Exception(status) ) {
          goto error;
      };
      PyConfig_Clear(&cfg);
      return 0;
      // Error case
      error:
      PyConfig_Clear(&cfg);
      return 1;
      } |]
  case r of 0 -> pure ()
            _ -> error "Failed to initialize interpreter"

doFinalizePython :: IO ()
doFinalizePython = [C.block| void {
  if( Py_IsInitialized() ) {
      Py_Finalize();
  }
  } |]


-- Evaluate python code in multithreaded RTS
evalReq :: IO ()
-- See NOTE: [Python and threading]
-- See NOTE: [Threading and exceptions]
evalReq = do
  PyEvalReq{prog=Py io, result, status} <- takeMVar toPythonThread
  -- GC
  let decrefList Nil = pure ()
      decrefList (p `Cons` ps) = do [CU.exp| void { Py_XDECREF($(PyObject* p)) } |]
                                    decrefList ps
  decrefList =<< modifyMVar toDECREF (\xs -> pure (Nil, xs))
  -- Update status of request
  do_eval <- modifyMVar status $ \case
    Running   -> error "Python evaluator: Internal error. Got 'Running' request"
    Done      -> error "Python evaluator: Internal error. Got 'Done' request"
    Cancelled -> return (Cancelled,False)
    Pending   -> return (Running,  True)
  when do_eval $ do
    a <- (Right <$> mask_ io) `catches`
         [ Handler $ \(e :: AsyncException)     -> throwIO e
         , Handler $ \(e :: SomeAsyncException) -> throwIO e
         , Handler $ \(e :: SomeException)      -> pure (Left e)
         ]
    modifyMVar_ status $ \_ -> pure Done
    -- It's possible that calling thread raised signal using
    -- PyErr_SetInterrupt after we finished execution. At this point
    -- we need to clear signals.
    --
    -- FIXME: Is this right way to do this?
    -- FIXME: Do I need to clear exceptions as well?
    [CU.exp| void { PyErr_CheckSignals() } |]
    putMVar result a


----------------------------------------------------------------
-- Creation of PyObject
----------------------------------------------------------------

decref :: Ptr PyObject -> Py ()
decref p = Py [CU.exp| void { Py_DECREF($(PyObject* p)) } |]

-- | Wrap raw python object into
newPyObject :: Ptr PyObject -> Py PyObject
-- We need to use different implementation for different RTS
-- See NOTE: [Python and threading]
newPyObject p
  | rtsSupportsBoundThreads = Py $ do
      fptr <- newForeignPtr_ p
      GHC.addForeignPtrFinalizer fptr $ modifyMVar_ toDECREF (pure . Cons p)
      pure $ PyObject fptr
  | otherwise = Py $ do
      fptr <- newForeignPtr_ p
      PyObject fptr <$ addForeignPtrFinalizer py_XDECREF fptr

py_XDECREF :: FunPtr (Ptr PyObject -> IO ())
py_XDECREF = [C.funPtr| void inline_py_XDECREF(PyObject* p) { Py_XDECREF(p); } |]



----------------------------------------------------------------
-- Conversion of exceptions
----------------------------------------------------------------

-- | Convert haskell exception to python exception. Always returns
--   NULL.
convertHaskell2Py :: SomeException -> Py (Ptr PyObject)
convertHaskell2Py err = Py $ do
  withCString ("Haskell exception: "++show err) $ \p_err -> do
    [CU.block| PyObject* {
      PyErr_SetString(PyExc_RuntimeError, $(char *p_err));
      return NULL;
      } |]

-- | Convert python exception to haskell exception. Should only be
--   called if there's unhandled python exception. Clears exception.
convertPy2Haskell :: Py PyError
convertPy2Haskell = evalContT $ do
  p_err <- withPyAlloca @(Ptr CChar)
  -- Return 0 and set error message if there's python error
  r     <- liftIO [CU.block| int {
    char **p_msg = $(char **p_err);
    PyObject *e_type, *e_value, *e_trace;
    // Fetch python's error
    PyErr_Fetch( &e_type, &e_value, &e_trace);
    if( NULL == e_value ) {
        return -1;
    }
    // Convert to python string object
    PyObject *e_str = PyObject_Str(e_value);
    if( NULL == e_str ) {
        *p_msg = 0;
        return 0;
    }
    // Convert to UTF8 C string
    Py_ssize_t len;
    const char *err_msg = PyUnicode_AsUTF8AndSize(e_str, &len);
    if( 0 == e_str ) {
        Py_DECREF(e_str);
        *p_msg = 0;
        return 0;
    }
    // Copy message
    *p_msg = malloc(len+1);
    strncpy(*p_msg, err_msg, len);
    Py_DECREF(e_str);
    return 0;
    } |]
  liftIO $ case r of
    0 -> peek p_err >>= \case
      NULL  -> pure $ PyError "CANNOT SHOW EXCEPTION"
      c_err -> do
        s <- peekCString c_err
        free c_err
        pure $ PyError s
    _ -> error "No python exception raised"

-- | Throw python error as haskell exception if it's raised.
throwPyError :: Py ()
throwPyError = 
  Py [CU.exp| PyObject* { PyErr_Occurred() } |] >>= \case
    NULL -> pure ()
    _    -> throwPy =<< convertPy2Haskell

throwPyConvesionFailed :: Py ()
throwPyConvesionFailed = do
  r <- Py [CU.block| int {
    if( PyErr_Occurred() ) {
        PyErr_Clear();
        return 1;
    }
    return 0;
    } |]
  case r of
    0 -> pure ()
    _ -> throwPy FromPyFailed 
