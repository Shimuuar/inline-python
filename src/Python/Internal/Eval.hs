{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
-- |
-- Evaluation of python expressions.
module Python.Internal.Eval
  ( -- * Locks
    ensurePyLock
  , callbackEnsurePyLock
    -- * Initialization
  , initializePython
  , finalizePython
  , withPython
    -- * Evaluator
  , runPy
  , runPyInMain
  , unsafeRunPy
    -- * GC-related
  , newPyObject
    -- * C-API wrappers
  , takeOwnership
  , ensureGIL
  , dropGIL
    -- * Exceptions
  , convertHaskell2Py
  , convertPy2Haskell
  , checkThrowPyError
  , mustThrowPyError
  , checkThrowBadPyType
  , throwOnNULL
    -- * Exec & eval
  , Namespace(..)
  , Main(..)
  , Temp(..)
  , Dict(..)
  , DictPtr(..)
  , Module(..)
  , ModulePtr(..)
  , unsafeWithCode
  , eval
  , exec
    -- * Debugging
  , debugPrintPy
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception         (interruptible)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Cont
import Data.Maybe
import Data.Function
import Data.ByteString.Unsafe    qualified as BS
import Foreign.Concurrent        qualified as GHC
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Storable
import System.Environment
import System.IO.Unsafe

import Language.C.Inline          qualified as C
import Language.C.Inline.Unsafe   qualified as CU

import Python.Internal.CAPI
import Python.Internal.Types
import Python.Internal.Util
import Python.Internal.Program


----------------------------------------------------------------
C.context (C.baseCtx <> pyCtx)
C.include "<inline-python.h>"
----------------------------------------------------------------

-- NOTE: [Python and threading]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Python (cpython to be precise) support threading to and it
-- interacts with haskell threading in interesting and generally
-- unpleasant ways. In short python's threads are:
--
--  1. OS threads. Python is designed to be embeddable and can
--     live with threads scheduled by outside python's runtime.
--
--  2. Any OS thread interacting with python interpreter must hold
--     global interpreter lock (GIL).
--
--  3. GIL uses thread local state.
--
-- Haskell has two runtimes. Single threaded one doesn't cause any
-- troubles and won't be discussed further. Multithreaded one
-- implement N-M threading and schedules N green thread on M OS
-- threads as it see fit.
--
-- One could think that running python code in bound threads and
-- making sure that GIL is held would suffice. It doesn't. Doing so
-- would quickly results in deadlock. Exact reason for that is not
-- understood.
--
-- Another problem is GHC may schedule two threads each running python
-- code on same capability. They won't have any problems taking GIL
-- and will run concurrently stepping on each other's toes.
--
-- Only way to solve this problem is to introduce another lock on
-- haskell side. It's visible to haskell RTS so we won't get deadlocks
-- and it makes sure that only one haskell thread interacts with
-- python at a time.
--
--
--
-- Also python designate thread in which python interpreter was
-- initialized as a main thread. It has special status for example
-- some libraries may run only in main thread (e.g. tkinter). But if
-- we don't take special precautions we won't know which thread it
-- is.
--
--
--
-- There's of course question how well python threading interacts with
-- haskell. No one knows, probably it won't work well.



-- NOTE: [GC]
-- ~~~~~~~~~~
--
-- CPython uses reference counting which should work very well with
-- ForeignPtr. But there's a catch: decrementing counter is only
-- possible if one holds GIL. Taking GIL may block and doing so during
-- GC may eventually will block GC thread and the whole program.
--
-- Current solution is not quite satisfactory: finalizer writes
-- pointer to `Chan` which delivers it to thread which decrements
-- counter. It's not very good solution since we need to take locks
-- for each DECREF which is relatively costly (O(1μs)). But better
-- solutions are not obvious.
--
-- Problem above is only relevant for multithreaded RTS there's no
-- other threads that could hold lock and taking GIL can't fail.



-- NOTE: [Interrupting python]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Being able to interrupt python when haskell exception arrives is
-- surely nice. However it's difficult and comes with tradeoffs.
--
-- First of all call must be done in a separate thread otherwise
-- there's no one to catch exception and to something. This also means
-- that python calls made using plain FFI are not interruptible.
--
-- In addition python's ability to notify other threads are limited:
--
--  + `Py_SetInterrupt` plain doesn't work. It uses signal which trips
--    up haskell RTS as well.
--
--  + `PyThreadState_SetAsyncExc` could be use but it requires special
--    setup from thread being interrupted.



----------------------------------------------------------------
-- Lock and global state
----------------------------------------------------------------

globalPyState :: TVar PyState
globalPyState = unsafePerformIO $ newTVarIO NotInitialized
{-# NOINLINE globalPyState #-}

globalPyLock :: TVar PyLock
globalPyLock = unsafePerformIO $ newTVarIO LockUninialized
{-# NOINLINE globalPyLock #-}


-- | State of python interpreter
data PyState
  = NotInitialized
    -- ^ Initialization is not done. Initial state.
  | InInitialization
    -- ^ Interpreter is being initialized.
  | InitFailed
    -- ^ Initialization was attempted but failed for whatever reason.
  | Running1
    -- ^ Interpreter is running. We're using single threaded RTS
  | RunningN !(Chan (Ptr PyObject))
             !(MVar EvalReq)
             !ThreadId
             !ThreadId
    -- ^ Interpreter is running. We're using multithreaded RTS
  | InFinalization
    -- ^ Interpreter is being finalized.
  | Finalized
    -- ^ Interpreter was shut down.


-- | Lock. It's necessary for lock to reentrant since thread may take
--   it several times for example by nesting 'runPy'. We use
--   'ThreadId' as a key to figure out whether thread may retake lock
--   or not.
--
--   Another special case is callbacks. Callback (via 'FunPtr') will
--   start new haskell thread so we need to add primitive for grabbing
--   lock regardless of current 'ThreadId'
data PyLock
  = LockUninialized
    -- ^ There's no interpreter and lock does not exist.
  | LockUnlocked
    -- ^ Lock could be taked
  | Locked !ThreadId [ThreadId]
    -- ^ Python is locked by given thread. Lock could be taken multiple
    --   times
  | LockedByGC
    -- ^ Python is locked by GC thread.
  | LockFinalized
    -- ^ Python interpreter shut down. Taking lock is not possible
  deriving Show

-- | Execute code ensuring that python lock is held by current thread.
ensurePyLock :: IO a -> IO a
ensurePyLock action = do
  tid <- myThreadId
  bracket_ (atomically $ acquireLock tid)
           (atomically $ releaseLock tid)
           action

-- | Retake lock regardless of thread which hold lock. Lock must be
--   already taken. Caller must make sure that thread holding lock is
--   block for duration of action.
--
--   This is very unsafe. It must be used only in callbacks from
--   python to haskell
callbackEnsurePyLock :: IO a -> IO a
callbackEnsurePyLock action = do
  tid <- myThreadId
  bracket_ (atomically $ grabLock tid)
           (atomically $ releaseLock tid)
           action


acquireLock :: ThreadId -> STM ()
acquireLock tid = readTVar globalPyLock >>= \case
  LockUninialized -> throwSTM PythonNotInitialized
  LockFinalized   -> throwSTM PythonIsFinalized
  LockedByGC      -> retry
  LockUnlocked    -> writeTVar globalPyLock $ Locked tid []
  Locked t xs
    | t == tid  -> writeTVar globalPyLock $ Locked t (t : xs)
    | otherwise -> retry

grabLock :: ThreadId -> STM ()
grabLock tid = readTVar globalPyLock >>= \case
  LockUninialized -> throwSTM PythonNotInitialized
  LockFinalized   -> throwSTM PythonIsFinalized
  LockedByGC      -> retry
  LockUnlocked    -> writeTVar globalPyLock $ Locked tid []
  Locked t xs     -> writeTVar globalPyLock $ Locked tid (t : xs)

releaseLock :: ThreadId -> STM ()
releaseLock tid = readTVar globalPyLock >>= \case
  LockUninialized -> throwSTM PythonNotInitialized
  LockFinalized   -> throwSTM PythonIsFinalized
  LockUnlocked    -> throwSTM $ PyInternalError "releaseLock: releasing LockUnlocked"
  LockedByGC      -> throwSTM $ PyInternalError "releaseLock: releasing LockedByGC"
  Locked t xs
    | t /= tid  -> throwSTM $ PyInternalError "releaseLock: releasing  wrong lock"
    | otherwise -> writeTVar globalPyLock $! case xs of
        []    -> LockUnlocked
        t':ts -> Locked t' ts



----------------------------------------------------------------
-- Initialization and finalization
----------------------------------------------------------------

-- | Initialize python interpreter. If interpreter is already
--   initialized it's a noop. Calling after python was shut down will
--   result in error.
initializePython :: IO ()
-- See NOTE: [Python and threading]
initializePython = [CU.exp| int { Py_IsInitialized() } |] >>= \case
  0 | rtsSupportsBoundThreads -> runInBoundThread $ doInializePython
    | otherwise               -> doInializePython
  _ -> pure ()

-- | Destroy python interpreter.
finalizePython :: IO ()
finalizePython = join $ atomically $ readTVar globalPyState >>= \case
  NotInitialized   -> throwSTM PythonNotInitialized
  InitFailed       -> throwSTM PythonIsFinalized
  Finalized        -> pure $ pure ()
  InInitialization -> retry
  InFinalization   -> retry
  -- We can simply call Py_Finalize
  Running1 -> checkLock $ [C.block| void {
    PyGILState_Ensure();
    Py_Finalize();
    } |]
  -- We need to call Py_Finalize on main thread
  RunningN _ lock_eval _ tid_gc -> checkLock $ do
    killThread tid_gc
    resp <- newEmptyMVar
    putMVar lock_eval $ StopReq resp
    takeMVar resp
  where
    checkLock action = readTVar globalPyLock >>= \case
      LockUninialized -> throwSTM $ PyInternalError "finalizePython LockUninialized"
      LockFinalized   -> throwSTM $ PyInternalError "finalizePython LockFinalized"
      Locked{}        -> retry
      LockedByGC      -> retry
      LockUnlocked    -> do
        writeTVar globalPyLock  LockFinalized
        writeTVar globalPyState Finalized
        pure action

-- | Bracket which ensures that action is executed with properly
--   initialized interpreter
withPython :: IO a -> IO a
withPython = bracket_ initializePython finalizePython


doInializePython :: IO ()
doInializePython = do
  -- First we need to grab global python lock on haskell side
  join $ atomically $ do
    readTVar globalPyState >>= \case
      Finalized        -> throwSTM PythonNotInitialized
      InitFailed       -> throwSTM PythonIsFinalized
      InInitialization -> retry
      InFinalization   -> retry
      Running1{}       -> pure $ pure ()
      RunningN{}       -> pure $ pure ()
      NotInitialized   -> do
        writeTVar globalPyState InInitialization
        let fini st = atomically $ do
              writeTVar globalPyState $ st
              writeTVar globalPyLock  $ LockUnlocked
        pure $
          (mask_ $ if
            -- On multithreaded runtime create bound thread to make
            -- sure we can call python in its main thread.
            | rtsSupportsBoundThreads -> do
                lock_init <- newEmptyMVar
                lock_eval <- newEmptyMVar
                -- Main thread
                tid_main <- forkOS $ mainThread lock_init lock_eval
                takeMVar lock_init >>= \case
                  True  -> pure ()
                  False -> throwM PyInitializationFailed
                -- GC thread
                gc_chan <- newChan
                tid_gc  <- forkOS $ gcThread gc_chan
                fini $ RunningN gc_chan lock_eval tid_main tid_gc
            -- Nothing special is needed on single threaded RTS
            | otherwise -> do
                doInializePythonIO >>= \case
                  True  -> pure ()
                  False -> throwM PyInitializationFailed
                fini Running1
          ) `onException` atomically (writeTVar globalPyState InitFailed)

-- This action is executed on python's main thread
mainThread :: MVar Bool -> MVar EvalReq -> IO ()
mainThread lock_init lock_eval = do
  r_init <- doInializePythonIO
  putMVar lock_init r_init
  case r_init of
    False -> pure ()
    True  -> mask_ $ fix $ \loop ->
      (takeMVar lock_eval `catch` (\InterruptMain -> pure HereWeGoAgain)) >>= \case
        EvalReq py resp -> do
          res <- (Right <$> runPy py) `catch` (pure . Left)
          putMVar resp res
          loop
        StopReq resp -> do
          [C.block| void {
            PyGILState_Ensure();
            Py_Finalize();
            } |]
          putMVar resp ()
        HereWeGoAgain -> loop


doInializePythonIO :: IO Bool
doInializePythonIO = do
  -- FIXME: I'd like more direct access to argv
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
      // Now fill config
      PyStatus status;
      PyConfig cfg;
      PyConfig_InitPythonConfig( &cfg );
      cfg.parse_argv              = 0;
      cfg.install_signal_handlers = 0;
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
      // Release GIL so other threads may take it
      PyEval_SaveThread();
      return 0;
      // Error case
      error:
      PyConfig_Clear(&cfg);
      return 1;
      } |]
  return $! r == 0


----------------------------------------------------------------
-- Running Py monad
----------------------------------------------------------------

data EvalReq
  = forall a. EvalReq (Py a) (MVar (Either SomeException a))
  | StopReq (MVar ())
  | HereWeGoAgain

data InterruptMain = InterruptMain
  deriving stock    Show
  deriving anyclass Exception

-- | Execute python action. It will take and hold global lock while
--   code is executed. Python exceptions raised during execution are
--   converted to haskell exception 'PyError'.
runPy :: Py a -> IO a
-- See NOTE: [Python and threading]
runPy py
  | rtsSupportsBoundThreads = runInBoundThread go -- Multithreaded RTS
  | otherwise               = go                  -- Single-threaded RTS
  where
    -- We check whether interpreter is initialized. Throw exception if
    -- it wasn't. Better than segfault isn't it?
    go = ensurePyLock $ mask_ $ unsafeRunPy (ensureGIL py)

-- | Same as 'runPy' but will make sure that code is run in python's
--   main thread. It's thread in which python's interpreter was
--   initialized. Some python's libraries may need that. It has higher
--   call overhead compared to 'runPy'.
runPyInMain :: Py a -> IO a
-- See NOTE: [Python and threading]
runPyInMain py
  -- Multithreaded RTS
  | rtsSupportsBoundThreads = do
      tid <- myThreadId
      bracket (acquireMain tid) fst snd
  -- Single-threaded RTS
  | otherwise = runPy py
  where
    acquireMain tid = atomically $ readTVar globalPyState >>= \case
      NotInitialized   -> throwSTM PythonNotInitialized
      InitFailed       -> throwSTM PyInitializationFailed
      Finalized        -> throwSTM PythonIsFinalized
      InInitialization -> retry
      InFinalization   -> retry
      Running1         -> throwSTM $ PyInternalError "runPyInMain: Running1"
      RunningN _ eval_lock tid_main _ -> readTVar globalPyLock >>= \case
        LockUninialized -> throwSTM PythonNotInitialized
        LockFinalized   -> throwSTM PythonIsFinalized
        LockedByGC      -> retry
        -- We need to send closure to main python thread when we're grabbing lock.
        LockUnlocked    -> do
          writeTVar globalPyLock $ Locked tid_main []
          pure ( atomically (releaseLock tid_main)
               , evalInOtherThread tid_main eval_lock
               )
        -- If we513 can grab lock and main thread taken lock we're
        -- already executing on main thread. We can simply execute code
        Locked t ts
          | t /= tid
            -> retry
          | t == tid_main || (tid_main `elem` ts) -> do
              writeTVar globalPyLock $ Locked t (t : ts)
              pure ( atomically (releaseLock t)
                   , unsafeRunPy $ ensureGIL py
                   )
          | otherwise -> do
              writeTVar globalPyLock $ Locked tid_main (t : ts)
              pure ( atomically (releaseLock tid_main)
                   , evalInOtherThread tid_main eval_lock
                   )
    --
    evalInOtherThread tid_main eval_lock = do
      r <- mask_ $ do resp <- newEmptyMVar
                      putMVar eval_lock $ EvalReq py resp
                      takeMVar resp `onException` throwTo tid_main InterruptMain
      either throwM pure r


-- | Execute python action. This function is unsafe and should be only
--   called in thread of interpreter.
unsafeRunPy :: Py a -> IO a
unsafeRunPy (Py io) = io



----------------------------------------------------------------
-- GC-related functions
----------------------------------------------------------------

-- | Wrap raw python object into
newPyObject :: Ptr PyObject -> Py PyObject
-- See NOTE: [GC]
newPyObject p = Py $ do
  fptr <- newForeignPtr_ p
  GHC.addForeignPtrFinalizer fptr $
    readTVarIO globalPyState >>= \case
      RunningN ch _ _ _  -> writeChan ch p
      Running1           -> singleThreadedDecrefCG p
      _                  -> pure ()
  pure $ PyObject fptr

-- | Thread doing garbage collection for python object in
--   multithreaded runtime.
gcThread :: Chan (Ptr PyObject) -> IO ()
gcThread ch = forever $ do
  decrefGC =<< readChan ch

decrefGC :: Ptr PyObject -> IO ()
decrefGC p = join $ atomically $ readTVar globalPyLock >>= \case
  LockUninialized -> pure $ pure ()
  LockFinalized   -> pure $ pure ()
  LockedByGC      -> pure $ pure ()
  Locked{}        -> retry
  LockUnlocked    -> do
    writeTVar globalPyLock LockedByGC
    pure $ do
      gcDecref p `finally` atomically (writeTVar globalPyLock LockUnlocked)

singleThreadedDecrefCG :: Ptr PyObject -> IO ()
singleThreadedDecrefCG p = readTVarIO globalPyLock >>= \case
  LockUninialized -> pure ()
  LockFinalized   -> pure ()
  LockedByGC      -> gcDecref p
  Locked{}        -> gcDecref p
  LockUnlocked    -> gcDecref p

gcDecref :: Ptr PyObject -> IO ()
gcDecref p = [CU.block| void {
  PyGILState_STATE st = PyGILState_Ensure();
  Py_XDECREF( $(PyObject* p) );
  PyGILState_Release(st);
  } |]


----------------------------------------------------------------
-- C-API wrappers
----------------------------------------------------------------

-- | Ensure that we hold GIL for duration of action
ensureGIL :: Py a -> Py a
ensureGIL action = do
  -- NOTE: We're cheating here and looking behind the veil.
  --       PyGILState_STATE is defined as enum. Let hope it will stay
  --       this way.
  gil_state <- Py [CU.exp| int { PyGILState_Ensure() } |]
  action `finally` Py [CU.exp| void { PyGILState_Release($(int gil_state)) } |]

-- | Drop GIL temporarily
dropGIL :: IO a -> Py a
dropGIL action = do
  -- NOTE: We're cheating here and looking behind the veil.
  --       PyGILState_STATE is defined as enum. Let hope it will stay
  --       this way.
  st <- Py [CU.exp| PyThreadState* { PyEval_SaveThread() } |]
  Py $ interruptible action
        `finally` [CU.exp| void { PyEval_RestoreThread($(PyThreadState *st)) } |]


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
convertPy2Haskell :: Py PyException
convertPy2Haskell = runProgram $ do
  p_errors <- withPyAllocaArray @(Ptr PyObject) 3
  -- Fetch error indicator
  (p_type, p_value) <- progIO $ do
    [CU.block| void {
       PyObject **p = $(PyObject** p_errors);
       PyErr_Fetch(p, p+1, p+2);
       }|]
    p_type  <- peekElemOff p_errors 0
    p_value <- peekElemOff p_errors 1
    -- Traceback is not used ATM
    pure (p_type,p_value)
  -- Convert exception type and value to strings.
  progPy $ do
    s_type  <- pyobjectStrAsHask p_type
    s_value <- pyobjectStrAsHask p_value
    incref p_value
    exc     <- newPyObject p_value
    let bad_str = "__str__ call failed"
    pure $ PyException
      { ty        = fromMaybe bad_str s_type
      , str       = fromMaybe bad_str s_value
      , exception = exc
      }

-- | Throw python error as haskell exception if it's raised.
checkThrowPyError :: Py ()
checkThrowPyError =
  Py [CU.exp| PyObject* { PyErr_Occurred() } |] >>= \case
    NULL -> pure ()
    _    -> throwM . PyError =<< convertPy2Haskell

-- | Throw python error as haskell exception if it's raised. If it's
--   not that internal error. Another exception will be raised
mustThrowPyError :: Py a
mustThrowPyError =
  Py [CU.exp| PyObject* { PyErr_Occurred() } |] >>= \case
    NULL -> error $ "mustThrowPyError: no python exception raised."
    _    -> throwM . PyError =<< convertPy2Haskell

-- | Calls mustThrowPyError if pointer is null or returns it unchanged
throwOnNULL :: Ptr PyObject -> Py (Ptr PyObject)
throwOnNULL = \case
  NULL -> mustThrowPyError
  p    -> pure p

checkThrowBadPyType :: Py ()
checkThrowBadPyType = do
  r <- Py [CU.block| int {
    if( PyErr_Occurred() ) {
        PyErr_Clear();
        return 1;
    }
    return 0;
    } |]
  case r of
    0 -> pure ()
    _ -> throwM BadPyType


----------------------------------------------------------------
-- Eval/exec
----------------------------------------------------------------

-- | Type class for values representing python dictionaries containing
--   global or local variables.
--
--   @since 0.2@
class Namespace a where
  -- | Returns dictionary object. Caller should take ownership of
  --   returned object.
  basicNamespaceDict :: a -> Py (Ptr PyObject)


-- | Namespace for the top level code execution. It corresponds to
--   @\__dict\__@ field of a @\__main\__@ module.
--
--   @since 0.2@
data Main = Main

instance Namespace Main where
  basicNamespaceDict _ =
    throwOnNULL =<< Py [CU.block| PyObject* {
      PyObject* main_module = PyImport_AddModule("__main__");
      if( PyErr_Occurred() )
          return NULL;
      PyObject* dict = PyModule_GetDict(main_module);
      Py_XINCREF(dict);
      return dict;
      }|]


-- | Temporary namespace which get destroyed after execution
--
--   @since 0.2@
data Temp = Temp

instance Namespace Temp where
  basicNamespaceDict _ = basicNewDict


-- | Newtype wrapper for bare python object. It's assumed to be a
--   dictionary. This is not checked.
--
--   @since 0.2@
newtype DictPtr = DictPtr (Ptr PyObject)

instance Namespace DictPtr where
  basicNamespaceDict (DictPtr p) = p <$ incref p


-- | Newtype wrapper for python dictionary. It's not checked whether
--   object is actually dictionary.
--
--   @since 0.2@
newtype Dict = Dict PyObject

instance Namespace Dict where
  basicNamespaceDict (Dict d)
    -- NOTE: We're incrementing counter inside bracket so we're safe.
    = unsafeWithPyObject d (basicNamespaceDict . DictPtr)

-- | Newtype wrapper over module object.
--
--   @since 0.2@
newtype ModulePtr = ModulePtr (Ptr PyObject)

instance Namespace ModulePtr where
  basicNamespaceDict (ModulePtr p) = do
    throwOnNULL =<< Py [CU.block| PyObject* {
      PyObject* dict = PyModule_GetDict($(PyObject* p));
      Py_XINCREF(dict);
      return dict;
      }|]

-- | Newtype wrapper over module object.
newtype Module = Module PyObject

instance Namespace Module where
  basicNamespaceDict (Module d)
    -- NOTE: We're incrementing counter inside bracket so we're safe.
    = unsafeWithPyObject d (basicNamespaceDict . ModulePtr)


-- | Evaluate python expression. This is wrapper over python's @eval@.
--
--   @since 0.2@
eval :: (Namespace global, Namespace local)
     => global  -- ^ Data type providing global variables dictionary
     -> local   -- ^ Data type providing local variables dictionary
     -> PyQuote -- ^ Source code
     -> Py PyObject
eval globals locals q = runProgram $ do
  p_py      <- unsafeWithCode q.code
  p_globals <- takeOwnership =<< progPy (basicNamespaceDict globals)
  p_locals  <- takeOwnership =<< progPy (basicNamespaceDict locals)
  progPy $ do
    q.binder.bind p_locals
    p_res <- Py [C.block| PyObject* {
      PyObject* globals = $(PyObject* p_globals);
      PyObject* locals  = $(PyObject* p_locals);
      // Compile code
      PyObject *code = Py_CompileString($(char* p_py), "<interactive>", Py_eval_input);
      if( PyErr_Occurred() ) {
          return NULL;
      }
      // Evaluate expression
      PyObject* r = PyEval_EvalCode(code, globals, locals);
      Py_DECREF(code);
      return r;
      }|]
    checkThrowPyError
    newPyObject p_res
{-# SPECIALIZE eval :: Main -> Temp -> PyQuote -> Py PyObject #-}

-- | Evaluate sequence of python statements This is wrapper over python's @exec@.
--
--   @since 0.2@
exec :: (Namespace global, Namespace local)
     => global  -- ^ Data type providing global variables dictionary
     -> local   -- ^ Data type providing local variables dictionary
     -> PyQuote -- ^ Source code
     -> Py ()
exec globals locals q = runProgram $ do
  p_py      <- unsafeWithCode q.code
  p_globals <- takeOwnership =<< progPy (basicNamespaceDict globals)
  p_locals  <- takeOwnership =<< progPy (basicNamespaceDict locals)
  progPy $ do
    q.binder.bind p_locals
    Py[C.block| void {
      PyObject* globals = $(PyObject* p_globals);
      PyObject* locals  = $(PyObject* p_locals);
      // Compile code
      PyObject *code = Py_CompileString($(char* p_py), "<interactive>", Py_file_input);
      if( PyErr_Occurred() ){
          return;
      }
      // Execute statements
      PyObject* res = PyEval_EvalCode(code, globals, locals);
      Py_XDECREF(res);
      Py_DECREF(code);
      } |]
    checkThrowPyError
{-# SPECIALIZE exec :: Main -> Main -> PyQuote -> Py () #-}
{-# SPECIALIZE exec :: Main -> Temp -> PyQuote -> Py () #-}

-- | Obtain pointer to code
unsafeWithCode :: Code -> Program r (Ptr CChar)
unsafeWithCode (Code bs) = Program $ ContT $ \fun ->
  Py (BS.unsafeUseAsCString bs $ unsafeRunPy . fun)


----------------------------------------------------------------
-- Debugging
----------------------------------------------------------------

debugPrintPy :: Ptr PyObject -> Py ()
debugPrintPy p = Py [CU.block| void {
  PyObject_Print($(PyObject *p), stdout, 0);
  printf(" [REF=%li]\n", Py_REFCNT($(PyObject *p)) );
  } |]
