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
  , unPy
    -- * GC-related
  , newPyObject
    -- * C-API wrappers
  , decref
  , incref
  , takeOwnership
  , ensureGIL
  , dropGIL
    -- * Exceptions
  , convertHaskell2Py
  , convertPy2Haskell
  , checkThrowPyError
  , mustThrowPyError
  , checkThrowBadPyType
    -- * Debugging
  , debugPrintPy
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception         (AsyncException(..),SomeAsyncException)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Cont
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
  LockUninialized -> error "Python is not started"
  LockFinalized   -> error "Python is already stopped"
  LockedByGC      -> retry
  LockUnlocked    -> writeTVar globalPyLock $ Locked tid []
  Locked t xs
    | t == tid  -> writeTVar globalPyLock $ Locked t (t : xs)
    | otherwise -> retry

grabLock :: ThreadId -> STM ()
grabLock tid = readTVar globalPyLock >>= \case
  LockUninialized -> error "Python is not started"
  LockFinalized   -> error "Python is already stopped"
  LockedByGC      -> retry
  LockUnlocked    -> writeTVar globalPyLock $ Locked tid []
  Locked t xs     -> writeTVar globalPyLock $ Locked tid (t : xs)

releaseLock :: ThreadId -> STM ()
releaseLock tid = readTVar globalPyLock >>= \case
  LockUninialized -> error "Python is not started"
  LockFinalized   -> error "Python is already stopped"
  LockUnlocked    -> error "INTERNAL ERROR releasing unlocked"
  LockedByGC      -> error "INTERNAL ERROR lock held by GC"
  Locked t xs
    | t /= tid  -> error "INTERNAL ERROR releasing wrong lock"
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
  0 | rtsSupportsBoundThreads -> runInBoundThread $ mask_ $ doInializePython
    | otherwise               -> mask_ $ doInializePython
  _ -> pure ()

-- | Destroy python interpreter.
finalizePython :: IO ()
-- See NOTE: [Python and threading]
finalizePython
  | rtsSupportsBoundThreads = runInBoundThread $ mask_ doFinalizePython
  | otherwise               = mask_ $ doFinalizePython

-- | Bracket which ensures that action is executed with properly
--   initialized interpreter
withPython :: IO a -> IO a
withPython = bracket_ initializePython finalizePython


doInializePython :: IO ()
doInializePython = do
  -- First we need to grab global python lock on haskell side
  join $ atomically $ do
    readTVar globalPyState >>= \case
      Finalized        -> error "Python was already finalized"
      InitFailed       -> error "Python was unable to initialize"
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
                tid_main  <- forkOS $ do
                  r <- doInializePythonIO
                  putMVar lock_init r
                  case r of
                    False -> pure ()
                    True  -> mask_ $ do
                      let loop = takeMVar lock_eval >>= \case
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
                      loop
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

doFinalizePython :: IO ()
doFinalizePython = join $ atomically $ readTVar globalPyState >>= \case
  NotInitialized   -> error "Python is not initialized"
  InitFailed       -> error "Python failed to initialize"
  Finalized        -> pure $ pure ()
  InInitialization -> retry
  InFinalization   -> retry
  -- We can simply call Py_Finalize
  Running1 -> readTVar globalPyLock >>= \case
    LockUninialized -> error "Internal error: Lock not initialized"
    LockFinalized   -> error "Internal error: Lock is already finalized"
    Locked{}        -> retry
    LockedByGC      -> retry
    LockUnlocked    -> do
      writeTVar globalPyLock  LockFinalized
      writeTVar globalPyState Finalized
      pure $ [C.block| void {
        PyGILState_Ensure();
        Py_Finalize();
        } |]
  -- We need to call Py_Finalize on main thread specifically
  RunningN _ eval tid_main tid_gc -> readTVar globalPyLock >>= \case
    LockUninialized -> error "Internal error: Lock not initialized"
    LockFinalized   -> error "Internal error: Lock is already finalized"
    Locked{}        -> retry
    LockedByGC      -> retry
    LockUnlocked    -> do
      writeTVar globalPyLock  LockFinalized
      writeTVar globalPyState Finalized
      pure $ do
        resp <- newEmptyMVar
        putMVar eval $ StopReq resp
        takeMVar resp
        killThread tid_gc


----------------------------------------------------------------
-- Running Py monad
----------------------------------------------------------------

data EvalReq
  = forall a. EvalReq (Py a) (MVar (Either SomeException a))
  | StopReq (MVar ())

-- | Execute python action. It will take global lock and no other
--   python action could start execution until one currently running
--   finished execution normally or with exception.
runPy :: Py a -> IO a
-- See NOTE: [Python and threading]
runPy py
  | rtsSupportsBoundThreads = runInBoundThread go -- Multithreaded RTS
  | otherwise               = go                  -- Single-threaded RTS
  where
    -- We check whether interpreter is initialized. Throw exception if
    -- it wasn't. Better than segfault isn't it?
    go = ensurePyLock $ unPy (ensureGIL py)

runPyInMain :: Py a -> IO a
-- See NOTE: [Python and threading]
runPyInMain py
  -- Multithreaded RTS
  | rtsSupportsBoundThreads = join $ atomically $ readTVar globalPyState >>= \case
      NotInitialized   -> error "Python is not initialized"
      InitFailed       -> error "Python failed to initialize"
      Finalized        -> error "Python is already finalized"
      InInitialization -> retry
      InFinalization   -> retry
      Running1         -> error "INTERNAL ERROR"
      RunningN _ eval tid_main _ -> do
        acquireLock tid_main
        pure $
          (( do resp <- newEmptyMVar
                putMVar eval $ EvalReq py resp
                either throwM pure =<< takeMVar resp
           ) `onException` throwTo tid_main UserInterrupt
          ) `finally` atomically (releaseLock tid_main)
  --     resp <- newEmptyMVar
  -- Single-threaded RTS
  | otherwise = runPy py

-- | Execute python action. This function is unsafe and should be only
--   called in thread of interpreter.
unPy :: Py a -> IO a
unPy (Py io) = io



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

decref :: Ptr PyObject -> Py ()
decref p = Py [CU.exp| void { Py_DECREF($(PyObject* p)) } |]

incref :: Ptr PyObject -> Py ()
incref p = Py [CU.exp| void { Py_INCREF($(PyObject* p)) } |]

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
  Py $ action `finally` [CU.exp| void { PyEval_RestoreThread($(PyThreadState *st)) } |]

-- | Decrement reference counter at end of ContT block
takeOwnership :: Ptr PyObject -> Program r (Ptr PyObject)
takeOwnership p = ContT $ \c -> c p `finally` decref p


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
  p_errors <- withPyAllocaArray @(Ptr PyObject) 3
  p_len    <- withPyAlloca      @CLong
  -- Fetch error indicator
  (p_type, p_value) <- liftIO $ do
    [CU.block| void {
       PyObject **p = $(PyObject** p_errors);
       PyErr_Fetch(p, p+1, p+2);
       }|]
    p_type  <- peekElemOff p_errors 0
    p_value <- peekElemOff p_errors 1
    -- Traceback is not used ATM
    pure (p_type,p_value)
  -- Convert exception type and value to strings.
  let pythonStr p = do
        p_str <- liftIO [CU.block| PyObject* {
          PyObject *s = PyObject_Str($(PyObject *p));
          if( PyErr_Occurred() ) {
              PyErr_Clear();
          }
          return s;
          } |]
        case p_str of
          NULL -> abort UncovertablePyError
          _    -> pure p_str
  s_type  <- takeOwnership =<< pythonStr p_type
  s_value <- takeOwnership =<< pythonStr p_value
  -- Convert to haskell strings
  let toString p = do
        c_str <- [CU.block| const char* {
          const char* s = PyUnicode_AsUTF8AndSize($(PyObject *p), $(long *p_len));
          if( PyErr_Occurred() ) {
              PyErr_Clear();
          }
          return s;
          } |]
        case c_str of
          NULL -> pure ""
          _    -> peekCString c_str
  liftIO $ PyError <$> toString s_type <*> toString s_value


-- | Throw python error as haskell exception if it's raised.
checkThrowPyError :: Py ()
checkThrowPyError =
  Py [CU.exp| PyObject* { PyErr_Occurred() } |] >>= \case
    NULL -> pure ()
    _    -> throwM =<< convertPy2Haskell

-- | Throw python error as haskell exception if it's raised. If it's
--   not that internal error. Another exception will be raised
mustThrowPyError :: String -> Py a
mustThrowPyError msg =
  Py [CU.exp| PyObject* { PyErr_Occurred() } |] >>= \case
    NULL -> error $ "mustThrowPyError: no python exception raised. " ++ msg
    _    -> throwM =<< convertPy2Haskell

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
-- Debugging
----------------------------------------------------------------

debugPrintPy :: Ptr PyObject -> Py ()
debugPrintPy p = Py [CU.block| void {
  PyObject_Print($(PyObject *p), stdout, 0);
  printf(" [REF=%li]\n", Py_REFCNT($(PyObject *p)) );
  } |]
