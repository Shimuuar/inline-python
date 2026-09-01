{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- |
-- Evaluation of python expressions.
module Python.Internal.Eval
  ( -- * Locks
    ensurePyLock
    -- * Initialization
  , initializePython
  , finalizePython
  , withPython
    -- * Evaluator
  , runPy
  , runPyInMain
  , unsafeRunPy
    -- ** Async
  , PyAsync
  , PyAsyncCancelled(..)
  , waitPy
  , waitPyCatch
  , cancelPy
  , uninterruptibleCancelPy
  , runPyAsync
  , withPyAsync
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
-- threads as GHC RTS sees fit.
--
-- Runtime may migrate haskell threads between OS threads freely so
-- consecutive calls to python may happen in different threads. This
-- doesn't seem to cause problems so far. In similar way several
-- threads may interleave calls to python in single OS
-- thread. Hopefully it won't cause problems either.
--
-- Pre 0.3 version had a global lock allowing only single runPy to
-- execute at any time.
--
-- For uses where serious concurrency is required runPyAsync machinery
-- should be used. See NOTE [Py Async] for details.



-- NOTE: [Main thread]
-- ~~~~~~~~~~~~~~~~~~~
--
-- Also python designate thread in which python interpreter was
-- initialized as a main thread. It has special status for example
-- some libraries may run only in main thread (e.g. tkinter). But if
-- we don't take special precautions we won't know which thread it
-- is.



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
-- Interrupting program that mixes python and haskell code is in fact
-- very difficult.
--
--  + Haskell code cannot receive exceptions while in foreign call.
--
--  + Haskell callback from python created new lightweight
--    thread. Thus we cannot interrupt callback thread since we need
--    to know its thread ID.
--
--  + `Py_SetInterrupt` plain doesn't work. It uses signal which trips
--    up haskell RTS as well.
--
--  + `PyThreadState_SetAsyncExc` uses OS thread id (or something
--    similar) as a key. So we must execute code in a bound thread and
--    be sure that no other haskell thread (except callbacks) uses it.
--
-- Together this means it's only possible to interrupt python when
-- it's called in dedicated OS thread. Such as created by `runPyAsync`
-- or in main thread. To do this we need thread ID as used by python.
--
-- To interrupt haskell, whether thread we spawned or any callback
-- we'll have to maintain stack of thread IDs somehow. Obvoiusly
-- such stack has to be done by callback.
--
-- And even if we do have stack we can't reliably interrupt callbacks
-- due to asynchrony. We may look at stack just before callbacks TID
-- is pushed onto it. In that case we'll try interrupt parent
-- thread. Or we can use its value just before TID is popped. In that
-- case we'll interrupt thread that's about to stop or stopped
-- already.
--
-- So it seems only way of dealing with this problem is to try to kill
-- thread on top of stack and whenever new thread appear on top of
-- stack. 
--
--
-- As for runPy it seems there's simply no way to forcefully interrupt
-- computation. So it's not interruptible.





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
    -- ^ Interpreter is being initialized. This state is required in
    --   case initialization is started from different threads.
  | InitFailed
    -- ^ Initialization was attempted but failed for whatever reason.
  | Running1
    -- ^ Interpreter is running. We're using single threaded RTS
  | RunningN !(Chan (Ptr PyObject))
             !(MVar EvalReq)
             !ThreadId   -- Haskell ID of main thread
             !PyThreadId -- Python ID of main thread
             !ThreadId   -- GC thread ID
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
  | LockReady !(TVar Int) !(TMVar ())
    -- ^ Interpreter is properly initialized and we track number of
    --   threads running python. Same thread may take lock multiple
    --   times: e.g. nested runPy.
    --
    --   Second parameter is mutex for main. We allow only single
    --   request in flight.
  | LockFinalized
    -- ^ Python interpreter shut down. Taking lock is not possible

-- | Execute code ensuring that python lock is held by current thread.
ensurePyLock :: IO a -> IO a
ensurePyLock = bracket_
  (atomically acquireLock)
  (atomically releaseLock)

acquireLock :: STM ()
acquireLock = readTVar globalPyLock >>= \case
  LockUninialized -> throwSTM PythonNotInitialized
  LockFinalized   -> throwSTM PythonIsFinalized
  LockReady n _   -> modifyTVar' n succ

releaseLock :: STM ()
releaseLock = readTVar globalPyLock >>= \case
  LockUninialized -> throwSTM PythonNotInitialized
  LockFinalized   -> throwSTM PythonIsFinalized
  LockReady n _   -> modifyTVar' n pred

ensureInit :: IO ()
ensureInit = readTVarIO globalPyLock >>= \case
  LockUninialized -> throwM PythonNotInitialized
  LockFinalized   -> throwM PythonIsFinalized
  LockReady{}     -> pure ()



----------------------------------------------------------------
-- Initialization and finalization
----------------------------------------------------------------

-- | Initialize python interpreter. If interpreter is already
--   initialized it's a noop. Calling after python was shut down will
--   result in error.
initializePython :: IO ()
-- See NOTE: [Python and threading]
initializePython = [CU.exp| int { Py_IsInitialized() } |] >>= \case
  0 | rtsSupportsBoundThreads -> runInBoundThread $ doInitializePython
    | otherwise               -> doInitializePython
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
  RunningN _ lock_eval _ _ tid_gc -> checkLock $ do
    killThread tid_gc
    resp <- newEmptyMVar
    putMVar lock_eval $ StopReq resp
    takeMVar resp
  where
    checkLock action = readTVar globalPyLock >>= \case
      LockUninialized -> throwSTM $ PyInternalError "finalizePython LockUninialized"
      LockFinalized   -> throwSTM $ PyInternalError "finalizePython LockFinalized"
      LockReady n _   -> readTVar n >>= \case
        0 -> do writeTVar globalPyLock  LockFinalized
                writeTVar globalPyState Finalized
                pure action
        _ -> retry

-- | Bracket which ensures that action is executed with properly
--   initialized interpreter
withPython :: IO a -> IO a
withPython = bracket_ initializePython finalizePython


doInitializePython :: IO ()
doInitializePython = do
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
              n         <- newTVar 0
              main_lock <- newTMVar ()
              writeTVar globalPyState $ st
              writeTVar globalPyLock  $ LockReady n main_lock
        pure $
          (mask_ $ if
            -- On multithreaded runtime create bound thread to make
            -- sure we can call python in its main thread.
            | rtsSupportsBoundThreads -> do
                lock_init <- newEmptyMVar
                lock_eval <- newEmptyMVar
                -- Main thread
                tid_main <- forkOS $ mainThread lock_init lock_eval
                tid_py   <- takeMVar lock_init >>= \case
                  Just tid -> pure tid
                  Nothing  -> throwM PyInitializationFailed
                -- GC thread
                gc_chan <- newChan
                tid_gc  <- forkOS $ gcThread gc_chan
                fini $ RunningN gc_chan lock_eval tid_main tid_py tid_gc
            -- Nothing special is needed on single threaded RTS
            | otherwise -> do
                doInitializePythonIO >>= \case
                  True  -> pure ()
                  False -> throwM PyInitializationFailed
                fini Running1
          ) `onException` atomically (writeTVar globalPyState InitFailed)

-- This action is executed on python's main thread
mainThread :: MVar (Maybe PyThreadId) -> MVar EvalReq -> IO ()
mainThread lock_init lock_eval = do
  doInitializePythonIO >>= \case
    False -> putMVar lock_init Nothing
    True  -> do
      putMVar lock_init . Just =<< getPyThreadID
      mask_ $ fix $ \loop ->
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


doInitializePythonIO :: IO Bool
doInitializePythonIO = do
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
      // This is hack for python<=3.11.
      //
      // Somehow we may end up in stet where thread ID of main thread is not equal
      // to main thread's one. Importing threading seems to fix it. However exact
      // reason for such behavior is unknown
      if( PY_MINOR_VERSION <= 11 ) {
          PyObject *threading = PyImport_ImportModule("threading");
          if( PyErr_Occurred() ) {
              PyErr_Clear();
          }
      }
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
      py_tid <- getPyThreadID
      bracket (acquireMain py_tid) fst snd
  -- Single-threaded RTS
  | otherwise = runPy py
  where
    acquireMain py_tid = atomically $ readTVar globalPyState >>= \case
      NotInitialized   -> throwSTM PythonNotInitialized
      InitFailed       -> throwSTM PyInitializationFailed
      Finalized        -> throwSTM PythonIsFinalized
      InInitialization -> retry
      InFinalization   -> retry
      Running1         -> throwSTM $ PyInternalError "runPyInMain: Running1"
      RunningN _ eval_lock tid_main tid_main_py _ -> readTVar globalPyLock >>= \case
        LockUninialized -> throwSTM PythonNotInitialized
        LockFinalized   -> throwSTM PythonIsFinalized
        LockReady _ main_lock
          -- We're on main thread. We can just run computation and not
          -- bother with incrementing thread counter. It's already
          -- incremented in outer scope
          | py_tid == tid_main_py -> pure ( pure ()
                                          , unsafeRunPy $ ensureGIL py
                                          )
          -- Otherwise we need to send closure to main thread for evaluation.
          -- We use mutex to make sure that only single request is executed
          | otherwise -> do
              takeTMVar main_lock
              acquireLock
              pure ( atomically (releaseLock >> putTMVar main_lock ())
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
-- Async running
----------------------------------------------------------------

-- NOTE: [Py Async]
-- ~~~~~~~~~~~~~~~~
--
-- Interaction with concurrent python in multithreaded environments
-- stays on rather shaky foundations. I'm not sure that RTS won't
-- schedule regular threads on forkOS'd thread and they won't cause
-- problems there.
--
-- General idea of python asyncs is: we start new thread using forkOS
-- and run python code there and hope that it won't interfere with
-- anything.
--
-- Separate problem is interrupting such threads. There're several
-- constraints which severly limit possible implementations:
--
--  1. Haskell exception cannot be delivered while thread is running
--     python. We're in the middle of foreign call. We need to
--     interrupt python as well.
--
--  2. PyThreadState_SetAsyncExc doesn't queue exception. If python
--     thread isn't running (e.g. released GIL by calling liftIO) it's
--     a noop.
--
--  3. PyThreadState_SetAsyncExc uses OS thread id as key for thread
--     interruption. And haskell runtime can schedule another thread
--     on same OS thread. So we must not to attempt to interrupt
--     thread after it finished.
--
-- So we try to throw both haskell and python exceptions concurrently
-- and add MVar lock to check liveliness of worker thread,


-- | Exception thrown to a thread doing async python computation.
data PyAsyncCancelled = PyAsyncCancelled
  deriving (Show, Eq)

instance Exception PyAsyncCancelled

-- | Handle to asynchronous python computation spawned by
--   'runPyAsync'. It's performed on separate OS thread. Use
--   'wait'\/'waitCatch' to obtain computation result.
data PyAsync a = PyAsync
  { asyncTID   :: !ThreadId        -- Thread ID
  , asyncPyTID :: !(IO PyThreadId) -- Thread ID used by python
  , asyncAlive :: !(MVar Bool)     -- Holds True while thread is alive
  , asyncWait  :: STM (Either SomeException a)
  }

-- | Wait for result of asynchronous computation. If it threw an
--   exception it will be rethrown by @wait@.
waitPy :: PyAsync a -> STM a
waitPy a = either throwSTM pure =<< a.asyncWait

-- | Wait for result of asynchronous computation. Exception thrown by
--   it will be returned as @Left@.
waitPyCatch :: PyAsync a -> STM (Either SomeException a)
waitPyCatch = (.asyncWait)

-- | Create new OS thread and execute python code on it.
runPyAsync :: Py a -> IO (PyAsync a)
runPyAsync py = do
  ensureInit
  result    <- newEmptyTMVarIO
  py_tid_mv <- newEmptyMVar
  alive     <- newMVar True
  -- Worker thread. We must modify liveliness MVar under
  -- uninterruptibleMask otherwise it could be interrupted and
  -- cancelPy will consider thread alive forever
  tid    <- forkOS $ mask_ $
    (do putMVar py_tid_mv =<< getPyThreadID
        a <- try $ ensurePyLock $ unsafeRunPy $ ensureGIL py
        atomically $ putTMVar result a
    ) `finally` uninterruptibleMask_ (modifyMVar_ alive (\_ -> pure False))
  pure PyAsync
    { asyncTID   = tid
    , asyncPyTID = readMVar py_tid_mv
    , asyncWait  = takeTMVar result
    , asyncAlive = alive
    }


-- | Cancel execution of asynchronous computation. Most likely thread
--   will be executing some python so first it attempts to raise async
--   exception in python code. Then it throws 'PyAsyncCancelled' in case
--   it executes haskell code. This means thread could be terminate
--   either with 'PyError' or 'PyAsyncCancelled'.
--
--   Note that python code generally is not written under assumption
--   that it could be smitten with exception at an absolutely any
--   moment.
cancelPy :: PyAsync a -> IO ()
cancelPy PyAsync{asyncTID=tid, asyncPyTID, asyncAlive} = do
  -- See NOTE: [Py Async]
  PyThreadId py_tid <- asyncPyTID
  -- Interrupting python
  _ <- forkIO $ fix $ \loop -> do
    -- Attempt to interrupt python. Only if thread is still alive
    n <- withMVar asyncAlive $ \case
      False -> return 1
      True  -> [C.block| int {
        int gil = PyGILState_Ensure();
        int n   = PyThreadState_SetAsyncExc($(uint64_t py_tid), inline_py_AsyncError());
        PyGILState_Release(gil);
        return n;
        }|]
    case n of
      0 -> do
        threadDelay 50 -- Avoid hammering interrupt too hard
        loop
      _ -> return ()
  -- Interrupt haskell
  throwTo tid PyAsyncCancelled


-- | Variant of 'cancel' which isn't interruptible.
uninterruptibleCancelPy :: PyAsync a -> IO ()
uninterruptibleCancelPy = uninterruptibleMask_ . cancelPy

-- | Create new OS thread and execute python code on it. Will use
--   'uninterruptibleCancel' after callback finishes execution.
withPyAsync :: Py a -> (PyAsync a -> IO b) -> IO b
withPyAsync py = bracket (runPyAsync py) uninterruptibleCancelPy


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
      RunningN ch _ _ _ _  -> writeChan ch p
      Running1             -> singleThreadedDecrefCG p
      _                    -> pure ()
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
  LockReady n _   -> do
    modifyTVar' n succ
    pure $ do
      gcDecref p `finally` atomically (modifyTVar' n pred)

singleThreadedDecrefCG :: Ptr PyObject -> IO ()
singleThreadedDecrefCG p = readTVarIO globalPyLock >>= \case
  LockUninialized -> pure ()
  LockFinalized   -> pure ()
  LockReady{}     -> gcDecref p

gcDecref :: Ptr PyObject -> IO ()
gcDecref p = [C.block| void {
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
  gil_state <- Py [C.exp| int { PyGILState_Ensure() } |]
  action `finally` Py [CU.exp| void { PyGILState_Release($(int gil_state)) } |]

-- | Drop GIL temporarily
dropGIL :: IO a -> Py a
dropGIL action = do
  -- NOTE: We're cheating here and looking behind the veil.
  --       PyGILState_STATE is defined as enum. Let hope it will stay
  --       this way.
  st <- Py [CU.exp| PyThreadState* { PyEval_SaveThread() } |]
  Py $ interruptible action
        `finally` [C.exp| void { PyEval_RestoreThread($(PyThreadState *st)) } |]


-- | Removes exception masking and releases GIL temporarily
instance MonadIO Py where
  liftIO = dropGIL . interruptible

getPyThreadID :: IO PyThreadId
getPyThreadID = PyThreadId <$> [CU.exp| uint64_t { PyThread_get_thread_ident() } |]

----------------------------------------------------------------
-- Conversion of exceptions
----------------------------------------------------------------

-- | Convert haskell exception to python exception. Always returns
--   NULL.
convertHaskell2Py :: SomeException -> Py (Ptr PyObject)
convertHaskell2Py err = Py $ do
  withCString ("Haskell exception: "++show err) $ \p_err -> do
    [C.block| PyObject* {
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
    -- NOTE: When we set exception using PyThreadState_SetAsyncExc
    --       this field remains NULL on python<=3.11. In this case we
    --       assume it's our AsyncError:
    p_value <- peekElemOff p_errors 1 >>= \case
      NULL -> [CU.block| PyObject* {
        PyObject *err_class = inline_py_AsyncError();
        PyObject *tuple     = PyTuple_New(0);
        PyObject *err       = PyObject_Call(err_class, tuple, NULL);
        Py_DECREF(tuple);
        return err;
        } |]
      p    -> pure p
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
    throwOnNULL =<< Py [C.block| PyObject* {
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
    throwOnNULL =<< Py [C.block| PyObject* {
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
debugPrintPy p = Py [C.block| void {
  PyObject_Print($(PyObject *p), stdout, 0);
  printf(" [REF=%li]\n", Py_REFCNT($(PyObject *p)) );
  } |]
