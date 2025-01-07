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
    -- * Locks
  , ensurePyLock
  , grabPyLock
    -- * Initialization
  , initializePython
  , finalizePython
  , withPython
    -- * PyObject wrapper
  , newPyObject
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
-- Python (cpython to be precise) support threading to some extent and
-- it interacts with haskell threading in interesting and generally
-- unpleasant ways. So python's threads are:
--
--  1. They're OS threads. Python is designed to be embeddable and can
--     live with threads scheduled by outside entity.
--
--  2. Any OS thread interacting with python interpreter must hold
--     global interpreter lock (GIL)
--
--  3. GIL uses thread local state.
--
-- Haskell has two runtimes. Single threaded one doesn't cause any
-- troubles and won't be discussed further. Multithreaded one
-- implement N-M threading and schedules N green thread over M OS
-- threads as it see fit.
--
-- One could think that running python code in bound threads and
-- making sure that GIL is held would suffice. It doesn't.
-- RTS m

--

-- This means python must run in bound threads. Or in case of
-- single-threaded RTS we could just make safe FFI calls. There's only
-- one thread anyway.
--
-- In order to track code with such requirements `Py` monad (just
-- newtype over `IO`). All code interacting with Python must live in
-- it. In addition it should run with async exceptions masked since
-- writing code which works with bare pointers AND safe in presence of
-- async exceptions is almost impossible.
--
--
-- And this is not end of our problems with threading. Python
-- designate thread in which it was initialized as a main thread. It
-- has special status but if don't take precautions we don't know
-- which haskell thread it is.



-- NOTE: [GC]
-- ~~~~~~~~~~
--
-- CPython uses reference counting which works very well with
-- ForeignPtr. But there's a catch: decrementing counter is only
-- possible if one holds GIL. And one could not touch GIL if
-- interpreter is not initialized or being finalized.
--
-- We do not need to care whether thread is bound or not since this is
-- single C call which will not getting migrated.
--
-- Still it's a question whether it's OK to call blocking code in
-- ForeignPtr's finalizers.



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
-- Execution of Py monad
----------------------------------------------------------------

data PyState
  = NotInitialized
  | InInitialization
  | InitFailed
  | Running !(Chan (Ptr PyObject)) !ThreadId
  | InFinalization
  | Finalized

data PyLock
  = LockUninialized
  | LockUnlocked
  | Locked !ThreadId [ThreadId]
  | LockedByGC
  | LockFinalized

globalPyState :: TVar PyState
globalPyState = unsafePerformIO $ newTVarIO NotInitialized
{-# NOINLINE globalPyState #-}

globalPyLock :: TVar PyLock
globalPyLock = unsafePerformIO $ newTVarIO LockUninialized
{-# NOINLINE globalPyLock #-}

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

ensurePyLock :: IO a -> IO a
ensurePyLock action = do
  tid <- myThreadId
  bracket_ (atomically $ acquireLock tid)
           (atomically $ releaseLock tid)
           action

grabPyLock :: IO a -> IO a
grabPyLock action = do
  tid <- myThreadId
  bracket_ (atomically $ grabLock tid)
           (atomically $ releaseLock tid)
           action


-- | Execute python action. It will be executed with GIL held and
--   async exceptions masked.
runPy :: Py a -> IO a
-- See NOTE: [Python and threading]
runPy py
  | rtsSupportsBoundThreads = runInBoundThread go -- Multithreaded RTS
  | otherwise               = go                  -- Single-threaded RTS
  where
    -- We check whether interpreter is initialized. Throw exception if
    -- it wasn't. Better than segfault isn't it?
    go = mask_ $ isInitialized >>= \case
      True  -> ensurePyLock $ unPy (ensureGIL py)
      False -> error "Python is not initialized"

-- | Execute python action. This function is unsafe and should be only
--   called in thread of interpreter.
unPy :: Py a -> IO a
unPy (Py io) = io


isInitialized :: IO Bool
isInitialized = do
  i <- [CU.exp| int { !Py_IsFinalizing() && Py_IsInitialized() } |]
  pure $! i /= 0



----------------------------------------------------------------
-- Initialization of interpreter
----------------------------------------------------------------

-- | Initialize python interpreter. If interpreter is already
--   initialized it's a noop.
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
      Running{}        -> pure $ pure ()
      NotInitialized   -> do
        writeTVar globalPyState InInitialization
        pure $
          (do doInializePythonIO
              gc_chan <- newChan
              gc_tid  <- if
                | rtsSupportsBoundThreads -> forkOS $ gcThread gc_chan
                | otherwise               -> forkIO $ gcThread gc_chan
              atomically $ do
                writeTVar globalPyState $ Running gc_chan gc_tid
                writeTVar globalPyLock  $ LockUnlocked
          ) `onException` atomically (writeTVar globalPyState InitFailed)

doInializePythonIO :: IO ()
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
  case r of 0 -> pure ()
            _ -> error "Failed to initialize interpreter"

doFinalizePython :: IO ()
doFinalizePython = join $ atomically $ readTVar globalPyState >>= \case
  NotInitialized   -> error "Python is not initialized"
  InitFailed       -> error "Python failed to initialize"
  Finalized        -> pure $ pure ()
  InInitialization -> retry
  InFinalization   -> retry
  Running _ gc_tid -> do
    readTVar globalPyLock >>= \case
      LockUninialized -> error "Internal error: Lock not initialized"
      LockFinalized   -> error "Internal error: Lock is already finalized"
      Locked{}        -> retry
      LockedByGC      -> retry
      LockUnlocked    -> do
        writeTVar globalPyLock  LockFinalized
        writeTVar globalPyState Finalized
        pure $ do
          killThread gc_tid
          [C.block| void {
            PyGILState_Ensure();
            Py_Finalize();
            } |]

----------------------------------------------------------------
-- Creation of PyObject
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


-- | Wrap raw python object into
newPyObject :: Ptr PyObject -> Py PyObject
-- See NOTE: [GC]
newPyObject p = Py $ do
  fptr <- newForeignPtr_ p
  GHC.addForeignPtrFinalizer fptr $
    readTVarIO globalPyState >>= \case
      Running ch _ -> writeChan ch p
      _            -> pure ()
  pure $ PyObject fptr

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
      [CU.block| void {
        PyGILState_STATE st = PyGILState_Ensure();
        Py_XDECREF( $(PyObject* p) );
        PyGILState_Release(st);
        } |]
      atomically $ writeTVar globalPyLock LockUnlocked

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
