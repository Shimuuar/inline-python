{-# LANGUAGE QuasiQuotes              #-}
{-# LANGUAGE TemplateHaskell          #-}
-- |
-- Initialization and deinitialization of python interpreter
module Python.Internal.Init
  ( initializePython
  , finalizePython
  , withPython
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Cont
import Control.Monad.IO.Class
import Foreign.Marshal
import Foreign.C.Types
import System.Environment

import Language.C.Inline        qualified as C
import Language.C.Inline.Unsafe qualified as CU

import Python.Internal.Types
import Python.Internal.Util
import Python.Internal.Eval

----------------------------------------------------------------
C.context (C.baseCtx <> pyCtx)
C.include "<inline-python.h>"
----------------------------------------------------------------


-- | Initialize python interpreter
initializePython :: IO ()
initializePython
  | rtsSupportsBoundThreads = tryReadMVar pythonInterpreter >>= \case
      Just _  -> pure ()
      Nothing -> putMVar pythonInterpreter =<< forkOS pythonThread
  | otherwise = doInializePython


finalizePython :: IO ()
finalizePython
  | rtsSupportsBoundThreads = tryReadMVar pythonInterpreter >>= \case
      Just pid -> killThread pid
      Nothing  -> pure ()
  | otherwise = doFinalizePython

withPython :: IO a -> IO a
withPython = bracket_ initializePython finalizePython

----------------------------------------------------------------
-- Worker functions

-- Main thread which does all python evaluation
pythonThread :: IO ()
pythonThread = doInializePython >> forever evalReq `finally` doFinalizePython

evalReq :: IO ()
evalReq = do
  PyEvalReq{prog=Py io, retval, status} <- takeMVar toPythonThread
  do_eval <- modifyMVar status $ \case
    Running   -> error "Python evaluator: Internal error. Got 'Running' request"
    Done      -> error "Python evaluator: Internal error. Got 'Done' request"
    Cancelled -> return (Cancelled,False)
    Pending   -> return (Running,  True)
  when do_eval $ do
    a <- (Right <$> io) `catches`
         [ Handler $ \(e :: AsyncException) -> throwIO e
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
    putMVar retval a


doInializePython :: IO ()
doInializePython = do
  -- NOTE: I'd like more direct access to argv
  argv0 <- getProgName
  argv  <- getArgs
  let n_argv = fromIntegral $ length argv + 1
  -- FIXME: For some reason sys.argv is initialized incorrectly. No
  --        easy way to debug. Will do for now
  r <- evalContT $ do
    p_argv0  <- ContT $ withWCtring argv0
    p_argv   <- traverse (ContT . withWCtring) argv
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
      //--
      status = PyConfig_SetBytesString(&cfg, &cfg.program_name, "XX");
      if (PyStatus_Exception(status)) { goto error; }
      //--
      status = PyConfig_SetArgv(&cfg,
          $(int       n_argv),
          $(wchar_t** ptr_argv)
      );
      if( PyStatus_Exception(status) ) { goto error; };
      // Initialize interpreter
      status = Py_InitializeFromConfig(&cfg);
      PyConfig_Clear(&cfg);
      return PyStatus_Exception(status);
      // Error case
      error:
      PyConfig_Clear(&cfg);
      return 1;
      } |]
  case r of 0 -> pure ()
            _ -> error "Failed to initialize interpreter"


doFinalizePython :: IO ()
doFinalizePython = [C.exp| void { Py_Finalize(); } |]
