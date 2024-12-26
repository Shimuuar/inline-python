{-# LANGUAGE CApiFFI                  #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE QuasiQuotes              #-}
{-# LANGUAGE TemplateHaskell          #-}
-- |

-- {-# OPTIONS_GHC -ddump-splices #-}
module Python where

import Control.Exception
import Control.Monad.Trans.Cont
import Control.Monad.IO.Class
import Data.Char
import Data.Map.Strict qualified as Map
import Foreign.Ptr
import Foreign.Marshal
import Foreign.C.String
import Foreign.C.Types
import System.Environment

import Language.C.Inline         qualified as C
import Language.C.Inline.Context qualified as C



pyCtx :: C.Context
pyCtx = mempty { C.ctxTypesTable = Map.fromList tytabs } where
  tytabs =
      [ --(TypeName "SEXP", [t| SEXP0 |])
--      , (TypeName "Rcomplex", [t| Complex Double |])
      ]

C.include "<inline-python.h>"

----------------------------------------------------------------
-- Interpreter initialization
----------------------------------------------------------------

-- | Initialize python interpreter
initializePython :: IO ()
initializePython = do
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


finalizePython :: IO ()
finalizePython = [C.exp| void { Py_Finalize(); } |]

withPython :: IO a -> IO a
withPython = bracket_ initializePython finalizePython


py_Foo :: IO ()
py_Foo =
  [C.block| void {
          PyRun_SimpleString("import sys\n"
                             "print(sys.argv)\n");
                             }
                             |]
    
 
withWCtring :: String -> (Ptr CWchar -> IO a) -> IO a
withWCtring = withArray0 (CWchar 0) . map (fromIntegral . ord)
