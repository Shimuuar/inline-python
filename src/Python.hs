{-# LANGUAGE CApiFFI                  #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE QuasiQuotes              #-}
{-# LANGUAGE TemplateHaskell          #-}
-- |

-- {-# OPTIONS_GHC -ddump-splices #-}
module Python where

import Foreign.Ptr
import Data.Map.Strict qualified as Map
import Language.C.Inline qualified as C
import Language.C.Inline.Context qualified as C



pyCtx :: C.Context
pyCtx = mempty { C.ctxTypesTable = Map.fromList tytabs } where
  tytabs =
      [ --(TypeName "SEXP", [t| SEXP0 |])
--      , (TypeName "Rcomplex", [t| Complex Double |])
      ]



C.include "<Python.h>"

----------------------------------------------------------------
-- Interpreter initialization
----------------------------------------------------------------

sizeof_PyConfig :: Int
sizeof_PyConfig = fromIntegral [C.pure| int { sizeof(PyConfig) } |]


py_new_PyConfig :: IO (Ptr ())
py_new_PyConfig = [C.block|
  void* {
    PyConfig* cfg = malloc(sizeof(PyConfig));
    PyConfig_InitPythonConfig(cfg);
    return cfg;
  }|]
  
py_InitializeFromConfig :: Ptr () -> IO C.CInt
py_InitializeFromConfig cfg =
  [C.block| int {
          PyStatus status;
          status = Py_InitializeFromConfig($(void * cfg));
          return PyStatus_Exception(status);
          } |]


py_Foo :: IO ()
py_Foo =
  [C.block| void {
          PyRun_SimpleString("from time import time,ctime\n"
                             "print('Today is', ctime(time()))\n");
                             }
                             |]
