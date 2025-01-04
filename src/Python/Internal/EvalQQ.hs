{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
module Python.Internal.EvalQQ
  ( -- * Evaluators and QQ
    pyEvalInMain
  , pyEvalExpr
  , expQQ
  , basicNewDict
  , basicMainDict
  , basicBindInDict
  , basicDecref
    -- * Python transformations
  , unindent
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Data.Bits
import Data.Char
import Data.ByteString           qualified as BS
import Data.Text                 qualified as T
import Data.Text.Encoding        qualified as T
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import System.Exit
import System.Process (readProcessWithExitCode)

import Language.C.Inline          qualified as C
import Language.C.Inline.Unsafe   qualified as CU
import Language.Haskell.TH.Lib    qualified as TH
import Language.Haskell.TH.Syntax qualified as TH

import Python.Types
import Python.Internal.Types
import Python.Internal.Program
import Python.Internal.Eval
import Python.Inline.Literal


----------------------------------------------------------------
C.context (C.baseCtx <> pyCtx)
C.include "<inline-python.h>"
----------------------------------------------------------------

----------------------------------------------------------------
-- Evaluators
----------------------------------------------------------------

-- | Evaluate expression within context of @__main__@ module. All
--   variables defined in this evaluator persist.
pyEvalInMain
  :: Ptr PyObject -- ^ Globals
  -> Ptr PyObject -- ^ Locals
  -> String
  -> Py ()
pyEvalInMain p_globals p_locals src = evalContT $ do
  p_py  <- withPyCString src
  r     <- liftIO [C.block| int {
    PyObject *code = Py_CompileString($(char* p_py), "<interactive>", Py_file_input);
    if( PyErr_Occurred() ){
        return IPY_ERR_COMPILE;
    }
    // Execute in context of main
    PyObject* globals = $(PyObject* p_globals);
    PyObject* locals  = $(PyObject* p_locals);
    PyObject* r = PyEval_EvalCode(code, globals, locals);
    Py_XDECREF(r);
    if( PyErr_Occurred() ) {
        return IPY_ERR_PYTHON;
    }
    return IPY_OK;
    } |]
  lift $ finiEval r (pure ())

-- | Evaluate expression with fresh local environment
pyEvalExpr
  :: Ptr PyObject -- ^ Dictionary with local
  -> String       -- ^ Python source code
  -> Py PyObject
pyEvalExpr p_env src = evalContT $ do
  p_py  <- withPyCString src
  p_res <- withPyAlloca @(Ptr PyObject)
  r     <- liftIO [C.block| int {
    // Compile code
    PyObject *code = Py_CompileString($(char* p_py), "<interactive>", Py_eval_input);
    if( PyErr_Occurred() ) {
        return IPY_ERR_COMPILE;
    }
    // Execute in context of main
    PyObject* main_module = PyImport_AddModule("__main__");
    if( PyErr_Occurred() ) {
        return IPY_ERR_PYTHON;
    }
    PyObject* globals     = PyModule_GetDict(main_module);
    if( PyErr_Occurred() ) {
        return IPY_ERR_PYTHON;
    }
    //
    PyObject* r = PyEval_EvalCode(code, globals, $(PyObject* p_env));
    if( PyErr_Occurred() ) {
        return IPY_ERR_PYTHON;
    }
    Py_INCREF(r);
    *$(PyObject **p_res) = r;
    return IPY_OK;
    }|]
  lift $ finiEval r (newPyObject =<< liftIO (peek p_res))

-- | Convert evaluation result and
finiEval
  :: CInt
  -> Py a
  -> Py a
finiEval r fini = case r of
  IPY_OK          -> fini
  IPY_ERR_COMPILE -> throwPy =<< convertPy2Haskell
  IPY_ERR_PYTHON  -> throwPy =<< convertPy2Haskell
  _ -> error $ "pyEvalStr: unexpected error: " ++ show r

basicBindInDict :: ToPy a => String -> a -> Ptr PyObject -> Py ()
basicBindInDict name a p_dict = evalContT $ do
  -- FIXME: error handling
  -- FIXME: meanining of errors in PyUnicode_DecodeUTF8?
  (p_key,len) <- withPyCStringLen name
  p_obj       <- lift $ basicToPy a
  let c_len = fromIntegral len :: CLong
  liftIO [C.block| void {
    PyObject* p_obj = $(PyObject* p_obj);
    PyObject* key   = PyUnicode_DecodeUTF8( $(char* p_key), $(long c_len), 0);
    PyDict_SetItem($(PyObject* p_dict), key, p_obj);
    Py_DECREF(p_obj);
    } |]

basicNewDict :: Py (Ptr PyObject)
basicNewDict = Py [CU.exp| PyObject* { PyDict_New() } |]

-- | Return dict of @__main__@ module
basicMainDict :: Py (Ptr PyObject)
basicMainDict = Py [CU.block| PyObject* {
  PyObject* main_module = PyImport_AddModule("__main__");
  return PyModule_GetDict(main_module);
  }|]

basicDecref :: Ptr PyObject -> Py ()
basicDecref o = Py [CU.exp| void { Py_DECREF($(PyObject* o)) } |]


----------------------------------------------------------------
-- TH generator
----------------------------------------------------------------

script :: String
script = $( do let path = "py/bound-vars.py"
               TH.addDependentFile path
               TH.lift =<< TH.runIO (readFile path)
          )

-- | Generate TH splice which updates python environment dictionary
--   and returns python source code.
expQQ :: String -- ^ Python evaluation mode: @exec@/@eval@
      -> String -- ^ Python source code
      -> TH.Q TH.Exp
expQQ mode src = do
  antis  <- liftIO $ do
    (code, stdout, stderr) <- readProcessWithExitCode "python" ["-", mode]
      $ unlines [ script
                , "decode_and_print('" <>
                  concat [ [ intToDigit $ fromIntegral (w `shiftR` 4)
                           , intToDigit $ fromIntegral (w .&. 15) ]
                         | w <- BS.unpack $ T.encodeUtf8 $ T.pack src
                         ]
                  <> "')"
                ]
    case code of
      ExitSuccess   -> pure $ words stdout
      ExitFailure{} -> error stderr
  let args = [ [| basicBindInDict $(TH.lift nm) $(TH.dyn (chop nm)) |]
             | nm <- antis
             ]
  --
  [| \p_dict -> do
        mapM_ ($ p_dict) $(TH.listE args)
        pure $(TH.lift src)
   |]


antiSuffix :: String
antiSuffix = "_hs"

-- | Chop antiquotation variable names to get the corresponding Haskell variable name.
chop :: String -> String
chop name = take (length name - length antiSuffix) name


----------------------------------------------------------------
-- Python source code transform
----------------------------------------------------------------

unindent :: String -> String
unindent py = case ls of
  [] -> ""
  _  -> unlines $ drop n <$> ls
  where
    n  = minimum [ length (takeWhile (==' ') s) | s <- ls ]
    ls = filter (any (not . isSpace)) $ lines py
