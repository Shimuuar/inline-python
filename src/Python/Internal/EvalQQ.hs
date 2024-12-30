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
    -- * Python transformations
  , unindent
  ) where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Data.Char
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.Exit
import System.Process (readProcessWithExitCode)

import Language.C.Inline          qualified as C
import Language.Haskell.TH.Lib    qualified as TH
import Language.Haskell.TH.Syntax qualified as TH

import Python.Types
import Python.Internal.Types
import Python.Internal.Util
import Python.Internal.Eval
import Python.Inline.Literal
import Paths_inline_python (getDataFileName)

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
  p_err <- withPyAlloca @(Ptr CChar)
  r     <- liftIO [C.block| int {
    PyObject *e_type, *e_value, *e_trace;
    // Compile code
    PyObject *code = Py_CompileString($(char* p_py), "<interactive>", Py_file_input);
    if( code == 0 ){
        PyErr_Fetch( &e_type, &e_value, &e_trace);
        inline_py_export_exception(e_type, e_value, e_trace, $(char** p_err));
        return INLINE_PY_ERR_COMPILE;
    }
    // Execute in context of main
    PyObject* globals = $(PyObject* p_globals);
    PyObject* locals  = $(PyObject* p_locals);
    PyObject* r = PyEval_EvalCode(code, globals, locals);
    Py_XDECREF(r);
    if( PyErr_Occurred() ) {
        PyErr_Fetch( &e_type, &e_value, &e_trace);
        inline_py_export_exception(e_type, e_value, e_trace, $(char** p_err));
        return INLINE_PY_ERR_EVAL;
    }
    return INLINE_PY_OK;
    } |]
  lift $ finiEval p_err r (pure ())

-- | Evaluate expression with fresh local environment
pyEvalExpr
  :: Ptr PyObject -- ^ Dictionary with local
  -> String       -- ^ Python source code
  -> Py PyObject
pyEvalExpr p_env src = evalContT $ do
  p_py  <- withPyCString src
  p_err <- withPyAlloca @(Ptr CChar)
  p_res <- withPyAlloca @(Ptr PyObject)
  r <- liftIO
    [C.block| int {
       PyObject *e_type, *e_value, *e_trace;
       // Compile code
       PyObject *code = Py_CompileString($(char* p_py), "<interactive>", Py_eval_input);
       if( code == 0 ){
           PyErr_Fetch( &e_type, &e_value, &e_trace);
           inline_py_export_exception(e_type, e_value, e_trace, $(char** p_err));
           return INLINE_PY_ERR_COMPILE;
       }
       // Execute in context of main
       PyObject* main_module = PyImport_AddModule("__main__");
       PyObject* globals     = PyModule_GetDict(main_module);
       //
       PyObject* r = PyEval_EvalCode(code, globals, $(PyObject* p_env));
       *$(PyObject** p_res) = r;
       if( PyErr_Occurred() ) {
           PyErr_Fetch( &e_type, &e_value, &e_trace);
           inline_py_export_exception(e_type, e_value, e_trace, $(char** p_err));
           return INLINE_PY_ERR_EVAL;
       }
       Py_INCREF(r);
       return INLINE_PY_OK;
       }|]
  lift $ finiEval p_err r (newPyObject =<< liftIO (peek p_res))

-- | Convert evaluation result and
finiEval
  :: Ptr CString
  -> CInt
  -> Py a
  -> Py a
finiEval p_err r fini = case r of
  INLINE_PY_OK          -> fini
  INLINE_PY_ERR_COMPILE -> Py $ peek p_err >>= \case
    p | nullPtr == p -> throwIO $ PyError "Compile error"
      | otherwise    -> do
          s <- peekCString p
          free p
          throwIO $ PyError s
  INLINE_PY_ERR_EVAL   -> Py $ peek p_err >>= \case
      p | nullPtr == p -> throwIO $ PyError "Evaluation error"
        | otherwise    -> do
            s <- peekCString p
            free p
            throwIO $ PyError s
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
basicNewDict = Py [C.exp| PyObject* { PyDict_New() } |]

-- | Return dict of @__main__@ module
basicMainDict :: Py (Ptr PyObject)
basicMainDict = Py [C.block| PyObject* {
  PyObject* main_module = PyImport_AddModule("__main__");
  return PyModule_GetDict(main_module);
  }|]


----------------------------------------------------------------
-- TH generator
----------------------------------------------------------------


-- | Generate TH splice which updates python environment dictionary
--   and returns python source code.
expQQ :: String -- ^ Python evaluation mode: @exec@/@eval@
      -> String -- ^ Python source code
      -> TH.Q TH.Exp
expQQ mode src = do
  script <- liftIO $ getDataFileName "py/bound-vars.py"
  antis  <- liftIO $ do
    (code, stdout, stderr) <- readProcessWithExitCode "python" [script, mode] src
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

