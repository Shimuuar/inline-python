{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
module Python.Internal.EvalQQ
  ( -- * Evaluators and QQ
    pyExecExpr
  , pyEvalExpr
  , expQQ
  , Mode(..)
  , basicNewDict
  , basicMainDict
  , basicBindInDict
  , getFunctionObject
  , callFunctionObject
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Data.Bits
import Data.Char
import Data.List                 (intercalate)
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
pyExecExpr
  :: Ptr PyObject -- ^ Globals
  -> Ptr PyObject -- ^ Locals
  -> String       -- ^ Python source code
  -> Py ()
pyExecExpr p_globals p_locals src = evalContT $ do
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

getFunctionObject :: Ptr PyObject -> Py (Ptr PyObject)
getFunctionObject p_dict = do
  Py [CU.exp| PyObject* { PyDict_GetItemString($(PyObject *p_dict), "_inline_python_") } |]

callFunctionObject :: Ptr PyObject -> Ptr PyObject -> Py (Ptr PyObject)
callFunctionObject fun kwargs = Py [CU.block| PyObject* {
  PyObject* args = PyTuple_Pack(0);
  return PyObject_Call($(PyObject *fun), args, $(PyObject *kwargs));
  } |]



----------------------------------------------------------------
-- TH generator
----------------------------------------------------------------

script :: String
script = $( do let path = "py/bound-vars.py"
               TH.addDependentFile path
               TH.lift =<< TH.runIO (readFile path)
          )

data Mode
  = Eval
  | Exec
  | Fun

-- | Generate TH splice which updates python environment dictionary
--   and returns python source code.
expQQ :: Mode   -- ^ Python evaluation mode: @exec@/@eval@
      -> String -- ^ Python source code
      -> TH.Q TH.Exp
expQQ mode qq_src = do
  -- We need to preprocess before passing it to python.
  let src     = prepareSource       mode qq_src
      src_var = prepareForVarLookup mode src
  antis  <- liftIO $ do
    -- We've embedded script into library and we need to pass source
    -- code of QQ to a script. It can contain whatever symbols so to
    -- be safe it's base16 encode. This encoding is very simple and we
    -- don't care much about efficiency here
    (code, stdout, stderr) <- readProcessWithExitCode "python"
        [ "-"
        , case mode of Eval -> "eval"
                       Exec -> "exec"
                       Fun  -> "exec"
        ]
      $ unlines [ script
                , "decode_and_print('" <>
                  concat [ [ intToDigit $ fromIntegral (w `shiftR` 4)
                           , intToDigit $ fromIntegral (w .&. 15) ]
                         | w <- BS.unpack $ T.encodeUtf8 $ T.pack src_var
                         ]
                  <> "')"
                ]
    case code of
      ExitSuccess   -> pure $ words stdout
      ExitFailure{} -> error stderr
  let args = [ [| basicBindInDict $(TH.lift nm) $(TH.dyn (chop nm)) |]
             | nm <- antis
             ]
      src_eval = prepareForEval mode antis src
  --
  [| \p_dict -> do
        mapM_ ($ p_dict) $(TH.listE args)
        pure $(TH.lift src_eval)
   |]


antiSuffix :: String
antiSuffix = "_hs"

-- | Chop antiquotation variable names to get the corresponding Haskell variable name.
chop :: String -> String
chop name = take (length name - length antiSuffix) name


----------------------------------------------------------------
-- Python source code transform
----------------------------------------------------------------

prepareSource :: Mode -> String -> String
prepareSource = \case
  Eval -> dropWhile isSpace
  Exec -> unindent
  Fun  -> unindent

prepareForVarLookup :: Mode -> String -> String
prepareForVarLookup = \case
  Eval -> id
  Exec -> id
  Fun  -> ("def __dummy__():\n"++) . indent

prepareForEval :: Mode -> [String] -> String -> String
prepareForEval mode vars src = case mode of
  Eval -> src
  Exec -> src
  Fun  -> "def _inline_python_("<>args<>"):\n"
    <> indent src
  where
    args = intercalate "," vars

-- Python is indentation based and quasiquotes do not strip leading
-- space. We have to do that ourself
unindent :: String -> String
unindent py_src = case lines py_src of
  []  -> ""
  -- Strip all leading space for 1-line scripts
  [l] -> dropWhile isSpace l
  -- For multiline script we require that first line should be empty
  l:ls
    | any (not . isSpace) l -> error "First line of multiline quasiquote must be empty"
    -- FIXME: We break multiline strings here. Badly. We need proper python lexer
    -- FIXME: We probably should just forbid tabs
    | otherwise ->
      let non_empty = filter (any (not . isSpace)) ls
          n         = minimum [ length (takeWhile (==' ') s) | s <- non_empty ]
      in unlines $ drop n <$> ls

indent :: String -> String
indent = unlines
       . map ("    "++)
       . lines
