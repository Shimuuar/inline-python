{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
module Python.Internal.EvalQQ
  ( -- * Evaluators and QQ
    evaluatorPymain
  , evaluatorPy_
  , evaluatorPye
  , evaluatorPyf
    -- * Code generation
  , expQQ
  , Mode(..)
  ) where

import Control.Monad.IO.Class
import Data.Bits
import Data.Char
import Data.List                 (intercalate)
import Data.ByteString           qualified as BS
import Data.Text                 qualified as T
import Data.Text.Encoding        qualified as T
import Foreign.C.Types
import Foreign.Ptr
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
import Python.Internal.CAPI
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
pyExecExpr p_globals p_locals src = runProgram $ do
  p_py <- withPyCString src
  progPy $ do
    Py [C.block| void {
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

-- | Evaluate expression with fresh local environment
pyEvalExpr
  :: Ptr PyObject -- ^ Globals
  -> Ptr PyObject -- ^ Locals
  -> String       -- ^ Python source code
  -> Py PyObject
pyEvalExpr p_globals p_locals src = runProgram $ do
  p_py  <- withPyCString src
  progPy $ do
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


evaluatorPymain :: (Ptr PyObject -> Py String) -> Py ()
evaluatorPymain getSource = do
  p_main <- basicMainDict
  src    <- getSource p_main
  pyExecExpr p_main p_main src

evaluatorPy_ :: (Ptr PyObject -> Py String) -> Py ()
evaluatorPy_ getSource = runProgram $ do
  p_globals <- progPy basicMainDict
  p_locals  <- takeOwnership =<< progPy basicNewDict
  progPy $ pyExecExpr p_globals p_locals =<< getSource p_locals

evaluatorPye :: (Ptr PyObject -> Py String) -> Py PyObject
evaluatorPye getSource = runProgram $ do
  p_globals <- progPy basicMainDict
  p_locals  <- takeOwnership =<< progPy basicNewDict
  progPy $ pyEvalExpr p_globals p_locals =<< getSource p_locals

evaluatorPyf :: (Ptr PyObject -> Py String) -> Py PyObject
evaluatorPyf getSource = runProgram $ do
  p_globals <- progPy basicMainDict
  p_locals  <- takeOwnership =<< progPy basicNewDict
  p_kwargs  <- takeOwnership =<< progPy basicNewDict
  progPy $ do
    -- Create function in p_locals
    pyExecExpr p_globals p_locals =<< getSource p_kwargs
    -- Look up function
    p_fun <- getFunctionObject p_locals >>= \case
      NULL -> error "INTERNAL ERROR: _inline_python_ must be present"
      p    -> pure p
    -- Call python function we just constructed
    newPyObject =<< throwOnNULL =<< basicCallKwdOnly p_fun p_kwargs


basicBindInDict :: ToPy a => String -> a -> Ptr PyObject -> Py ()
basicBindInDict name a p_dict = runProgram $ do
  p_key <- withPyCString name
  p_obj <- takeOwnership =<< progPy (throwOnNULL =<< basicToPy a)
  progPy $ do
    r <- Py [C.block| int {
      PyObject* p_obj = $(PyObject* p_obj);
      return PyDict_SetItemString($(PyObject* p_dict), $(char* p_key), p_obj);
      } |]
    case r of
      0 -> pure ()
      _ -> mustThrowPyError

-- | Return dict of @__main__@ module
basicMainDict :: Py (Ptr PyObject)
basicMainDict = Py [CU.block| PyObject* {
  PyObject* main_module = PyImport_AddModule("__main__");
  if( PyErr_Occurred() )
      return NULL;
  return PyModule_GetDict(main_module);
  }|]

getFunctionObject :: Ptr PyObject -> Py (Ptr PyObject)
getFunctionObject p_dict = do
  Py [CU.exp| PyObject* { PyDict_GetItemString($(PyObject *p_dict), "_inline_python_") } |]



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
