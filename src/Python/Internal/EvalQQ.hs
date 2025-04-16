{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
module Python.Internal.EvalQQ
  ( -- * Evaluators and QQ
    evaluatorPymain
  , evaluatorPy_
  , evaluatorPye
  , evaluatorPyf
  , Code
  , codeFromText
  , codeFromString
  , DictBinder
  , PyQuote(..)
    -- * Code generation
  , expQQ
  , Mode(..)
  ) where

import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Trans.Cont (ContT(..))
import Data.Bits
import Data.Char
import Data.List                 (intercalate)
import Data.ByteString           qualified as BS
import Data.ByteString.Unsafe    qualified as BS
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

import Python.Internal.Types
import Python.Internal.Program
import Python.Internal.Eval
import Python.Internal.CAPI
import Python.Inline.Literal


----------------------------------------------------------------
C.context (C.baseCtx <> pyCtx)
C.include "<inline-python.h>"
----------------------------------------------------------------

data PyQuote = PyQuote
  { code   :: !Code
  , binder :: !DictBinder
  } 


-- | UTF-8 encoded python source code. Usually it's produced by
--   Template Haskell's 'TH.lift' function.
newtype Code = Code BS.ByteString
  deriving stock (Show, TH.Lift)

-- | Create properly encoded @Code@. This function doesn't check
--   syntactic validity.
codeFromText :: T.Text -> Code
codeFromText = Code . T.encodeUtf8

-- | Create properly encoded @Code@. This function doesn't check
--   syntactic validity.
codeFromString :: String -> Code
codeFromString = codeFromText . T.pack

unsafeWithCode :: Code -> Program r (Ptr CChar)
unsafeWithCode (Code bs) = Program $ ContT $ \fun ->
  Py (BS.unsafeUseAsCString bs $ unsafeRunPy . fun)


-- | Python's variable name encoded using UTF-8. It exists in order to
--   avoid working with @String@ at runtime.
newtype PyVarName = PyVarName BS.ByteString
  deriving stock (Show, TH.Lift)

varName :: String -> PyVarName
varName = PyVarName . T.encodeUtf8 . T.pack

unsafeWithPyVarName :: PyVarName -> Program r (Ptr CChar)
unsafeWithPyVarName (PyVarName bs) = Program $ ContT $ \fun ->
  Py (BS.unsafeUseAsCString bs $ unsafeRunPy . fun)


-- | Closure which stores values in provided dictionary
newtype DictBinder = DictBinder { bind :: Ptr PyObject -> Py () }

instance Semigroup DictBinder where
  f <> g = DictBinder $ \p -> f.bind p >> g.bind p
instance Monoid DictBinder where
  mempty = DictBinder $ \_ -> pure ()


bindVar :: ToPy a => PyVarName -> a -> DictBinder
bindVar var a = DictBinder $ \p_dict -> runProgram $ do
  p_key <- unsafeWithPyVarName var
  p_obj <- takeOwnership =<< progPy (throwOnNULL =<< basicToPy a)
  progPy $ do
    r <- Py [CU.block| int {
      PyObject* p_obj = $(PyObject* p_obj);
      return PyDict_SetItemString($(PyObject* p_dict), $(char* p_key), p_obj);
      } |]
    case r of
      0 -> pure ()
      _ -> mustThrowPyError



----------------------------------------------------------------
-- Evaluators
----------------------------------------------------------------

-- | Evaluate expression within context of @__main__@ module. All
--   variables defined in this evaluator persist.
pyExecExpr
  :: Ptr PyObject -- ^ Globals
  -> Ptr PyObject -- ^ Locals
  -> Code         -- ^ Python source code
  -> Py ()
pyExecExpr p_globals p_locals src = runProgram $ do
  p_py <- unsafeWithCode src
  progIO [C.block| void {
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
  progPy checkThrowPyError

-- | Evaluate expression with fresh local environment
pyEvalExpr
  :: Ptr PyObject -- ^ Globals
  -> Ptr PyObject -- ^ Locals
  -> Code         -- ^ Python source code
  -> Py PyObject
pyEvalExpr p_globals p_locals src = runProgram $ do
  p_py  <- unsafeWithCode src
  p_res <- progIO [C.block| PyObject* {
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
  progPy $ checkThrowPyError
  progPy $ newPyObject p_res


evaluatorPymain :: PyQuote -> Py ()
evaluatorPymain (PyQuote code binder) = do
  p_main <- basicMainDict
  binder.bind p_main
  pyExecExpr p_main p_main code

evaluatorPy_ :: PyQuote -> Py ()
evaluatorPy_ (PyQuote code binder) = runProgram $ do
  p_globals <- progPy basicMainDict
  p_locals  <- takeOwnership =<< progPy basicNewDict
  progPy $ do
    binder.bind p_locals
    pyExecExpr p_globals p_locals code

evaluatorPye :: PyQuote -> Py PyObject
evaluatorPye (PyQuote code binder) = runProgram $ do
  p_globals <- progPy basicMainDict
  p_locals  <- takeOwnership =<< progPy basicNewDict
  progPy $ do
    binder.bind p_locals
    pyEvalExpr p_globals p_locals code

evaluatorPyf :: PyQuote -> Py PyObject
evaluatorPyf (PyQuote code binder) = runProgram $ do
  p_globals <- progPy basicMainDict
  p_locals  <- takeOwnership =<< progPy basicNewDict
  p_kwargs  <- takeOwnership =<< progPy basicNewDict
  progPy $ do
    -- Create function in p_locals
    pyExecExpr p_globals p_locals code
    -- Look up function
    binder.bind p_kwargs
    p_fun <- getFunctionObject p_locals >>= \case
      NULL -> throwM $ PyInternalError "_inline_python_ must be present"
      p    -> pure p
    -- Call python function we just constructed
    newPyObject =<< throwOnNULL =<< basicCallKwdOnly p_fun p_kwargs

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
      ExitFailure{} -> fail stderr
  let args = [ [| bindVar $(TH.lift (varName nm)) $(TH.dyn (chop nm)) |]
             | nm <- antis
             ]
      src_eval = prepareForEval mode antis src
  --
  [| PyQuote ($(TH.lift $ codeFromString src_eval))
             (mconcat $(TH.listE args))
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
