{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
module Python.Internal.EvalQQ
  ( -- * Evaluators and QQ
    evaluatorPyf
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

-- | Python's variable name encoded using UTF-8. It exists in order to
--   avoid working with @String@ at runtime.
newtype PyVarName = PyVarName BS.ByteString
  deriving stock (Show, TH.Lift)

varName :: String -> PyVarName
varName = PyVarName . T.encodeUtf8 . T.pack

unsafeWithPyVarName :: PyVarName -> Program r (Ptr CChar)
unsafeWithPyVarName (PyVarName bs)
  = progIOBracket (BS.unsafeUseAsCString bs)


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

evaluatorPyf :: PyQuote -> Py PyObject
evaluatorPyf (PyQuote code binder) = runProgram $ do
  p_locals <- takeOwnership =<< progPy basicNewDict
  p_kwargs <- takeOwnership =<< progPy basicNewDict
  progPy $ do
    -- Create function in p_locals
    exec Main (DictPtr p_locals) (PyQuote code mempty)
    -- Look up function
    p_fun <- getFunctionObject p_locals >>= \case
      NULL -> throwM $ PyInternalError "_inline_python_ must be present"
      p    -> pure p
    -- Call python function we just constructed
    binder.bind p_kwargs
    newPyObject =<< throwOnNULL =<< basicCallKwdOnly p_fun p_kwargs

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
