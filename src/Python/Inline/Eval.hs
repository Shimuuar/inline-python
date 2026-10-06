{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
-- |
-- Interface to python's @eval@ and @exec@ which gives programmer
-- control over local and global variables.
module Python.Inline.Eval
  ( -- * Python execution
    eval
  , exec
  , evalPyFunction
  , createModule
    -- * Source code
  , PyQuote(..)
  , PyQuoteFun(..)
  , Code
  , codeFromText
  , codeFromString
  , DictBinder
  , bindVar
    -- * Variable namespaces
  , Namespace(..)
  , Main(..)
  , Temp(..)
  , Dict(..)
  , Module(..)
  ) where

import Data.ByteString            qualified as BS
import Data.Text                  (Text)
import Data.Text                  qualified as T
import Data.Text.Encoding         qualified as T
import Language.C.Inline          qualified as C
import Language.C.Inline.Unsafe   qualified as CU

import Python.Internal.Types
import Python.Internal.Eval
import Python.Internal.Program
import Python.Inline.Literal

----------------------------------------------------------------
C.context (C.baseCtx <> pyCtx)
C.include "<inline-python.h>"
----------------------------------------------------------------

-- | Bind variable in dictionary
bindVar
  :: (ToPy a)
  => Text -- ^ Variable name
  -> a    -- ^ Variable value
  -> DictBinder
bindVar name a = DictBinder $ \p_dict -> runProgram $ do
  p_key <- progIOBracket $ BS.useAsCString (T.encodeUtf8 name)
  p_obj <- takeOwnership =<< progPy (throwOnNULL =<< basicToPy a)
  progPy $ do
    r <- Py [CU.block| int {
      PyObject* p_obj = $(PyObject* p_obj);
      return PyDict_SetItemString($(PyObject* p_dict), $(char* p_key), p_obj);
      } |]
    case r of
      0 -> pure ()
      _ -> mustThrowPyError


-- | Create python module from given quote. This function creates new
--   module using @importlib@ machinery and executes quote in context
--   of this module.
createModule
  :: Text
  -> PyQuote
  -> Py Module
createModule name quote = do
  -- It would be nice to use quasiquotes but linker chokes on them.
  -- We have to this hard way
  mdl <- Module <$> evalPyFunction Temp PyQuoteFun
    { code   = codeFromText $ T.unlines
        [ "def _inline_python_(name):"
        , "    import types"
        , "    return types.ModuleType(name)"
        ]
    , binder = bindVar "name" name
    }
  mdl <$ exec mdl mdl quote


