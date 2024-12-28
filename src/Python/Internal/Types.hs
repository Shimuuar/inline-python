{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
-- |
-- Definition of data types used by inline-python. They are moved to
-- separate module since some are required for @inline-c@'s context
-- and we need context for
module Python.Internal.Types
  ( PyObject(..)
  , PyError(..)
  , pyCtx
    -- * Patterns
  , pattern INLINE_PY_OK
  , pattern INLINE_PY_ERR_COMPILE
  , pattern INLINE_PY_ERR_EVAL
  ) where

import Control.Exception
import Data.Map.Strict           qualified as Map
import Foreign.ForeignPtr
import Foreign.C.Types
import Language.C.Types
import Language.C.Inline.Context


-- | Some python object. Since almost everything in python is mutable
--   it could only be accessed only in IO monad.
newtype PyObject = PyObject (ForeignPtr PyObject)


data PyError = PyError String
  deriving stock (Show)

instance Exception PyError

-- | @inline-c@ context for mapping
pyCtx :: Context
pyCtx = mempty { ctxTypesTable = Map.fromList tytabs } where
  tytabs =
    [ (TypeName "PyObject", [t| PyObject |])
    ]


pattern INLINE_PY_OK, INLINE_PY_ERR_COMPILE, INLINE_PY_ERR_EVAL :: CInt
pattern INLINE_PY_OK          = 0
pattern INLINE_PY_ERR_COMPILE = 1
pattern INLINE_PY_ERR_EVAL    = 2
