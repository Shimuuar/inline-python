{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
-- |
-- Definition of data types used by inline-python. They are moved to
-- separate module since some are required for @inline-c@'s context
-- and we need context for 
module Python.Internal.Types
  ( PyObject(..)
  , pyCtx
  ) where

import Data.Map.Strict           qualified as Map
import Foreign.ForeignPtr
import Language.C.Types
import Language.C.Inline.Context


-- | Some python object. Since almost everything in python is mutable
--   it could only be accessed only in IO monad.
newtype PyObject = PyObject (ForeignPtr PyObject)

-- | @inline-c@ context for mapping 
pyCtx :: Context
pyCtx = mempty { ctxTypesTable = Map.fromList tytabs } where
  tytabs =
    [ (TypeName "PyObject", [t| PyObject |])
    ]
