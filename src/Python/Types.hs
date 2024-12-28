{-# LANGUAGE CApiFFI           #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
-- |
module Python.Types
  ( PyObject(..)
  , unsafeWithPyObject
  ) where

import Data.Coerce

import Foreign.Ptr
import Foreign.ForeignPtr
import Language.C.Inline.Context qualified as C
import Language.C.Types   qualified as C
import Data.Map.Strict qualified as Map
import Language.Haskell.TH.Quote

import GHC.ForeignPtr

newtype PyObject = PyObject (ForeignPtr PyObject)

unsafeWithPyObject :: forall a. PyObject -> (Ptr PyObject -> IO a) -> IO a
unsafeWithPyObject = coerce (unsafeWithForeignPtr @PyObject @a)
