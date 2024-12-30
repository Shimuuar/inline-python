{-# LANGUAGE CApiFFI           #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
-- |
module Python.Types
  ( PyObject(..)
  , PyError(..)
  , unsafeWithPyObject
  ) where

import Data.Coerce

import Foreign.Ptr
import Foreign.ForeignPtr
import Language.C.Inline         qualified as C

import GHC.ForeignPtr

import Python.Internal.Types

unsafeWithPyObject :: forall a. PyObject -> (Ptr PyObject -> Py a) -> Py a
unsafeWithPyObject = coerce (unsafeWithForeignPtr @PyObject @a)


