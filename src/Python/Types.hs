{-# LANGUAGE CApiFFI           #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
-- |
module Python.Types
  ( PyObject(..)
  , unsafeWithPyObject
  , newPyObject
  ) where

import Data.Coerce

import Foreign.Ptr
import Foreign.ForeignPtr
import Language.C.Inline         qualified as C

import GHC.ForeignPtr

import Python.Internal.Types

----------------------------------------------------------------
C.context (C.baseCtx <> pyCtx)
C.include "<inline-python.h>"
----------------------------------------------------------------

unsafeWithPyObject :: forall a. PyObject -> (Ptr PyObject -> IO a) -> IO a
unsafeWithPyObject = coerce (unsafeWithForeignPtr @PyObject @a)

newPyObject :: Ptr PyObject -> IO PyObject
newPyObject
  = fmap PyObject
  . newForeignPtr py_XDECREF

py_XDECREF :: FunPtr (Ptr PyObject -> IO ())
py_XDECREF = [C.funPtr| void inline_py_XDECREF(PyObject* p) { Py_XDECREF(p); } |]
