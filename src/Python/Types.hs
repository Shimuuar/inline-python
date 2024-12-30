{-# LANGUAGE CApiFFI           #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
-- |
module Python.Types
  ( PyObject(..)
  , PyError(..)
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

unsafeWithPyObject :: forall a. PyObject -> (Ptr PyObject -> Py a) -> Py a
unsafeWithPyObject = coerce (unsafeWithForeignPtr @PyObject @a)


-- FIXME: !!! We can call DECREF in python thread only!!!

newPyObject :: Ptr PyObject -> Py PyObject
newPyObject
  = Py
  . fmap PyObject
  . newForeignPtr py_XDECREF

py_XDECREF :: FunPtr (Ptr PyObject -> IO ())
py_XDECREF = [C.funPtr| void inline_py_XDECREF(PyObject* p) { Py_XDECREF(p); } |]
