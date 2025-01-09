{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Thin wrappers over C API
module Python.Internal.CAPI
  ( decref
  , incref
    -- * Simple wrappers
  , basicNewDict
  , basicCallKwdOnly
  ) where

import Foreign.Ptr
import Language.C.Inline          qualified as C
import Language.C.Inline.Unsafe   qualified as CU

import Python.Internal.Types


----------------------------------------------------------------
C.context (C.baseCtx <> pyCtx)
C.include "<inline-python.h>"
----------------------------------------------------------------


decref :: Ptr PyObject -> Py ()
decref p = Py [CU.exp| void { Py_DECREF($(PyObject* p)) } |]

incref :: Ptr PyObject -> Py ()
incref p = Py [CU.exp| void { Py_INCREF($(PyObject* p)) } |]

basicNewDict :: Py (Ptr PyObject)
basicNewDict = Py [CU.exp| PyObject* { PyDict_New() } |]

-- | Call python function using only keyword arguments
basicCallKwdOnly
  :: Ptr PyObject -- ^ Function object
  -> Ptr PyObject -- ^ Keywords. Must be dictionary
  -> Py (Ptr PyObject)
basicCallKwdOnly fun kwd = Py [CU.block| PyObject* {
  PyObject* args = PyTuple_Pack(0);
  PyObject* res  = PyObject_Call($(PyObject *fun), args, $(PyObject *kwd));
  Py_DECREF(args);
  return res;
  } |]
