{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE QuasiQuotes              #-}
{-# LANGUAGE TemplateHaskell          #-}
-- |
-- Conversion between haskell data types and python values
module Python.Inline.Literal
  ( Literal(..)
  , toPy
  , fromPy
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Cont
import Data.Int
import Data.Word
import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Storable

import Language.C.Inline         qualified as C
import Language.C.Inline.Unsafe  qualified as CU

import Python.Types
import Python.Internal.Types

----------------------------------------------------------------
C.context (C.baseCtx <> pyCtx)
C.include "<inline-python.h>"
----------------------------------------------------------------

class Literal a where
  -- | Convert haskell value to python object. It should generate new
  --   object (except for immortals like None, True, etc).
  basicToPy   :: a -> IO (Ptr PyObject)
  -- |
  basicFromPy :: Ptr PyObject -> IO (Maybe a)

fromPy :: Literal a => PyObject -> IO (Maybe a)
fromPy py = unsafeWithPyObject py basicFromPy

toPy :: Literal a => a -> IO PyObject
toPy a = mask_ $ newPyObject =<< basicToPy a


instance Literal CLong where
  basicToPy i =
    [CU.exp| PyObject* { PyLong_FromLong($(long i)) } |]
  basicFromPy p_py = evalContT $ do
    p_out <- ContT $ alloca @CLong
    r     <- liftIO $ [CU.block| int {
      * $(long* p_out) = PyLong_AsLong($(PyObject *p_py));
      INLINE_PY_SIMPLE_ERROR_HANDLING();
      } |]
    liftIO $ case r of
      0 -> Just <$> peek p_out
      _ -> pure Nothing

instance Literal CLLong where
  basicToPy i =
    [CU.exp| PyObject* { PyLong_FromLongLong($(long long i)) } |]
  basicFromPy p_py = evalContT $ do
    p_out <- ContT $ alloca @CLLong
    r     <- liftIO $ [CU.block| int {
      * $(long long* p_out) = PyLong_AsLongLong($(PyObject *p_py));
      INLINE_PY_SIMPLE_ERROR_HANDLING();
      } |]
    liftIO $ case r of
      0 -> Just <$> peek p_out
      _ -> pure Nothing

instance Literal CULong where
  basicToPy i =
    [CU.exp| PyObject* { PyLong_FromUnsignedLong($(unsigned long i)) } |]
  basicFromPy p_py = evalContT $ do
    p_out <- ContT $ alloca @CULong
    r     <- liftIO $ [CU.block| int {
      * $(unsigned long* p_out) = PyLong_AsUnsignedLong($(PyObject *p_py));
      INLINE_PY_SIMPLE_ERROR_HANDLING();
      } |]
    liftIO $ case r of
      0 -> Just <$> peek p_out
      _ -> pure Nothing

instance Literal CULLong where
  basicToPy i =
    [CU.exp| PyObject* { PyLong_FromUnsignedLongLong($(unsigned long long i)) } |]
  basicFromPy p_py = evalContT $ do
    p_out <- ContT $ alloca @CULLong
    r     <- liftIO $ [CU.block| int {
      * $(unsigned long long* p_out) = PyLong_AsUnsignedLongLong($(PyObject *p_py));
      INLINE_PY_SIMPLE_ERROR_HANDLING();
      } |]
    liftIO $ case r of
      0 -> Just <$> peek p_out
      _ -> pure Nothing

instance Literal CDouble where
  basicToPy i =
    [CU.exp| PyObject* { PyFloat_FromDouble($(double i)) } |]
  basicFromPy p_py = evalContT $ do
    p_out <- ContT $ alloca @CDouble
    r     <- liftIO $ [CU.block| int {
      * $(double* p_out) = PyFloat_AsDouble($(PyObject *p_py));
      INLINE_PY_SIMPLE_ERROR_HANDLING();
      } |]
    liftIO $ case r of
      0 -> Just <$> peek p_out
      _ -> pure Nothing

deriving via CLLong  instance Literal Int64
deriving via CULLong instance Literal Word64
deriving via CDouble instance Literal Double

instance Literal Int where
  basicToPy   = basicToPy @Int64 . fromIntegral
  basicFromPy = (fmap . fmap) fromIntegral . basicFromPy @Int64


----------------------------------------------------------------
-- Functions marshalling
----------------------------------------------------------------

-- NOTE: [Creation of python functions]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- We need to call haskell from python we need to first to create
-- FunPtr on haskell side and wrap it using python's C API. Process is
-- unpleasantly convoluted.
--
-- Function marshalled from haskell side could only be called with
-- using positional arguments. Two calling conventions are supported:
--
--  - METH_O        for 1-argument
--  - METH_FASTCALL for 2+ argument functions
--
-- One problem is we need to keep PyMethodDef struct alive while
-- function object is alive and GC it when function object is GC'd.
-- To that end we use horrible hack.
--
-- PyMethodDef is allocated on C heap, wrapped into PyCapsule passed
-- to CFunction as self. It does seems icky. However it does the trick.
-- Maybe there's other way.



-- NOTE: [Exceptions in callbacks]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- We absolutely must not allow unhandled haskell exceptions in
-- callbacks from python. Else they will hit C wall and terminate
-- program. They MUST be converted to python ones.
--
-- FIXME: figure out masking for python's call. I DON'T want get hit
--        with async exception out of the blue


instance (Literal a, Literal b) => Literal (a -> IO b) where
  basicToPy f = do
    -- C function pointer for callback
    f_ptr <- wrapO $ \_ p_a -> do
      ma <- basicFromPy p_a
      case ma of
        Nothing -> do
          [C.exp| void { PyErr_SetString(PyExc_RuntimeError, "Converiosn failed")} |]
          return nullPtr  
        Just a  -> basicToPy =<< f a
    --
    [C.exp| PyObject* {
      inline_py_function_wrapper(
          $(PyObject* (*f_ptr)(PyObject*, PyObject*)),
          METH_O)
      }|]
  basicFromPy = error "IMPOSSIBLE!"

instance (Literal a1, Literal a2, Literal b) => Literal (a1 -> a2 -> IO b) where
  basicToPy f = do
    -- Create haskell function
    f_ptr <- wrapFastcall $ \_ p_arr -> \case
      2 -> do
        peekElemOff p_arr 0 >>= basicFromPy >>= \case
          Nothing -> do
            [C.exp| void { PyErr_SetString(PyExc_RuntimeError, "Conversion failed")} |]
            return nullPtr
          Just a -> peekElemOff p_arr 0 >>= basicFromPy >>= \case
            Nothing -> do
              [C.exp| void { PyErr_SetString(PyExc_RuntimeError, "Conversion failed")} |]
              return nullPtr
            Just b -> basicToPy =<< f a b
      _ -> do
        [C.exp| void { PyErr_SetString(PyExc_RuntimeError, "Expecting two arguments")} |]
        return nullPtr
    [C.block| PyObject* {
      _PyCFunctionFast impl = $(PyObject* (*f_ptr)(PyObject*, PyObject*const*, int64_t));
      return inline_py_function_wrapper(
          (PyCFunction)impl,
          METH_FASTCALL);
      }|]
  basicFromPy = error "IMPOSSIBLE!"




type FunWrapper a = a -> IO (FunPtr a)

foreign import ccall "wrapper" wrapO
  :: FunWrapper (Ptr PyObject -> Ptr PyObject -> IO (Ptr PyObject))

foreign import ccall "wrapper" wrapFastcall
  :: FunWrapper (Ptr PyObject -> Ptr (Ptr PyObject) -> Int64 -> IO (Ptr PyObject))
