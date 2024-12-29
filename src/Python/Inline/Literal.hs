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
  basicToPy   :: a -> IO (Ptr PyObject)
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



instance (Literal a, Literal b) => Literal (a -> IO b) where
  basicToPy f = do
    -- Create haskell function
    f_ptr <- wrapO $ \_ p_a -> do
      ma <- basicFromPy p_a
      case ma of
        Nothing -> throwIO $ PyError "Failed conversion"
        Just a  -> basicToPy =<< f a
    --
    p_r  <- [C.block| PyObject* {
      PyCFunction impl = $(PyObject* (*f_ptr)(PyObject*, PyObject*));
      // Create function definition
      PyMethodDef *meth = malloc(sizeof(PyMethodDef));
      meth->ml_name  = "[inline_python]";
      meth->ml_meth  = impl;
      meth->ml_flags = METH_O;
      meth->ml_doc   = "Wrapper constructed by inline-python";
      PyObject* meth_obj = PyCapsule_New(meth, NULL, &inline_py_free_capsule);
      // Create function object and attach capsule to it
      PyObject* f = PyCFunction_New(meth, meth_obj);
      Py_DECREF(meth_obj);
      return f;
      }|]
    return p_r
  basicFromPy = error "IMPOSSIBLE!"

instance (Literal a1, Literal a2, Literal b) => Literal (a1 -> a2 -> IO b) where
  basicToPy f = do
    -- Create haskell function
    f_ptr <- wrapFastcall $ \_ p_arr n -> do
      when (n /= 2) $ do
        error "Bad number if args"
      ma <- basicFromPy =<< peekElemOff p_arr 0
      mb <- basicFromPy =<< peekElemOff p_arr 1
      case ma of
        Nothing -> throwIO $ PyError "Failed conversion"
        Just a  -> case mb of
          Nothing -> throwIO $ PyError "Failed conversion"
          Just b  -> basicToPy =<< f a b
    p_r  <- [C.block| PyObject* {
      _PyCFunctionFast impl = $(PyObject* (*f_ptr)(PyObject*, PyObject*const*, int64_t));
      // Create function definition
      PyMethodDef *meth = malloc(sizeof(PyMethodDef));
      meth->ml_name  = "[inline_python]";
      meth->ml_meth  = (PyCFunction)impl;
      meth->ml_flags = METH_FASTCALL;
      meth->ml_doc   = "Wrapper constructed by inline-python";
      PyObject* meth_obj = PyCapsule_New(meth, NULL, &inline_py_free_capsule);
      // Create function object and attach capsule to it
      PyObject* f = PyCFunction_New(meth, meth_obj);
      Py_DECREF(meth_obj);
      return f;
      }|]
    return p_r
  basicFromPy = error "IMPOSSIBLE!"




type FunWrapper a = a -> IO (FunPtr a)

foreign import ccall "wrapper" wrapO
  :: FunWrapper (Ptr PyObject -> Ptr PyObject -> IO (Ptr PyObject))

foreign import ccall "wrapper" wrapFastcall
  :: FunWrapper (Ptr PyObject -> Ptr (Ptr PyObject) -> Int64 -> IO (Ptr PyObject))
