{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Conversion between haskell data types and python values
module Python.Inline.Literal
  ( Literal(..)
  , toPy
  , fromPy
  ) where

import Control.Exception
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


