{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Convert between haskell data types and python values
module Python.Literal where

import Control.Monad.IO.Class
import Control.Monad.Trans.Cont
import Data.Int
import Data.Word
import Foreign.Ptr
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Storable

import Language.C.Inline         qualified as C
import Language.C.Inline.Context qualified as C
import Language.C.Types          qualified as C
import Language.C.Inline.Unsafe  qualified as CU

import Python.Types
import Python.Context

C.context (C.baseCtx <> pyCtx)
C.include "<inline-python.h>"



class Literal a where
  basicToPy :: a -> IO (Ptr PyObject)
  basicFromPy :: Ptr PyObject -> IO (Maybe a)


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


deriving via CLLong  instance Literal Int64
deriving via CULLong instance Literal Word64

instance Literal Int where
  basicToPy   = basicToPy @Int64 . fromIntegral
  basicFromPy = (fmap . fmap) fromIntegral . basicFromPy @Int64
