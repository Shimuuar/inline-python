{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE QuasiQuotes              #-}
{-# LANGUAGE TemplateHaskell          #-}
-- |
-- Conversion between haskell data types and python values
module Python.Inline.Literal
  ( FromPy(..)
  , ToPy(..)
  , toPy
  , fromPy
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
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
import Python.Internal.Eval

import Python.Internal.Program

----------------------------------------------------------------
C.context (C.baseCtx <> pyCtx)
C.include "<inline-python.h>"
----------------------------------------------------------------

-- | Convert haskell value to python value. 
class ToPy a where
  -- | Convert haskell value to python object. This function returns
  --   strong reference to newly create objects (except singletons
  --   like @None@, @True@, etc). Normally conversion should not fail
  --   but when it does function must raise suitable python exception
  --   and return @NULL@. Caller must check that.
  --
  --   This is low level function. It should be only used when working
  --   with python's C API. Otherwise 'toPy' is preferred.
  basicToPy :: a -> Py (Ptr PyObject)

-- | Convert python object to haskell value. 
class FromPy a where
  -- | Convert python value into haskell value. This function should
  --   not modify python's data and raise both python and haskell
  --   exceptions.
  --
  --   This is low level function. It should be only used when working
  --   with python's C API. Otherwise 'fromPy' is preferred.
  basicFromPy :: Ptr PyObject -> Py (Maybe a)

-- | Convert python object to haskell value
fromPy :: FromPy a => PyObject -> IO (Maybe a)
fromPy py = runPy $ unsafeWithPyObject py basicFromPy

-- | Convert haskell value to a python object
toPy :: ToPy a => a -> IO PyObject
toPy a = runPy $ newPyObject =<< basicToPy a


instance ToPy CLong where
  basicToPy i = Py [CU.exp| PyObject* { PyLong_FromLong($(long i)) } |]
instance FromPy CLong where
  basicFromPy p_py = Py $ evalContT $ do
    p_out <- ContT $ alloca @CLong
    r     <- liftIO $ [CU.block| int {
      * $(long* p_out) = PyLong_AsLong($(PyObject *p_py));
      INLINE_PY_SIMPLE_ERROR_HANDLING();
      } |]
    liftIO $ case r of
      0 -> Just <$> peek p_out
      _ -> pure Nothing

instance ToPy CLLong where
  basicToPy i = Py [CU.exp| PyObject* { PyLong_FromLongLong($(long long i)) } |]
instance FromPy CLLong where
  basicFromPy p_py = Py $ evalContT $ do
    p_out <- ContT $ alloca @CLLong
    r     <- liftIO $ [CU.block| int {
      * $(long long* p_out) = PyLong_AsLongLong($(PyObject *p_py));
      INLINE_PY_SIMPLE_ERROR_HANDLING();
      } |]
    liftIO $ case r of
      0 -> Just <$> peek p_out
      _ -> pure Nothing

instance ToPy CULong where
  basicToPy i = Py [CU.exp| PyObject* { PyLong_FromUnsignedLong($(unsigned long i)) } |]
instance FromPy CULong where
  basicFromPy p_py = Py $ evalContT $ do
    p_out <- ContT $ alloca @CULong
    r     <- liftIO $ [CU.block| int {
      * $(unsigned long* p_out) = PyLong_AsUnsignedLong($(PyObject *p_py));
      INLINE_PY_SIMPLE_ERROR_HANDLING();
      } |]
    liftIO $ case r of
      0 -> Just <$> peek p_out
      _ -> pure Nothing

instance ToPy CULLong where
  basicToPy i = Py [CU.exp| PyObject* { PyLong_FromUnsignedLongLong($(unsigned long long i)) } |]
instance FromPy CULLong where
  basicFromPy p_py = Py $ evalContT $ do
    p_out <- ContT $ alloca @CULLong
    r     <- liftIO $ [CU.block| int {
      * $(unsigned long long* p_out) = PyLong_AsUnsignedLongLong($(PyObject *p_py));
      INLINE_PY_SIMPLE_ERROR_HANDLING();
      } |]
    liftIO $ case r of
      0 -> Just <$> peek p_out
      _ -> pure Nothing

instance ToPy CDouble where
  basicToPy i = Py [CU.exp| PyObject* { PyFloat_FromDouble($(double i)) } |]
instance FromPy CDouble where
  basicFromPy p_py = Py $ evalContT $ do
    p_out <- ContT $ alloca @CDouble
    r     <- liftIO $ [CU.block| int {
      * $(double* p_out) = PyFloat_AsDouble($(PyObject *p_py));
      INLINE_PY_SIMPLE_ERROR_HANDLING();
      } |]
    liftIO $ case r of
      0 -> Just <$> peek p_out
      _ -> pure Nothing

deriving via CLLong  instance ToPy   Int64
deriving via CLLong  instance FromPy Int64
deriving via CULLong instance ToPy   Word64
deriving via CULLong instance FromPy Word64
deriving via CDouble instance ToPy   Double
deriving via CDouble instance FromPy Double

instance ToPy Int where
  basicToPy   = basicToPy @Int64 . fromIntegral
instance FromPy Int where
  basicFromPy = (fmap . fmap) fromIntegral . basicFromPy @Int64

-- TODO: Int may be 32 or 64 bit!
-- TODO: Int{8,16,32} & Word{8,16,32}

instance ToPy Bool where
  basicToPy True  = Py [CU.exp| PyObject* { Py_True  } |]
  basicToPy False = Py [CU.exp| PyObject* { Py_False } |]

-- | Uses python's truthiness conventions
instance FromPy Bool where
  basicFromPy p = Py $ do
    r <- [CU.block| int {
      int r = PyObject_IsTrue($(PyObject* p));
      PyErr_Clear();
      return r;
      } |]
    case r of
      0 -> pure $ Just False
      1 -> pure $ Just True
      _ -> pure $ Nothing

instance (ToPy a, ToPy b) => ToPy (a,b) where
  basicToPy (a,b) = do
    basicToPy a >>= \case
      NULL -> pure NULL
      p_a  -> basicToPy b >>= \case
        NULL -> pure $ NULL
        p_b  -> Py [CU.exp| PyObject* { PyTuple_Pack(2, $(PyObject* p_a), $(PyObject* p_b)) } |]

instance (FromPy a, FromPy b) => FromPy (a,b) where
  basicFromPy p_tup = evalContT $ do
    -- Unpack 2-tuple.
    p_args    <- withPyAllocaArray 2
    unpack_ok <- liftIO [CU.exp| int {
      inline_py_unpack_iterable($(PyObject *p_tup), 2, $(PyObject **p_args))
      }|]
    -- We may want to extract exception to haskell side later
    liftIO [CU.exp| void { PyErr_Clear() } |]
    when (unpack_ok /= 0) $ abort Nothing
    -- Unpack 2-elements
    lift $ do
      p_a <- liftIO $ peekElemOff p_args 0
      p_b <- liftIO $ peekElemOff p_args 1
      let parse = basicFromPy p_a >>= \case
            Nothing -> pure Nothing
            Just a  -> basicFromPy p_b >>= \case
              Nothing -> pure Nothing
              Just b  -> pure $ Just (a,b)
          fini  = liftIO [CU.block| void {
            Py_XDECREF( $(PyObject* p_a) );
            Py_XDECREF( $(PyObject* p_b) );
            } |]
      parse `finallyPy` fini



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


instance (FromPy a, ToPy b) => ToPy (a -> IO b) where
  basicToPy f = Py $ do
    -- C function pointer for callback
    f_ptr <- wrapO $ \_ p_a -> pyProg $ do
      a <- liftIO (unPy (basicFromPy p_a)) >>= \case
        Nothing -> abortM $ raiseUndecodedArg 1 1
        Just a  -> pure a
      liftIO $ unPy . basicToPy =<< f a
    --
    [C.exp| PyObject* {
      inline_py_function_wrapper(
          $(PyObject* (*f_ptr)(PyObject*, PyObject*)),
          METH_O)
      }|]

instance (FromPy a1, FromPy a2, ToPy b) => ToPy (a1 -> a2 -> IO b) where
  basicToPy f = Py $ do
    -- Create haskell function
    f_ptr <- wrapFastcall $ \_ p_arr n -> pyProg $ do
      when (n /= 2) $ abortM $ raiseBadNArgs 2 n
      a <- loadArgFastcall p_arr 0 n
      b <- loadArgFastcall p_arr 1 n
      liftIO $ unPy . basicToPy =<< f a b
    -- Create python function
    [C.block| PyObject* {
      _PyCFunctionFast impl = $(PyObject* (*f_ptr)(PyObject*, PyObject*const*, int64_t));
      return inline_py_function_wrapper(
          (PyCFunction)impl,
          METH_FASTCALL);
      }|]

loadArgFastcall :: FromPy a => Ptr (Ptr PyObject) -> Int -> Int64 -> Program (Ptr PyObject) a
loadArgFastcall p_arr i tot = do
  p <- liftIO $ peekElemOff p_arr i
  liftIO (unPy (basicFromPy p)) >>= \case
    Nothing -> abortM $ raiseUndecodedArg (fromIntegral i + 1) (fromIntegral tot)
    Just a  -> pure a


raiseUndecodedArg :: CInt -> CInt -> Py (Ptr PyObject)
raiseUndecodedArg n tot = Py [CU.block| PyObject* {
  char err[256];
  sprintf(err, "Failed to decode function argument %i of %i", $(int n), $(int tot));
  PyErr_SetString(PyExc_TypeError, err);
  return NULL;
  } |]

raiseBadNArgs :: CInt -> Int64 -> Py (Ptr PyObject)
raiseBadNArgs tot n = Py [CU.block| PyObject* {
  char err[256];
  sprintf(err, "Function takes exactly %i arguments (%li given)", $(int tot), $(int64_t n));
  PyErr_SetString(PyExc_TypeError, err);
  return NULL;
  } |]

pyProg :: Program (Ptr PyObject) (Ptr PyObject) -> IO (Ptr PyObject)
pyProg io = unPy $ evalContT io `catchPy` convertHaskell2Py


type FunWrapper a = a -> IO (FunPtr a)

foreign import ccall "wrapper" wrapO
  :: FunWrapper (Ptr PyObject -> Ptr PyObject -> IO (Ptr PyObject))

foreign import ccall "wrapper" wrapFastcall
  :: FunWrapper (Ptr PyObject -> Ptr (Ptr PyObject) -> Int64 -> IO (Ptr PyObject))
