{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE QuasiQuotes              #-}
{-# LANGUAGE TemplateHaskell          #-}
-- |
-- Conversion between haskell data types and python values
module Python.Inline.Literal
  ( FromPy(..)
  , ToPy(..)
  , toPy
  , fromPyEither
  , fromPy
  , fromPy'
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Data.Char
import Data.Int
import Data.Word
import Foreign.Ptr
import Foreign.C.Types
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
  --   like @None@, @True@, etc).
  --
  --   Implementations should try to avoid failing conversions.
  --   There're two ways of signalling failure: errors on python side
  --   should return NULL and raise python exception. Haskell code
  --   should just throw exception.
  --
  --   This is low level function. It should be only used when working
  --   with python's C API. Otherwise 'toPy' is preferred.
  basicToPy :: a -> Py (Ptr PyObject)
  -- | Old hack for handling of strings
  basicListToPy :: [a] -> Py (Ptr PyObject)
  basicListToPy xs = evalContT $ do
    let n = fromIntegral $ length xs :: CLLong
    p_list <- checkNull (Py [CU.exp| PyObject* { PyList_New($(long long n)) } |])
    onExceptionProg $ decref p_list
    let loop !_ []     = pure p_list
        loop  i (a:as) = basicToPy a >>= \case
          NULL -> pure nullPtr
          p_a  -> do
            liftIO [CU.exp| void { PyList_SET_ITEM($(PyObject* p_list), $(long long i), $(PyObject* p_a)) } |]
            loop (i+1) as
    lift $ loop 0 xs

-- | Convert python object to haskell value.
class FromPy a where
  -- | Convert python value into haskell value. This function should
  --   try to not modify python's data. This function should avoid
  --   throwing haskell exception. Any python exceptions should be
  --   thrown as 'PyError'. When data type couldn't be converted
  --   'FromPyFailed' should be thrown to indicate failure.
  --
  --   This is low level function. It should be only used when working
  --   with python's C API. Otherwise 'fromPy' is preferred.
  basicFromPy :: Ptr PyObject -> Py a

-- | Convert python object to haskell value
fromPyEither :: FromPy a => PyObject -> IO (Either PyError a)
fromPyEither py = runPy $ unsafeWithPyObject py $ \p ->
  (Right <$> basicFromPy p) `catchPy` (pure . Left)


-- | Convert python object to haskell value. Python exception raised
--   during execution are thrown as exceptions
fromPy :: FromPy a => PyObject -> IO (Maybe a)
fromPy py = runPy $ unsafeWithPyObject py $ \p ->
  (Just <$> basicFromPy p) `catchPy` \case
    FromPyFailed -> pure Nothing
    e            -> throwPy e

-- | Convert python object to haskell value. Throws exception on failure
fromPy' :: FromPy a => PyObject -> IO a
fromPy' py = runPy $ unsafeWithPyObject py basicFromPy

-- | Convert haskell value to a python object.
toPy :: ToPy a => a -> IO PyObject
toPy a = runPy $ basicToPy a >>= \case
  NULL -> throwPy =<< convertPy2Haskell
  p    -> newPyObject p

instance ToPy CLong where
  basicToPy i = Py [CU.exp| PyObject* { PyLong_FromLong($(long i)) } |]
instance FromPy CLong where
  basicFromPy p_py = do
    r <- Py [CU.exp| long { PyLong_AsLong($(PyObject *p_py)) } |]
    r <$ throwPyConvesionFailed

instance ToPy CLLong where
  basicToPy i = Py [CU.exp| PyObject* { PyLong_FromLongLong($(long long i)) } |]
instance FromPy CLLong where
  basicFromPy p_py = do
    r <- Py [CU.exp| long long { PyLong_AsLongLong($(PyObject *p_py)) } |]
    r <$ throwPyConvesionFailed

instance ToPy CULong where
  basicToPy i = Py [CU.exp| PyObject* { PyLong_FromUnsignedLong($(unsigned long i)) } |]
instance FromPy CULong where
  basicFromPy p_py = do
    r <- Py [CU.exp| unsigned long { PyLong_AsUnsignedLong($(PyObject *p_py)) } |]
    r <$ throwPyConvesionFailed

instance ToPy CULLong where
  basicToPy i = Py [CU.exp| PyObject* { PyLong_FromUnsignedLongLong($(unsigned long long i)) } |]
instance FromPy CULLong where
  basicFromPy p_py = do
    r <- Py [CU.exp| unsigned long long { PyLong_AsUnsignedLongLong($(PyObject *p_py)) } |]
    r <$ throwPyConvesionFailed


instance ToPy CDouble where
  basicToPy i = Py [CU.exp| PyObject* { PyFloat_FromDouble($(double i)) } |]
instance FromPy CDouble where
  basicFromPy p_py = do
    r <- Py [CU.exp| double { PyFloat_AsDouble($(PyObject *p_py)) } |]
    r <$ throwPyConvesionFailed

deriving via CLLong  instance ToPy   Int64
deriving via CLLong  instance FromPy Int64
deriving via CULLong instance ToPy   Word64
deriving via CULLong instance FromPy Word64
deriving via CDouble instance ToPy   Double
deriving via CDouble instance FromPy Double

instance ToPy Int where
  basicToPy   = basicToPy @Int64 . fromIntegral
instance FromPy Int where
  basicFromPy = fmap fromIntegral . basicFromPy @Int64

-- -- TODO: Int may be 32 or 64 bit!
-- -- TODO: Int{8,16,32} & Word{8,16,32}

-- | Encoded as 1-character string
instance ToPy Char where
  basicToPy c = do
    let i = fromIntegral (ord c) :: CUInt
    Py [CU.block| PyObject* {
       uint32_t cs[1] = { $(unsigned i) };
       return PyUnicode_DecodeUTF32((char*)cs, 4, NULL, NULL);
       } |]
  basicListToPy str = evalContT $ do
    p_str <- withPyWCString str
    liftIO [CU.exp| PyObject* { PyUnicode_FromWideChar($(wchar_t *p_str), -1) } |]


instance FromPy Char where
  basicFromPy p = do
    r <- Py [CU.block| int {
      PyObject* p = $(PyObject *p);
      if( !PyUnicode_Check(p) )
          return -1;
      if( 1 != PyUnicode_GET_LENGTH(p) )
          return -1;
      switch( PyUnicode_KIND(p) ) {
      case PyUnicode_1BYTE_KIND:
          return PyUnicode_1BYTE_DATA(p)[0];
      case PyUnicode_2BYTE_KIND:
          return PyUnicode_2BYTE_DATA(p)[0];
      case PyUnicode_4BYTE_KIND:
          return PyUnicode_4BYTE_DATA(p)[0];
      }
      return -1;
      } |]
    if | r < 0     -> throwPy FromPyFailed
       | otherwise -> pure $ chr $ fromIntegral r

instance ToPy Bool where
  basicToPy True  = Py [CU.exp| PyObject* { Py_True  } |]
  basicToPy False = Py [CU.exp| PyObject* { Py_False } |]

-- | Uses python's truthiness conventions
instance FromPy Bool where
  basicFromPy p = do
    r <- Py [CU.exp| int { PyObject_IsTrue($(PyObject* p)) } |]
    throwPyError
    pure $! r /= 0


instance (ToPy a, ToPy b) => ToPy (a,b) where
  basicToPy (a,b) = evalContT $ do
    p_a <- takeOwnership =<< checkNull (basicToPy a)
    p_b <- takeOwnership =<< checkNull (basicToPy b)
    liftIO [CU.exp| PyObject* { PyTuple_Pack(2, $(PyObject* p_a), $(PyObject* p_b)) } |]

instance (FromPy a, FromPy b) => FromPy (a,b) where
  basicFromPy p_tup = evalContT $ do
    -- Unpack 2-tuple.
    p_args    <- withPyAllocaArray 2
    unpack_ok <- liftIO [CU.exp| int {
      inline_py_unpack_iterable($(PyObject *p_tup), 2, $(PyObject **p_args))
      }|]
    lift $ do throwPyError
              when (unpack_ok /= 0) $ throwPy FromPyFailed
    -- Parse each element of tuple
    p_a <- liftIO $ peekElemOff p_args 0
    p_b <- liftIO $ peekElemOff p_args 1
    finallyProg $ decref p_a >> decref p_b
    lift $ do a <- basicFromPy p_a
              b <- basicFromPy p_b
              pure (a,b)

instance (ToPy a) => ToPy [a] where
  basicToPy = basicListToPy

instance (FromPy a) => FromPy [a] where
  basicFromPy p_list = do
    p_iter <- Py [CU.block| PyObject* {
      PyObject* iter = PyObject_GetIter( $(PyObject *p_list) );
      if( PyErr_Occurred() ) {
          PyErr_Clear();
      }
      return iter;
      } |]
    when (nullPtr == p_iter) $ throwPy FromPyFailed
    --
    let loop f = do
          p <- Py [C.exp| PyObject* { PyIter_Next($(PyObject* p_iter)) } |]
          throwPyError
          case p of
            NULL -> pure f
            _    -> do a <- basicFromPy p `finallyPy` decref p
                       loop (f . (a:))
    ($ []) <$> loop id


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
-- to CFunction as self. It does seems hacky. However it does the trick.
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


instance (FromPy a, Show a, ToPy b) => ToPy (a -> IO b) where
  basicToPy f = Py $ do
    -- C function pointer for callback
    f_ptr <- wrapO $ \_ p_a -> pyCallback $ do
      a <- lift (tryPy (basicFromPy p_a)) >>= \case
        Left  FromPyFailed -> abortM $ raiseUndecodedArg 1 1
        Left  e            -> lift   $ throwPy e
        Right a            -> pure a
      liftIO $ unPy . basicToPy =<< f a
    --
    [CU.exp| PyObject* { inline_py_callback_METH_O($(PyCFunction f_ptr)) } |]


instance (FromPy a1, FromPy a2, ToPy b) => ToPy (a1 -> a2 -> IO b) where
  basicToPy f = Py $ do
    -- Create haskell function
    f_ptr <- wrapFastcall $ \_ p_arr n -> pyCallback $ do
      when (n /= 2) $ abortM $ raiseBadNArgs 2 n
      a <- loadArgFastcall p_arr 0 n
      b <- loadArgFastcall p_arr 1 n
      liftIO $ unPy . basicToPy =<< f a b
    -- Create python function
    [C.exp| PyObject* { inline_py_callback_METH_FASTCALL($(PyCFunctionFast f_ptr)) } |]

----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------


-- | Execute haskell callback function
pyCallback :: Program (Ptr PyObject) (Ptr PyObject) -> IO (Ptr PyObject)
pyCallback io = unPy $ ensureGIL $ evalContT io `catchPy` convertHaskell2Py

loadArgFastcall :: FromPy a => Ptr (Ptr PyObject) -> Int -> Int64 -> Program (Ptr PyObject) a
loadArgFastcall p_arr i tot = do
  p <- liftIO $ peekElemOff p_arr i
  lift (tryPy (basicFromPy p)) >>= \case
    Right a            -> pure a
    Left  FromPyFailed -> abortM $ raiseUndecodedArg (fromIntegral i + 1) (fromIntegral tot)
    Left  e            -> lift   $ throwPy e

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



type FunWrapper a = a -> IO (FunPtr a)

foreign import ccall "wrapper" wrapO
  :: FunWrapper (Ptr PyObject -> Ptr PyObject -> IO (Ptr PyObject))

foreign import ccall "wrapper" wrapFastcall
  :: FunWrapper (Ptr PyObject -> Ptr (Ptr PyObject) -> Int64 -> IO (Ptr PyObject))
