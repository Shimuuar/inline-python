{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
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

import Control.Exception           (evaluate)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans.Cont
import Data.Bits
import Data.Char
import Data.Int
import Data.Word
import Data.ByteString             qualified as BS
import Data.ByteString.Unsafe      qualified as BS
import Data.ByteString.Short       qualified as SBS
import Data.ByteString.Lazy        qualified as BL
import Data.Set                    qualified as Set
import Data.Map.Strict             qualified as Map
import Data.Text                   qualified as T
import Data.Text.Encoding          qualified as T
import Data.Text.Lazy              qualified as TL
import Data.Vector.Generic         qualified as VG
import Data.Vector.Generic.Mutable qualified as MVG
import Data.Vector                 qualified as V
#if MIN_VERSION_vector(0,13,2)
import Data.Vector.Strict          qualified as VV
#endif
import Data.Vector.Storable        qualified as VS
import Data.Vector.Primitive       qualified as VP
import Data.Vector.Unboxed         qualified as VU
import Foreign.Ptr
import Foreign.C.Types
import Foreign.Storable
import Foreign.Marshal.Alloc     (alloca,mallocBytes)
import Foreign.Marshal.Utils     (copyBytes)
import GHC.Float                 (float2Double, double2Float)
import Data.Complex              (Complex((:+)))

import Language.C.Inline         qualified as C
import Language.C.Inline.Unsafe  qualified as CU

import Python.Internal.Types
import Python.Internal.Eval
import Python.Internal.CAPI
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
  basicListToPy xs = runProgram $ do
    let n = fromIntegral $ length xs :: CLLong
    p_list <- takeOwnership =<< checkNull (Py [CU.exp| PyObject* { PyList_New($(long long n)) } |])
    let loop !_ []     = p_list <$ incref p_list
        loop  i (a:as) = basicToPy a >>= \case
          NULL -> pure nullPtr
          p_a  -> do
            -- NOTE: PyList_SET_ITEM steals reference
            Py [CU.exp| void { PyList_SET_ITEM($(PyObject* p_list), $(long long i), $(PyObject* p_a)) } |]
            loop (i+1) as
    progPy $ loop 0 xs

-- | Convert python object to haskell value.
class FromPy a where
  -- | Convert python value into haskell value. This function should
  --   try to not modify python's data. This function should avoid
  --   throwing haskell exception. Any python exceptions should be
  --   thrown as 'PyError'. When data type couldn't be converted
  --   'BadPyType' or 'OutOfRange' should be thrown to indicate failure.
  --
  --   This is low level function. It should be only used when working
  --   with python's C API. Otherwise 'fromPy' is preferred.
  basicFromPy :: Ptr PyObject -> Py a

-- | Convert python object to haskell value. All python exceptions
--   which happen during execution will be converted to @PyError@.
fromPyEither :: FromPy a => PyObject -> Py (Either PyError a)
fromPyEither py = unsafeWithPyObject py $ \p ->
  (Right <$> basicFromPy p) `catch` (pure . Left)


-- | Convert python object to haskell value. Will return @Nothing@ if
--   'BadPyType' or 'OutOfRange' is thrown. Other python exceptions
--   are rethrown.
fromPy :: FromPy a => PyObject -> Py (Maybe a)
fromPy py = unsafeWithPyObject py $ \p ->
  (Just <$> basicFromPy p) `catch` \case
    BadPyType  -> pure Nothing
    OutOfRange -> pure Nothing
    e          -> throwM e

-- | Convert python object to haskell value. Throws exception on
--   failure.
fromPy' :: FromPy a => PyObject -> Py a
fromPy' py = unsafeWithPyObject py basicFromPy

-- | Convert haskell value to a python object.
toPy :: ToPy a => a -> Py PyObject
toPy a = basicToPy a >>= \case
  NULL -> mustThrowPyError
  p    -> newPyObject p


----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

instance ToPy PyObject where
  basicToPy o = unsafeWithPyObject o $ \p -> p <$ incref p
instance FromPy PyObject where
  basicFromPy p = incref p >> newPyObject p

deriving newtype instance ToPy   Module
deriving newtype instance FromPy Module
deriving newtype instance ToPy   Dict
deriving newtype instance FromPy Dict

instance ToPy () where
  basicToPy () = Py [CU.exp| PyObject* { Py_None } |]

instance ToPy CLong where
  basicToPy i = Py [CU.exp| PyObject* { PyLong_FromLong($(long i)) } |]
instance FromPy CLong where
  basicFromPy p_py = do
    r <- Py [CU.exp| long { PyLong_AsLong($(PyObject *p_py)) } |]
    r <$ checkThrowBadPyType

instance ToPy CLLong where
  basicToPy i = Py [CU.exp| PyObject* { PyLong_FromLongLong($(long long i)) } |]
instance FromPy CLLong where
  basicFromPy p_py = do
    r <- Py [CU.exp| long long { PyLong_AsLongLong($(PyObject *p_py)) } |]
    r <$ checkThrowBadPyType

instance ToPy CULong where
  basicToPy i = Py [CU.exp| PyObject* { PyLong_FromUnsignedLong($(unsigned long i)) } |]
instance FromPy CULong where
  basicFromPy p_py = do
    r <- Py [CU.exp| unsigned long { PyLong_AsUnsignedLong($(PyObject *p_py)) } |]
    r <$ checkThrowBadPyType

instance ToPy CULLong where
  basicToPy i = Py [CU.exp| PyObject* { PyLong_FromUnsignedLongLong($(unsigned long long i)) } |]
instance FromPy CULLong where
  basicFromPy p_py = do
    r <- Py [CU.exp| unsigned long long { PyLong_AsUnsignedLongLong($(PyObject *p_py)) } |]
    r <$ checkThrowBadPyType

instance ToPy CDouble where
  basicToPy i = Py [CU.exp| PyObject* { PyFloat_FromDouble($(double i)) } |]
instance FromPy CDouble where
  basicFromPy p_py = do
    r <- Py [CU.exp| double { PyFloat_AsDouble($(PyObject *p_py)) } |]
    r <$ checkThrowBadPyType

deriving via CLLong  instance ToPy   Int64
deriving via CLLong  instance FromPy Int64
deriving via CULLong instance ToPy   Word64
deriving via CULLong instance FromPy Word64

deriving newtype instance ToPy   CInt
deriving newtype instance FromPy CInt
deriving newtype instance ToPy   CUInt
deriving newtype instance FromPy CUInt
deriving newtype instance ToPy   CShort
deriving newtype instance FromPy CShort
deriving newtype instance ToPy   CUShort
deriving newtype instance FromPy CUShort
deriving newtype instance ToPy   CChar
deriving newtype instance FromPy CChar
deriving newtype instance ToPy   CUChar
deriving newtype instance FromPy CUChar
deriving newtype instance ToPy   CSChar
deriving newtype instance FromPy CSChar

deriving via CDouble instance ToPy   Double
deriving via CDouble instance FromPy Double

instance ToPy   Float where basicToPy   = basicToPy . float2Double
instance FromPy Float where basicFromPy = fmap double2Float . basicFromPy

-- | @since 0.2
instance ToPy (Complex Float) where
  basicToPy (x:+y) = basicToPy $ float2Double x :+ float2Double y
-- | @since 0.2
instance FromPy (Complex Float) where
  basicFromPy xy_py = do
     x :+ y <- basicFromPy xy_py
     return $ double2Float x :+ double2Float y

-- | @since 0.2
instance ToPy (Complex Double) where
  basicToPy (x:+y) = Py [CU.exp| PyObject* { PyComplex_FromDoubles($(double x'), $(double y')) } |]
   where x' = CDouble x
         y' = CDouble y
-- | @since 0.2
instance FromPy (Complex Double) where
  basicFromPy xy_py = do
    CDouble x <- Py [CU.exp| double { PyComplex_RealAsDouble($(PyObject *xy_py)) } |]
    checkThrowBadPyType
    CDouble y <- Py [CU.exp| double { PyComplex_ImagAsDouble($(PyObject *xy_py)) } |]
    checkThrowBadPyType
    return $ x :+ y

instance ToPy Int where
  basicToPy
    | wordSizeInBits == 64 = basicToPy @Int64 . fromIntegral
    | otherwise            = basicToPy @Int32 . fromIntegral
instance FromPy Int where
  basicFromPy
    | wordSizeInBits == 64 = fmap fromIntegral . basicFromPy @Int64
    | otherwise            = fmap fromIntegral . basicFromPy @Int32

instance ToPy Word where
  basicToPy
    | wordSizeInBits == 64 = basicToPy @Word64 . fromIntegral
    | otherwise            = basicToPy @Word32 . fromIntegral
instance FromPy Word where
  basicFromPy
    | wordSizeInBits == 64 = fmap fromIntegral . basicFromPy @Word64
    | otherwise            = fmap fromIntegral . basicFromPy @Word32

instance ToPy Int8   where basicToPy = basicToPy @Int64  . fromIntegral
instance ToPy Int16  where basicToPy = basicToPy @Int64  . fromIntegral
instance ToPy Int32  where basicToPy = basicToPy @Int64  . fromIntegral
instance ToPy Word8  where basicToPy = basicToPy @Word64 . fromIntegral
instance ToPy Word16 where basicToPy = basicToPy @Word64 . fromIntegral
instance ToPy Word32 where basicToPy = basicToPy @Word64 . fromIntegral

instance FromPy Int8 where
  basicFromPy p = basicFromPy @Int64 p >>= \case
    i | i <= fromIntegral (maxBound :: Int8)
      , i >= fromIntegral (minBound :: Int8) -> pure $! fromIntegral i
      | otherwise -> throwM OutOfRange

instance FromPy Int16 where
  basicFromPy p = basicFromPy @Int64 p >>= \case
    i | i <= fromIntegral (maxBound :: Int16)
      , i >= fromIntegral (minBound :: Int16) -> pure $! fromIntegral i
      | otherwise -> throwM OutOfRange

instance FromPy Int32 where
  basicFromPy p = basicFromPy @Int64 p >>= \case
    i | i <= fromIntegral (maxBound :: Int32)
      , i >= fromIntegral (minBound :: Int32) -> pure $! fromIntegral i
      | otherwise -> throwM OutOfRange

instance FromPy Word8 where
  basicFromPy p = basicFromPy @Word64 p >>= \case
    i | i <= fromIntegral (maxBound :: Word8) -> pure $! fromIntegral i
      | otherwise -> throwM OutOfRange

instance FromPy Word16 where
  basicFromPy p = basicFromPy @Word64 p >>= \case
    i | i <= fromIntegral (maxBound :: Word16) -> pure $! fromIntegral i
      | otherwise -> throwM OutOfRange

instance FromPy Word32 where
  basicFromPy p = basicFromPy @Word64 p >>= \case
    i | i <= fromIntegral (maxBound :: Word32) -> pure $! fromIntegral i
      | otherwise -> throwM OutOfRange


-- | Encoded as 1-character string
instance ToPy Char where
  basicToPy c = do
    let i = fromIntegral (ord c) :: CUInt
    Py [CU.block| PyObject* {
       uint32_t cs[1] = { $(unsigned i) };
       return PyUnicode_DecodeUTF32((char*)cs, 4, NULL, NULL);
       } |]
  basicListToPy str = runProgram $ do
    p_str <- withPyWCString str
    progIO [CU.exp| PyObject* { PyUnicode_FromWideChar($(wchar_t *p_str), -1) } |]


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
    if | r < 0     -> throwM BadPyType
       | otherwise -> pure $ chr $ fromIntegral r

instance ToPy Bool where
  basicToPy True  = Py [CU.exp| PyObject* { Py_True  } |]
  basicToPy False = Py [CU.exp| PyObject* { Py_False } |]

-- | Uses python's truthiness conventions
instance FromPy Bool where
  basicFromPy p = do
    r <- Py [CU.exp| int { PyObject_IsTrue($(PyObject* p)) } |]
    checkThrowPyError
    pure $! r /= 0


instance (ToPy a, ToPy b) => ToPy (a,b) where
  basicToPy (a,b) = runProgram $ do
    p_a <- takeOwnership =<< checkNull (basicToPy a)
    p_b <- takeOwnership =<< checkNull (basicToPy b)
    progIO [CU.exp| PyObject* { PyTuple_Pack(2, $(PyObject* p_a), $(PyObject* p_b)) } |]

-- | Will accept any iterable
instance (FromPy a, FromPy b) => FromPy (a,b) where
  basicFromPy p_tup = runProgram $ do
    -- Unpack 2-tuple.
    p_args    <- withPyAllocaArray 2
    unpack_ok <- progIO [CU.exp| int {
      inline_py_unpack_iterable($(PyObject *p_tup), 2, $(PyObject **p_args))
      }|]
    progPy $ do checkThrowPyError
                when (unpack_ok /= 0) $ throwM BadPyType
    -- Parse each element of tuple
    p_a <- takeOwnership =<< progIO (peekElemOff p_args 0)
    p_b <- takeOwnership =<< progIO (peekElemOff p_args 1)
    progPy $ do a <- basicFromPy p_a
                b <- basicFromPy p_b
                pure (a,b)

instance (ToPy a, ToPy b, ToPy c) => ToPy (a,b,c) where
  basicToPy (a,b,c) = runProgram $ do
    p_a <- takeOwnership =<< checkNull (basicToPy a)
    p_b <- takeOwnership =<< checkNull (basicToPy b)
    p_c <- takeOwnership =<< checkNull (basicToPy c)
    progIO [CU.exp| PyObject* {
      PyTuple_Pack(3, $(PyObject *p_a), $(PyObject *p_b), $(PyObject *p_c)) } |]

-- | Will accept any iterable
instance (FromPy a, FromPy b, FromPy c) => FromPy (a,b,c) where
  basicFromPy p_tup = runProgram $ do
    -- Unpack 3-tuple.
    p_args    <- withPyAllocaArray 3
    unpack_ok <- progIO [CU.exp| int {
      inline_py_unpack_iterable($(PyObject *p_tup), 3, $(PyObject **p_args))
      }|]
    progPy $ do checkThrowPyError
                when (unpack_ok /= 0) $ throwM BadPyType
    -- Parse each element of tuple
    p_a <- takeOwnership =<< progIO (peekElemOff p_args 0)
    p_b <- takeOwnership =<< progIO (peekElemOff p_args 1)
    p_c <- takeOwnership =<< progIO (peekElemOff p_args 2)
    progPy $ do a <- basicFromPy p_a
                b <- basicFromPy p_b
                c <- basicFromPy p_c
                pure (a,b,c)

instance (ToPy a, ToPy b, ToPy c, ToPy d) => ToPy (a,b,c,d) where
  basicToPy (a,b,c,d) = runProgram $ do
    p_a <- takeOwnership =<< checkNull (basicToPy a)
    p_b <- takeOwnership =<< checkNull (basicToPy b)
    p_c <- takeOwnership =<< checkNull (basicToPy c)
    p_d <- takeOwnership =<< checkNull (basicToPy d)
    progIO [CU.exp| PyObject* {
      PyTuple_Pack(4, $(PyObject *p_a), $(PyObject *p_b), $(PyObject *p_c), $(PyObject *p_d)) } |]

-- | Will accept any iterable
instance (FromPy a, FromPy b, FromPy c, FromPy d) => FromPy (a,b,c,d) where
  basicFromPy p_tup = runProgram $ do
    -- Unpack 3-tuple.
    p_args    <- withPyAllocaArray 4
    unpack_ok <- progIO [CU.exp| int {
      inline_py_unpack_iterable($(PyObject *p_tup), 4, $(PyObject **p_args))
      }|]
    progPy $ do checkThrowPyError
                when (unpack_ok /= 0) $ throwM BadPyType
    -- Parse each element of tuple
    p_a <- takeOwnership =<< progIO (peekElemOff p_args 0)
    p_b <- takeOwnership =<< progIO (peekElemOff p_args 1)
    p_c <- takeOwnership =<< progIO (peekElemOff p_args 2)
    p_d <- takeOwnership =<< progIO (peekElemOff p_args 3)
    progPy $ do a <- basicFromPy p_a
                b <- basicFromPy p_b
                c <- basicFromPy p_c
                d <- basicFromPy p_d
                pure (a,b,c,d)


-- | @Nothing@ is encoded as @None@. @Just a@ same as @a@.
--
-- @since 0.2
instance (ToPy a) => ToPy (Maybe a) where
  basicToPy Nothing  = Py [CU.exp| PyObject* { Py_None } |]
  basicToPy (Just a) = basicToPy a

-- | @None@ is decoded as @Nothing@ rest is attempted to be decoded as @a@
--
-- @since 0.2
instance (FromPy a) => FromPy (Maybe a) where
  basicFromPy p =
    Py [CU.exp| bool { Py_None == $(PyObject *p) } |] >>= \case
      0 -> Just <$> basicFromPy p
      _ -> pure Nothing


instance (ToPy a) => ToPy [a] where
  basicToPy = basicListToPy

-- | Will accept any iterable
instance (FromPy a) => FromPy [a] where
  basicFromPy p_list = do
    p_iter <- Py [CU.block| PyObject* {
      PyObject* iter = PyObject_GetIter( $(PyObject *p_list) );
      if( PyErr_Occurred() ) {
          PyErr_Clear();
      }
      return iter;
      } |]
    when (nullPtr == p_iter) $ throwM BadPyType
    --
    f <- foldPyIterable p_iter
      (\f p -> do a <- basicFromPy p
                  pure (f . (a:)))
      id
    pure $ f []

instance (ToPy a, Ord a) => ToPy (Set.Set a) where
  basicToPy set = runProgram $ do
    p_set <- takeOwnership =<< checkNull basicNewSet
    progPy $ do
      let loop []     = p_set <$ incref p_set
          loop (x:xs) = basicToPy x >>= \case
            NULL -> pure NULL
            p_a  -> Py [C.exp| int { PySet_Add($(PyObject *p_set), $(PyObject *p_a)) }|] >>= \case
              0 -> decref p_a >> loop xs
              _ -> mustThrowPyError
      loop $ Set.toList set

instance (FromPy a, Ord a) => FromPy (Set.Set a) where
  basicFromPy p_set = basicGetIter p_set >>= \case
    NULL -> do Py [C.exp| void { PyErr_Clear() } |]
               throwM BadPyType
    p_iter -> foldPyIterable p_iter
      (\s p -> do a <- basicFromPy p
                  pure $! Set.insert a s)
      Set.empty


instance (ToPy k, ToPy v, Ord k) => ToPy (Map.Map k v) where
  basicToPy dct = runProgram $ do
    p_dict <- takeOwnership =<< checkNull basicNewDict
    progPy $ do
      let loop []         = p_dict <$ incref p_dict
          loop ((k,v):xs) = basicToPy k >>= \case
            NULL -> mustThrowPyError
            p_k  -> flip finally (decref p_k) $ basicToPy v >>= \case
              NULL -> mustThrowPyError
              p_v  -> Py [CU.exp| int { PyDict_SetItem($(PyObject *p_dict), $(PyObject* p_k), $(PyObject *p_v)) }|] >>= \case
                0 -> loop xs
                _ -> nullPtr <$ decref p_v
      loop $ Map.toList dct

instance (FromPy k, FromPy v, Ord k) => FromPy (Map.Map k v) where
  basicFromPy p_dct = basicGetIter p_dct >>= \case
    NULL   -> do Py [C.exp| void { PyErr_Clear() } |]
                 throwM BadPyType
    p_iter -> foldPyIterable p_iter
      (\m p -> do k <- basicFromPy p
                  v <- Py [CU.exp| PyObject* { PyDict_GetItem($(PyObject* p_dct), $(PyObject *p)) }|] >>= \case
                    NULL -> throwM BadPyType
                    p_v  -> basicFromPy p_v
                  pure $! Map.insert k v m)
      Map.empty

-- | Converts to python's list
instance ToPy a => ToPy (V.Vector a) where
  basicToPy = vectorToPy
-- | Converts to python's list
instance (ToPy a, VS.Storable a) => ToPy (VS.Vector a) where
  basicToPy = vectorToPy
-- | Converts to python's list
instance (ToPy a, VP.Prim a) => ToPy (VP.Vector a) where
  basicToPy = vectorToPy
-- | Converts to python's list
instance (ToPy a, VU.Unbox a) => ToPy (VU.Vector a) where
  basicToPy = vectorToPy
#if MIN_VERSION_vector(0,13,2)
-- | Converts to python's list
instance (ToPy a) => ToPy (VV.Vector a) where
  basicToPy = vectorToPy
#endif

-- | Accepts python's sequence (@len@ and indexing)
instance FromPy a => FromPy (V.Vector a) where
  basicFromPy = vectorFromPy
-- | Accepts python's sequence (@len@ and indexing)
instance (FromPy a, VS.Storable a) => FromPy (VS.Vector a) where
  basicFromPy = vectorFromPy
-- | Accepts python's sequence (@len@ and indexing)
instance (FromPy a, VP.Prim a) => FromPy (VP.Vector a) where
  basicFromPy = vectorFromPy
-- | Accepts python's sequence (@len@ and indexing)
instance (FromPy a, VU.Unbox a) => FromPy (VU.Vector a) where
  basicFromPy = vectorFromPy
#if MIN_VERSION_vector(0,13,2)
-- | Accepts python's sequence (@len@ and indexing)
instance FromPy a => FromPy (VV.Vector a) where
  basicFromPy = vectorFromPy
#endif


-- | Fold over python's iterator. Function takes ownership over iterator.
foldPyIterable
  :: Ptr PyObject                -- ^ Python iterator (not checked)
  -> (a -> Ptr PyObject -> Py a) -- ^ Step function. It takes borrowed pointer.
  -> a                           -- ^ Initial value
  -> Py a
foldPyIterable p_iter step a0
  = loop a0 `finally` decref p_iter
  where
    loop a = basicIterNext p_iter >>= \case
      NULL -> a <$ checkThrowPyError
      p    -> loop =<< (step a p `finally` decref p)


vectorFromPy :: (VG.Vector v a, FromPy a) => Ptr PyObject -> Py (v a)
{-# INLINE vectorFromPy #-}
vectorFromPy p_seq = do
  len <- Py [CU.exp| long long { PySequence_Size($(PyObject* p_seq)) } |]
  when (len < 0) $ do
    Py [C.exp| void { PyErr_Clear() } |]
    throwM BadPyType
  -- Read data into vector
  buf <- MVG.generateM (fromIntegral len) $ \i -> do
    let i_c = fromIntegral i
    Py [CU.exp| PyObject* { PySequence_GetItem($(PyObject* p_seq), $(long long i_c)) } |] >>= \case
      NULL -> mustThrowPyError
      p    -> basicFromPy p `finally` decref p
  VG.unsafeFreeze buf

vectorToPy :: (VG.Vector v a, ToPy a) => v a -> Py (Ptr PyObject)
vectorToPy vec = runProgram $ do
  p_list <- takeOwnership =<< checkNull (Py [CU.exp| PyObject* { PyList_New($(long long n_c)) } |])
  progPy $ do
    let loop i
          | i >= n    = p_list <$ incref p_list
          | otherwise = basicToPy (VG.unsafeIndex vec i) >>= \case
              NULL -> pure nullPtr
              p_a  -> do
                let i_c = fromIntegral i :: CLLong
                -- NOTE: PyList_SET_ITEM steals reference
                Py [CU.exp| void { PyList_SET_ITEM($(PyObject* p_list), $(long long i_c), $(PyObject* p_a)) } |]
                loop (i+1)
    loop 0
  where
    n   = VG.length vec
    n_c = fromIntegral n :: CLLong


-- | Converted to @bytes@
--
--   @since 0.2
instance ToPy BS.ByteString where
  basicToPy bs = pyIO $ BS.unsafeUseAsCStringLen bs $ \(ptr,len) -> do
    let c_len = fromIntegral len :: CLLong
    py <- [CU.exp| PyObject* { PyBytes_FromStringAndSize($(char* ptr), $(long long c_len)) }|]
    case py of
      NULL -> unsafeRunPy mustThrowPyError
      _    -> return py

-- | Accepts @bytes@ and @bytearray@
--
--   @since 0.2
instance FromPy BS.ByteString where
  basicFromPy py = pyIO $ do
    [CU.exp| int { PyBytes_Check($(PyObject* py)) } |] >>= \case
      TRUE -> do
        sz  <- [CU.exp| int64_t { PyBytes_GET_SIZE( $(PyObject* py)) } |]
        buf <- [CU.exp| char*   { PyBytes_AS_STRING($(PyObject* py)) } |]
        fini buf (fromIntegral sz)
      _ -> [CU.exp| int { PyByteArray_Check($(PyObject* py)) } |] >>= \case
        TRUE -> do
          sz  <- [CU.exp| int64_t { PyByteArray_GET_SIZE( $(PyObject* py)) } |]
          buf <- [CU.exp| char*   { PyByteArray_AS_STRING($(PyObject* py)) } |]
          fini buf (fromIntegral sz)
        _ -> throwM BadPyType
    where
      fini py_buf sz = do
        hs_buf <- mallocBytes sz
        copyBytes hs_buf py_buf sz
        BS.unsafePackMallocCStringLen (hs_buf, sz)

-- | Converted to @bytes@
--
--   @since 0.2
instance ToPy BL.ByteString where
  basicToPy = basicToPy . BL.toStrict

-- | Accepts @bytes@ and @bytearray@
--
--   @since 0.2
instance FromPy BL.ByteString where
  basicFromPy = fmap BL.fromStrict . basicFromPy


-- | Accepts @bytes@ and @bytearray@
--
--   @since 0.2
instance FromPy SBS.ShortByteString where
  basicFromPy py = pyIO $ do
    [CU.exp| int { PyBytes_Check($(PyObject* py)) } |] >>= \case
      TRUE -> do
        sz  <- [CU.exp| int64_t { PyBytes_GET_SIZE( $(PyObject* py)) } |]
        buf <- [CU.exp| char*   { PyBytes_AS_STRING($(PyObject* py)) } |]
        fini buf (fromIntegral sz)
      _ -> [CU.exp| int { PyByteArray_Check($(PyObject* py)) } |] >>= \case
        TRUE -> do
          sz  <- [CU.exp| int64_t { PyByteArray_GET_SIZE( $(PyObject* py)) } |]
          buf <- [CU.exp| char*   { PyByteArray_AS_STRING($(PyObject* py)) } |]
          fini buf (fromIntegral sz)
        _ -> throwM BadPyType
    where
      fini buf sz = do
        bs <- BS.unsafePackCStringLen (buf, sz)
        evaluate $ SBS.toShort bs

-- | Converted to @bytes@
--
--   @since 0.2
instance ToPy SBS.ShortByteString where
  basicToPy bs = pyIO $ SBS.useAsCStringLen bs $ \(ptr,len) -> do
    let c_len = fromIntegral len :: CLLong
    py <- [CU.exp| PyObject* { PyBytes_FromStringAndSize($(char* ptr), $(long long c_len)) }|]
    case py of
      NULL -> unsafeRunPy mustThrowPyError
      _    -> return py


-- | @since 0.2@.
instance ToPy T.Text where
  -- NOTE: Is there ore efficient way to access
  basicToPy str = pyIO $ BS.unsafeUseAsCStringLen bs $ \(ptr,len) -> do
    let c_len = fromIntegral len :: CLLong
    py <- [CU.exp| PyObject* { PyUnicode_FromStringAndSize($(char* ptr), $(long long c_len)) } |]
    case py of
      NULL -> unsafeRunPy mustThrowPyError
      _    -> pure py
    where
      bs = T.encodeUtf8 str

-- | @since 0.2@.
instance ToPy TL.Text where
  basicToPy = basicToPy . TL.toStrict

-- | @since 0.2@.
instance FromPy T.Text where
  basicFromPy py = pyIO $ do
    [CU.exp| int { PyUnicode_Check($(PyObject* py)) } |] >>= \case
      TRUE -> alloca $ \p_size -> do
        buf <- [CU.exp| const char* { PyUnicode_AsUTF8AndSize($(PyObject* py), $(long* p_size)) } |]
        sz  <- peek p_size
        bs  <- BS.unsafePackCStringLen (buf, fromIntegral sz)
        return $! T.decodeUtf8Lenient bs
      _ -> throwM BadPyType

-- | @since 0.2@.
instance FromPy TL.Text where
  basicFromPy = fmap TL.fromStrict . basicFromPy



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


-- | Converted to 0-ary function
instance (ToPy b) => ToPy (IO b) where
  basicToPy f = Py $ do
    --
    f_ptr <- wrapCFunction $ \_ _ -> pyCallback $ do
      progPy $ basicToPy =<< dropGIL f
    --
    [CU.exp| PyObject* { inline_py_callback_METH_NOARGS($(PyCFunction f_ptr)) } |]

-- | Only accepts positional parameters
instance (FromPy a, Show a, ToPy b) => ToPy (a -> IO b) where
  basicToPy f = Py $ do
    --
    f_ptr <- wrapCFunction $ \_ p_a -> pyCallback $ do
      a <- loadArg p_a 0 1
      progPy $ basicToPy =<< dropGIL (f a)
    --
    [CU.exp| PyObject* { inline_py_callback_METH_O($(PyCFunction f_ptr)) } |]

-- | Only accepts positional parameters
instance (FromPy a1, FromPy a2, ToPy b) => ToPy (a1 -> a2 -> IO b) where
  basicToPy f = Py $ do
    --
    f_ptr <- wrapFastcall $ \_ p_arr n -> pyCallback $ do
      when (n /= 2) $ abortM $ raiseBadNArgs 2 n
      a1 <- loadArgFastcall p_arr 0 n
      a2 <- loadArgFastcall p_arr 1 n
      progPy $ basicToPy =<< dropGIL (f a1 a2)
    --
    [CU.exp| PyObject* { inline_py_callback_METH_FASTCALL($(PyCFunctionFast f_ptr)) } |]


-- | Converted to 0-ary function
instance (ToPy b) => ToPy (Py b) where
  basicToPy f = Py $ do
    --
    f_ptr <- wrapCFunction $ \_ _ -> pyCallback $ do
      progPy $ basicToPy =<< f
    --
    [CU.exp| PyObject* { inline_py_callback_METH_NOARGS($(PyCFunction f_ptr)) } |]

-- | Only accepts positional parameters
instance (FromPy a, Show a, ToPy b) => ToPy (a -> Py b) where
  basicToPy f = Py $ do
    --
    f_ptr <- wrapCFunction $ \_ p_a -> pyCallback $ do
      a <- loadArg p_a 0 1
      progPy $ basicToPy =<< f a
    --
    [CU.exp| PyObject* { inline_py_callback_METH_O($(PyCFunction f_ptr)) } |]

-- | Only accepts positional parameters
instance (FromPy a1, FromPy a2, ToPy b) => ToPy (a1 -> a2 -> Py b) where
  basicToPy f = Py $ do
    --
    f_ptr <- wrapFastcall $ \_ p_arr n -> pyCallback $ do
      when (n /= 2) $ abortM $ raiseBadNArgs 2 n
      a1 <- loadArgFastcall p_arr 0 n
      a2 <- loadArgFastcall p_arr 1 n
      progPy $ basicToPy =<< f a1 a2
    --
    [CU.exp| PyObject* { inline_py_callback_METH_FASTCALL($(PyCFunctionFast f_ptr)) } |]



----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------


-- | Execute haskell callback function
pyCallback :: Program (Ptr PyObject) (Ptr PyObject) -> IO (Ptr PyObject)
pyCallback io = callbackEnsurePyLock $ unsafeRunPy $ ensureGIL $ runProgram io `catch` convertHaskell2Py

-- | Load argument from python object for haskell evaluation
loadArg
  :: FromPy a
  => (Ptr PyObject) -- ^ Python object to decode
  -> Int            -- ^ Argument number (0-based)
  -> Int64          -- ^ Total number of arguments
  -> Program (Ptr PyObject) a
loadArg p (fromIntegral -> i) (fromIntegral -> tot) = Program $ ContT $ \success -> do
  try (basicFromPy p) >>= \case
    Right a          -> success a
    Left  BadPyType  -> oops
    Left  OutOfRange -> oops
    Left  e          -> throwM e
    where
      oops = Py [CU.block| PyObject* {
        char err[256];
        sprintf(err, "Failed to decode function argument %i of %li", $(int i)+1, $(int64_t tot));
        PyErr_SetString(PyExc_TypeError, err);
        return NULL;
        } |]

-- | Load i-th argument from array as haskell parameter
loadArgFastcall
  :: FromPy a
  => Ptr (Ptr PyObject) -- ^ Array of arguments
  -> Int                -- ^ Argument number (0-based)
  -> Int64              -- ^ Total number of arguments
  -> Program (Ptr PyObject) a
loadArgFastcall p_arr i tot = do
  p <- progIO $ peekElemOff p_arr i
  loadArg p i tot

raiseBadNArgs :: CInt -> Int64 -> Py (Ptr PyObject)
raiseBadNArgs expected got = Py [CU.block| PyObject* {
  char err[256];
  sprintf(err, "Function takes exactly %i arguments (%li given)", $(int expected), $(int64_t got));
  PyErr_SetString(PyExc_TypeError, err);
  return NULL;
  } |]


type FunWrapper a = a -> IO (FunPtr a)

foreign import ccall "wrapper" wrapCFunction
  :: FunWrapper (Ptr PyObject -> Ptr PyObject -> IO (Ptr PyObject))

foreign import ccall "wrapper" wrapFastcall
  :: FunWrapper (Ptr PyObject -> Ptr (Ptr PyObject) -> Int64 -> IO (Ptr PyObject))


wordSizeInBits :: Int
wordSizeInBits = finiteBitSize (0 :: Word)
{-# INLINE wordSizeInBits #-}
