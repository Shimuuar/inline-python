{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
-- |
module TST.Roundtrip (tests) where

import Data.Int
import Data.Word
import Data.Typeable
import Data.Set        (Set)
import Data.Map.Strict (Map)
import Data.Text       qualified as T
import Data.Text.Lazy  qualified as TL
import Data.Complex (Complex)
import Foreign.C.Types

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Instances.Vector ()
import Test.QuickCheck.Instances.ByteString ()
import Test.QuickCheck.Instances.Text ()
import Python.Inline
import Python.Inline.QQ

import Data.ByteString             qualified as BS
import Data.ByteString.Lazy        qualified as BL
import Data.ByteString.Short       qualified as SBS
import Data.Vector                 qualified as V
#if MIN_VERSION_vector(0,13,2)
import Data.Vector.Strict          qualified as VV
#endif
import Data.Vector.Storable        qualified as VS
import Data.Vector.Primitive       qualified as VP
import Data.Vector.Unboxed         qualified as VU


tests :: TestTree
tests = testGroup "Roundtrip"
  [ testGroup "Roundtrip"
    [ -- Integral types
      testRoundtrip @Int8
    , testRoundtrip @Int16
    , testRoundtrip @Int32
    , testRoundtrip @Int64
    , testRoundtrip @Int
    , testRoundtrip @Word8
    , testRoundtrip @Word16
    , testRoundtrip @Word32
    , testRoundtrip @Word64
    , testRoundtrip @Word
      -- C wrappers
    , testRoundtrip @CChar
    , testRoundtrip @CSChar
    , testRoundtrip @CUChar
    , testRoundtrip @CShort
    , testRoundtrip @CUShort
    , testRoundtrip @CInt
    , testRoundtrip @CUInt
    , testRoundtrip @CLong
    , testRoundtrip @CULong
    , testRoundtrip @CLLong
    , testRoundtrip @CULLong
      -- Floating point
    , testRoundtrip @Double
    , testRoundtrip @Float
      -- Complex
    , testRoundtrip @(Complex Double)
    , testRoundtrip @(Complex Float)
      -- Other scalars
    , testRoundtrip @Char
    , testRoundtrip @Bool
      -- Containers
    , testRoundtrip @(Int,Char)
    , testRoundtrip @(Int,(Int,Int))
    , testRoundtrip @(Int,Int,Int)
    , testRoundtrip @(Int,Int,Int,Char)
    , testRoundtrip @(Maybe Int)
    , testRoundtrip @(Maybe T.Text)
    , testRoundtrip @[Int]
    , testRoundtrip @[[Int]]
    , testRoundtrip @[Complex Double]
    , testRoundtrip @(Set Int)
    , testRoundtrip @(Map Int Int)
    -- , testRoundtrip @String -- Trips on zero byte as it should
    , testRoundtrip @(V.Vector Int)
    , testRoundtrip @(VS.Vector Int)
    , testRoundtrip @(VP.Vector Int)
    , testRoundtrip @(VU.Vector Int)
#if MIN_VERSION_vector(0,13,2)
--    , testRoundtrip @(VV.Vector Int)
#endif
    , testRoundtrip @BS.ByteString
    , testRoundtrip @BL.ByteString
    , testRoundtrip @SBS.ShortByteString
    , testRoundtrip @T.Text
    , testRoundtrip @TL.Text
    ]
  , testGroup "OutOfRange"
    [ testOutOfRange @Int8   @Int16
    , testOutOfRange @Int16  @Int32
    , testOutOfRange @Int32  @Int64
    , testOutOfRange @Word8  @Word16
    , testOutOfRange @Word16 @Word32
    , testOutOfRange @Word32 @Word64
    ]
  ]

testRoundtrip
  :: forall a. (FromPy a, ToPy a, Eq a, Arbitrary a, Show a, Typeable a) => TestTree
testRoundtrip = testProperty (show (typeOf (undefined :: a))) (propRoundtrip @a)

testOutOfRange
  :: forall a wide. (ToPy wide, FromPy a, Eq a, Eq wide, Integral wide, Integral a
                    , Typeable a, Typeable wide, Arbitrary wide, Show wide
                    )
  => TestTree
testOutOfRange = testProperty
  (show (typeOf (undefined :: a)) ++ " [" ++ show (typeOf (undefined::wide)) ++ "]")
  (propOutOfRange @a @wide)

propRoundtrip :: forall a. (FromPy a, ToPy a, Eq a) => a -> Property
propRoundtrip a = ioProperty $ do
  a' <- runPy $ fromPy' =<< [pye| a_hs |]
  pure $ a == a'


-- Check that values out of range produce out of range
propOutOfRange
  :: forall a wide. (ToPy wide, FromPy a, Eq a, Eq wide, Integral wide, Integral a)
  => wide -> Property
propOutOfRange wide = ioProperty $ do
  a_py <- runPy $ fromPy @a =<< [pye| wide_hs |]
  pure $ a_hs == a_py
  where
    -- Convert taking range into account
    a_hs = case fromIntegral wide :: a of
      a' | fromIntegral a' == wide -> Just a'
         | otherwise               -> Nothing
