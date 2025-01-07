{-# LANGUAGE AllowAmbiguousTypes #-}
-- |
module TST.Roundtrip (tests) where

import Data.Int
import Data.Word
import Data.Typeable
import Foreign.C.Types

import Test.Tasty
import Test.Tasty.QuickCheck
import Python.Inline
import Python.Inline.QQ

tests :: TestTree
tests = testGroup "Roundtrip"
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
    -- Other scalars
  , testRoundtrip @Char
  , testRoundtrip @Bool
    -- Containers
  , testRoundtrip @(Int,Char)
  , testRoundtrip @(Int,(Int,Int))
  , testRoundtrip @(Int,Int,Int)
  , testRoundtrip @(Int,Int,Int,Char)
  , testRoundtrip @[Int]
  , testRoundtrip @[[Int]]
  -- , testRoundtrip @String -- Trips on zero byte as it should
  ]

testRoundtrip
  :: forall a. (FromPy a, ToPy a, Eq a, Arbitrary a, Show a, Typeable a) => TestTree
testRoundtrip = testProperty (show (typeOf (undefined :: a))) (propRoundtrip @a)

propRoundtrip :: forall a. (FromPy a, ToPy a, Eq a) => a -> Property
propRoundtrip a = ioProperty $ do
  a' <- fromPy' =<< [pye| a_hs |]
  pure $ a == a'
