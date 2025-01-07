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

