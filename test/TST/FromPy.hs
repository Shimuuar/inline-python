{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings   #-}
-- |
module TST.FromPy (tests) where

import Data.ByteString qualified as BS
import Control.Monad.IO.Class
import Test.Tasty
import Test.Tasty.HUnit
import Python.Inline
import Python.Inline.QQ
import Data.Complex (Complex((:+)))

import TST.Util


tests :: TestTree
tests = testGroup "FromPy"
  [ testGroup "Int"
    [ testCase "Int->Int"    $ eq @Int (Just 1234) [pye| 1234    |]
    , testCase "Double->Int" $ eq @Int Nothing     [pye| 1234.25 |]
    , testCase "None->Int"   $ eq @Int Nothing     [pye| None    |]
    ]
  , testGroup "Double"
    [ testCase "Int->Double"    $ eq @Double (Just 1234)    [pye| 1234    |]
    , testCase "Double->Double" $ eq @Double (Just 1234.25) [pye| 1234.25 |]
    , testCase "None->Double"   $ eq @Double Nothing        [pye| None    |]
    ]
  , testGroup "Complex"
    [ testCase "Int->Complex"     $ eq @(Complex Double) (Just 1234)    [pye| 1234    |]
    , testCase "Double->Complex"  $ eq @(Complex Double) (Just 1234.25) [pye| 1234.25 |]
    , testCase "Complex->Complex" $ eq @(Complex Double) (Just $ 1234.5 :+ 6789)
                                     [pye| 1234.5+6789.0j |]
    , testCase "None->Complex"    $ eq @(Complex Double) Nothing        [pye| None    |]
    ]
  , testGroup "Char"
    [ testCase "0"    $ eq @Char Nothing    [pye| ""   |]
    , testCase "1 1B" $ eq @Char (Just 'a') [pye| "a"  |]
    , testCase "2 2B" $ eq @Char (Just 'ы') [pye| "ы"  |]
    , testCase "2"    $ eq @Char Nothing    [pye| "as" |]
    , testCase "None" $ eq @Char Nothing    [pye| None |]
    ]
  , testGroup "String"
    [ testCase "asdf" $ eq @String (Just "asdf") [pye| "asdf" |]
    , testCase "фыва" $ eq @String (Just "фыва") [pye| "фыва" |]
    ]
  , testGroup "ByteString"
    [ testCase "empty" $ eq @BS.ByteString (Just "") [pye| b'' |]
    , testCase "x00"   $ eq @BS.ByteString (Just $ BS.pack [0]) [pye| b'\x00' |]
    , testCase "empty arr" $ eq @BS.ByteString (Just "") [pye| bytearray(b'') |]
    , testCase "x00 arr"   $ eq @BS.ByteString (Just $ BS.pack [0]) [pye| bytearray(b'\x00') |]
    ]
  , testGroup "Bool"
    [ testCase "True->Bool"  $ eq @Bool (Just True)  [pye| True  |]
    , testCase "False->Bool" $ eq @Bool (Just False) [pye| False |]
    , testCase "None->Bool"  $ eq @Bool (Just False) [pye| None  |]
      -- FIXME: Names defined in pymain leak!
    , testCase "Exception" $ runPy $ do
        [pymain|
               class Bad:
                   def __bool__(self):
                       raise Exception("Bad __bool__")
               |]
        failE @Bool =<< [pye| Bad() |]
        -- Segfaults if exception is not cleared
        [py_| 1+1 |]
    ]
  , testGroup "Tuple2"
    [ testCase "T2" $ eq @(Int,Bool) (Just (2,True)) [pye| (2,3) |]
    , testCase "L2" $ eq @(Int,Bool) (Just (2,True)) [pye| [2,3] |]
    , testCase "L1" $ eq @(Int,Bool) Nothing [pye| [1]     |]
    , testCase "T3" $ eq @(Int,Bool) Nothing [pye| (1,2,3) |]
    , testCase "X"  $ eq @(Int,Bool) Nothing [pye| 2 |]
    ]
  , testGroup "Tuple3"
    [ testCase "T3" $ eq @(Int,Int,Int) (Just (1,2,3)) [pye| (1,2,3) |]
    , testCase "L3" $ eq @(Int,Int,Int) (Just (1,2,3)) [pye| [1,2,3] |]
    , testCase "L1" $ eq @(Int,Int,Int) Nothing [pye| [1]       |]
    , testCase "T4" $ eq @(Int,Int,Int) Nothing [pye| (1,2,3,4) |]
    , testCase "X"  $ eq @(Int,Int,Int) Nothing [pye| 2 |]
    ]
  , testGroup "Tuple4"
    [ testCase "T4" $ eq @(Int,Int,Int,Int) (Just (1,2,3,4)) [pye| (1,2,3,4) |]
    , testCase "L4" $ eq @(Int,Int,Int,Int) (Just (1,2,3,4)) [pye| [1,2,3,4] |]
    , testCase "L1" $ eq @(Int,Int,Int,Int) Nothing [pye| [1] |]
    , testCase "X"  $ eq @(Int,Int,Int,Int) Nothing [pye| 2   |]
    ]
  , testGroup "List"
    [ testCase "()"  $ eq @[Int] (Just [])      [pye| ()      |]
    , testCase "[]"  $ eq @[Int] (Just [])      [pye| []      |]
    , testCase "[1]" $ eq @[Int] (Just [1])     [pye| [1]     |]
    , testCase "[3]" $ eq @[Int] (Just [1,2,3]) [pye| [1,2,3] |]
    , testCase "Int" $ eq @[Int] Nothing        [pye| None    |]
    ]
  ]

failE :: forall a. (Eq a, Show a, FromPy a) => PyObject -> Py ()
failE p = fromPyEither @a p >>= \case
  Left PyError{} -> pure ()
  r              -> liftIO $ assertFailure $ "Should fail with exception, but: " ++ show r

