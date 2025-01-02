{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes         #-}
-- |
module TST.FromPy (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Python.Inline
import Python.Inline.QQ

tests :: TestTree
tests = testGroup "FromPy"
  [ testGroup "Int"
    [ testCase "Int->Int"    $ eq @Int (Just 1234) =<< [pye| 1234    |]
    , testCase "Double->Int" $ eq @Int Nothing     =<< [pye| 1234.25 |]
    , testCase "None->Int"   $ eq @Int Nothing     =<< [pye| None    |]
    ]
  , testGroup "Double"
    [ testCase "Int->Double"    $ eq @Double (Just 1234)    =<< [pye| 1234    |]
    , testCase "Double->Double" $ eq @Double (Just 1234.25) =<< [pye| 1234.25 |]
    , testCase "None->Double"   $ eq @Double Nothing        =<< [pye| None    |]
    ]
  , testGroup "Bool"
    [ testCase "True->Bool"  $ eq @Bool (Just True)  =<< [pye| True  |]
    , testCase "False->Bool" $ eq @Bool (Just False) =<< [pye| False |]
    , testCase "None->Bool"  $ eq @Bool (Just False) =<< [pye| None  |]
      -- FIXME: Names leak!
    , testCase "Exception" $ do
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
    [ testCase "(2)->2" $ eq @(Int,Bool) (Just (2,True)) =<< [pye| (2,2) |]
    , testCase "[2]->2" $ eq @(Int,Bool) (Just (2,True)) =<< [pye| [2,2] |]
    , testCase "(1)->2" $ eq @(Int,Bool) Nothing =<< [pye| (1)     |]
    , testCase "(3)->2" $ eq @(Int,Bool) Nothing =<< [pye| (1,2,3) |]
    , testCase "X->2"   $ eq @(Int,Bool) Nothing =<< [pye| 2 |]
    ]
  , testGroup "List"
    [ testCase "()"  $ eq @[Int] (Just [])      =<< [pye| ()      |]
    , testCase "[]"  $ eq @[Int] (Just [])      =<< [pye| []      |]
    , testCase "[1]" $ eq @[Int] (Just [1])     =<< [pye| [1]     |]
    , testCase "[3]" $ eq @[Int] (Just [1,2,3]) =<< [pye| [1,2,3] |]
    , testCase "Int" $ eq @[Int] Nothing        =<< [pye| None    |]
    ]
  ]

eq :: (Eq a, Show a, FromPy a) => Maybe a -> PyObject -> IO ()
eq a p = assertEqual "fromPy: " a =<< fromPy p

failE :: forall a. (Eq a, Show a, FromPy a) => PyObject -> IO ()
failE p = fromPyEither @a p >>= \case
  Left PyError{} -> pure ()
  r              -> assertFailure $ "Should fail with exception, but: " ++ show r

