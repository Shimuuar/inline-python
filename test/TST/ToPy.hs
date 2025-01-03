-- |
module TST.ToPy (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Python.Inline
import Python.Inline.QQ

tests :: TestTree
tests = testGroup "ToPy"
  [ testCase "Int"            $ let i = 1234    :: Int    in [py_| assert i_hs == 1234    |]
  , testCase "Double"         $ let i = 1234.25 :: Double in [py_| assert i_hs == 1234.25 |]
  , testCase "Char ASCII"     $ let c = 'a'    in [py_| assert c_hs == 'a' |]
  , testCase "Char unicode"   $ let c = 'ы'    in [py_| assert c_hs == 'ы' |]
  , testCase "String ASCII"   $ let c = "asdf" in [py_| assert c_hs == 'asdf' |]
  , testCase "String unicode" $ let c = "фыва" in [py_| assert c_hs == 'фыва' |]
    -- Container types
  , testCase "Tuple2" $
      let x = (1::Int, 333::Int)
      in [py_| assert x_hs == (1,333) |]
  , testCase "nested Tuple2" $
      let x = (1::Int, (333::Int,4.5::Double))
      in [py_| assert x_hs == (1,(333,4.5)) |]
  , testCase "list" $
      let x = [1 .. 5::Int]
      in [py_| assert x_hs == [1,2,3,4,5] |]
  ]
