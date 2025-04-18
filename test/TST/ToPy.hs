{-# LANGUAGE OverloadedStrings #-}
-- |
module TST.ToPy (tests) where

import Data.ByteString      qualified as BS
import Data.Set             qualified as Set
import Data.Map.Strict      qualified as Map
import Data.Complex         (Complex((:+)))
import Test.Tasty
import Test.Tasty.HUnit
import Python.Inline
import Python.Inline.QQ
import TST.Util


tests :: TestTree
tests = testGroup "ToPy"
  [ testCase "Int"            $ runPy $ let i = 1234    :: Int    in [py_| assert i_hs == 1234    |]
  , testCase "Double"         $ runPy $ let i = 1234.25 :: Double in [py_| assert i_hs == 1234.25 |]
  , testCase "Complex" $ runPy $
      let z = 5.5 :+ 7.5 :: Complex Double
      in [py_| assert (z_hs.real == 5.5); assert (z_hs.imag == 7.5)|]
  , testCase "Char ASCII"     $ runPy $ let c = 'a'    in [py_| assert c_hs == 'a' |]
  , testCase "Char unicode"   $ runPy $ let c = 'ы'    in [py_| assert c_hs == 'ы' |]
  , testCase "String ASCII"   $ runPy $ let c = "asdf"::String in [py_| assert c_hs == 'asdf' |]
  , testCase "String unicode" $ runPy $ let c = "фыва"::String in [py_| assert c_hs == 'фыва' |]
    -- Byte objects
  , testCase "empty ByteString" $ runPy $
      let bs = BS.empty in [py_| assert bs_hs == b'' |]
  , testCase "0 ByteString" $ runPy $
      let bs = BS.pack [0] in [py_| assert bs_hs == b'\x00' |]
    -- Container types
  , testCase "Tuple2" $ runPy $
      let x = (1::Int, 333::Int)
      in [py_| assert x_hs == (1,333) |]
  , testCase "Tuple3" $ runPy $
      let x = (1::Int, 333::Int, True)
      in [py_| assert x_hs == (1,333,True) |]
  , testCase "Tuple4" $ runPy $
      let x = (1::Int, 333::Int, True, 'c')
      in [py_| assert x_hs == (1,333,True,'c') |]
  , testCase "nested Tuple2" $ runPy $
      let x = (1::Int, (333::Int,4.5::Double))
      in [py_| assert x_hs == (1,(333,4.5)) |]
  , testCase "list" $ runPy $
      let x = [1 .. 5::Int]
      in [py_| assert x_hs == [1,2,3,4,5] |]
  , testCase "set<int>" $ runPy $
      let x = Set.fromList [1, 5, 3::Int]
      in [py_| assert x_hs == {1,3,5} |]
  , testCase "set unhashable" $ runPy $
      let x = Set.fromList [[1], [5], [3::Int]]
      in throwsPy [py_| x_hs |]
  , testCase "dict<int,int>" $ runPy $
      let x = Map.fromList [(1,10), (5,50), (3,30)] :: Map.Map Int Int
      in [py_| assert x_hs == {1:10, 3:30, 5:50} |]
  , testCase "dict unhashable" $ runPy $
      let x = Map.fromList [([1],10), ([5],50), ([3],30)] :: Map.Map [Int] Int
      in throwsPy [py_| x_hs |]
  ]
