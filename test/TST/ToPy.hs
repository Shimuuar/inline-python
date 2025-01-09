-- |
module TST.ToPy (tests) where

import Data.Set qualified as Set
import Test.Tasty
import Test.Tasty.HUnit
import Python.Inline
import Python.Inline.QQ
import TST.Util


tests :: TestTree
tests = testGroup "ToPy"
  [ testCase "Int"            $ runPy $ let i = 1234    :: Int    in [py_| assert i_hs == 1234    |]
  , testCase "Double"         $ runPy $ let i = 1234.25 :: Double in [py_| assert i_hs == 1234.25 |]
  , testCase "Char ASCII"     $ runPy $ let c = 'a'    in [py_| assert c_hs == 'a' |]
  , testCase "Char unicode"   $ runPy $ let c = 'ы'    in [py_| assert c_hs == 'ы' |]
  , testCase "String ASCII"   $ runPy $ let c = "asdf" in [py_| assert c_hs == 'asdf' |]
  , testCase "String unicode" $ runPy $ let c = "фыва" in [py_| assert c_hs == 'фыва' |]
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
      in throwsPy [py_| assert x_hs == {1,3,5} |]
  ]
