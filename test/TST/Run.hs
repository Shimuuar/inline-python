{-# LANGUAGE QuasiQuotes #-}
-- |
module TST.Run(tests) where

import Control.Exception
import Test.Tasty
import Test.Tasty.HUnit
import Python.Inline
import Python.Inline.QQ

tests :: TestTree
tests = testGroup "Run python"
  [ testCase "Empty QQ" [py_| |]
  , testCase "Python exceptions are converted" $ throwsPy [py_| 1 / 0 |]
  , testsScope
  , testCapture
  ]


testsScope :: TestTree
testsScope = testGroup "Variable scope"
  [ testCase "pymain->any" $ do
      [pymain|
             x = 12
             x
             |]
      -- Visible
      [py_|
             x
             |]
      [pymain|
             x
             del x
             |]
      -- Disappears
      [pymain|
             try:
                 x
                 assert False, "x shouln't be visible"
             except NameError:
                 pass
             |]
  , testCase "py_->any" $ do
      [py_|
             x = 12
             x
             |]
      -- Not visible
      [py_|
             try:
                 x
                 assert False, "x shouln't be visible (1)"
             except NameError:
                 pass
             |]
      [pymain|
             try:
                 x
                 assert False, "x shouln't be visible (2)"
             except NameError:
                 pass
             |]
  ]



testCapture :: TestTree
testCapture = testGroup "Capture of vars"
  [ testCase "Capture int"    $ let i = 1::Int      in [py_| assert i_hs == 1   |]
  , testCase "Capture double" $ let x = 1.5::Double in [py_| assert x_hs == 1.5 |]
    --
  , testCase "Closure(arity=1)" $ do
      let double = pure . (*2) :: Int -> IO Int
      [py_|
          assert double_hs(3) == 6
          # Invalid arg
          try:
              double_hs(None)
          except TypeError as e:
              pass
          # Wrong arg number
          try:
              double_hs(1,2,3)
          except TypeError as e:
              pass
          |]
  , testCase "Closure(arity=2)" $ do
      let foo :: Int -> Double -> IO Int
          foo x y = pure $ x + round y
      [py_|
          assert foo_hs(3, 100.2) == 103
          assert foo_hs(3, 100)   == 103
          # Invalid arg
          try:
              foo_hs(None, 100)
          except TypeError as e:
              pass
          # Wrong arg number
          try:
              foo_hs(1,2,3)
          except TypeError as e:
              pass

          |]
    --
  , testCase "Haskell exception in callbacks" $ do
      let foo :: Int -> Int -> IO Int
          foo x y = pure $ x `div` y
      throwsPy [py_| foo_hs(1, 0) |]
  ]


throwsPy :: IO () -> IO ()
throwsPy io = (io >> assertFailure "Evaluation should raise python exception")
  `catch` (\(_::PyError) -> pure ())
