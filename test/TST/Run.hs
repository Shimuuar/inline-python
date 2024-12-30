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


throwsPy :: IO () -> IO ()
throwsPy io = (io >> assertFailure "Evaluation should raise python exception")
  `catch` (\(_::PyError) -> pure ())
