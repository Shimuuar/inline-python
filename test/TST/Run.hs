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
  [ testCase "Empty QQ" [py| |]
  , testCase "Vars are visible" $ do
      [py| x = 12       |]
      [py| y = x + 1000 |]
  , testCase "Python exceptions are converted" $ do
      throwsPy [py| 1 / 0 |]
  ]

throwsPy :: IO () -> IO ()
throwsPy io = (io >> assertFailure "Evaluation should raise python exception")
  `catch` (\(e::PyError) -> pure ())
