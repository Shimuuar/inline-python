{-# LANGUAGE QuasiQuotes #-}
-- |
module TST.Run(tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Python.Inline
import Python.Inline.QQ

tests :: TestTree
tests = testGroup "Run python"
  [ -- testCase "Empty QQ" [py| print(1) |]
  ]
  
