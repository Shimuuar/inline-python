{-# LANGUAGE QuasiQuotes #-}
module Main where

import Test.Tasty
import Test.Tasty.HUnit

import TST.Run
import Python.Inline
import Python.Inline.QQ


main :: IO ()
main = withPython $ do
  [py| print(1) |]
  defaultMain $ testGroup "PY"
    [ TST.Run.tests
    , testCase "Z" $ [py| print(1) |]
    ]
