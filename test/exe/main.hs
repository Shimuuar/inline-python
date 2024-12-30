module Main where

import Test.Tasty

import TST.Run
import Python.Inline

main :: IO ()
main = withPython $ defaultMain $ testGroup "PY"
  [ TST.Run.tests
  ]
