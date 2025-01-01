module Main where

import Test.Tasty

import TST.Run
import TST.FromPy
import Python.Inline

main :: IO ()
main = withPython $ defaultMain $ testGroup "PY"
  [ TST.Run.tests
  , TST.FromPy.tests
  ]
