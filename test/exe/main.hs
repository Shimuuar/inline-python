module Main where

import Test.Tasty

import TST.Run
import TST.FromPy
import TST.ToPy
import TST.Callbacks
import TST.Roundtrip
import Python.Inline

main :: IO ()
main = withPython $ defaultMain $ testGroup "PY"
  [ TST.Run.tests
  , TST.FromPy.tests
  , TST.ToPy.tests
  , TST.Roundtrip.tests
  , TST.Callbacks.tests
  ]
