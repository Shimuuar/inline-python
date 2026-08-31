-- |
module TST.Module where

import Test.Tasty
import Test.Tasty.HUnit
import Python.Inline
import Python.Inline.QQ


tests :: TestTree
tests = testGroup "Builtin module"
  [ testCase "Module exists" $ runPy [py_| import inline_python |]
  ]
