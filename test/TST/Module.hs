-- |
module TST.Module where

import Test.Tasty
import Test.Tasty.HUnit
import Python.Inline
import Python.Inline.QQ


tests :: TestTree
tests = testGroup "Builtin module"
  [ testCase "Module exists" $ runPy [py_| import inline_python |]
  , testCase "AsyncCancelled" $ runPy [py_|
      import inline_python
      assert issubclass(inline_python.AsyncCancelled, BaseException)
      assert not issubclass(inline_python.AsyncCancelled, Exception)
      |]
    -- We want to check that inline_python types are stable under
    -- reload using importlib.
  , testCase "importlib.reload stable" $ runPy [py_|
      import inline_python
      import importlib
      ty  = inline_python.AsyncCancelled
      err = ty()
      importlib.reload(inline_python)
      assert ty is inline_python.AsyncCancelled
      assert isinstance(err, inline_python.AsyncCancelled)
      |]
  ]
