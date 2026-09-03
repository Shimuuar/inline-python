-- |
module TST.Module where

import Control.Exception
import Data.Typeable
import Test.Tasty
import Test.Tasty.HUnit
import Python.Inline
import Python.Inline.QQ
import Python.Inline.Async

tests :: TestTree
tests = testGroup "Builtin module"
  [ testCase "Module exists" $ runPy [py_| import inline_python |]
  , testCase "Exceptions" $ runPy [py_|
      import inline_python
      assert issubclass(inline_python.AsyncCancelled, BaseException)
      assert not issubclass(inline_python.AsyncCancelled, Exception)

      assert issubclass(inline_python.HaskellError, Exception),     "HaskellError is Exception"
      assert issubclass(inline_python.HaskellError, BaseException), "HaskellError is BaseException"
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
  , testCase "AsyncCancelled is converted to PyAsyncCancelled" $ do
    r :: Either SomeException () <- try $ runPy [py_|
      import inline_python
      raise inline_python.AsyncCancelled()
      |]
    case r of
      Right () -> error "No exception"
      Left  (SomeException e)
        | Just PyAsyncCancelled <- cast e -> pure ()
        | otherwise                       -> throwIO e
  , testCase "Haskell exception are converted 1" $ do
      let foo :: IO Int
          foo = return $! 1 `div` 0
      runPy [py_|
                try:
                    foo_hs()
                except Exception as e:
                    pass
                    del e
                    #print(e)
                    #print(type(e))
               |]
  , testCase "Haskell exception are converted 2" $ do
      let foo :: IO Int
          foo = return $! 1 `div` 0
      let handler DivideByZero = pure ()
          handler e            = throwIO e
      runPy [py_| foo_hs() |] `catch` handler

  ]
