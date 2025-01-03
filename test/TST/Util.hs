-- |
module TST.Util where

import Control.Exception
import Test.Tasty.HUnit

import Python.Inline

throwsPy :: IO () -> IO ()
throwsPy io = (io >> assertFailure "Evaluation should raise python exception")
  `catch` (\(_::PyError) -> pure ())

