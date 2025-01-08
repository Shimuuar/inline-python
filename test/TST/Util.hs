-- |
module TST.Util where

import Control.Monad.IO.Class
import Control.Monad.Catch
import Test.Tasty.HUnit

import Python.Inline
import Python.Types

throwsPy :: Py () -> Py ()
throwsPy io = (io >> liftIO (assertFailure "Evaluation should raise python exception"))
  `catch` (\(_::PyError) -> pure ())

