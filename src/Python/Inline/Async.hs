-- |
-- Asynchronous computation using python. Its API is modelled after
-- @async@ package. It evaluates python on separate OS thread so it's
-- more heavyweight than 'Python.Inline.runPy'. But it's possible to
-- properly interrupt running computation with 'cancelPy' or to use
-- 'withPyAsync' to ensure that async computation properly terminated.
--
-- Since arbitrary IO is available either in @Py@ via @liftIO@ or in
-- haskell callbacks from python code. It's possible to use haskell
-- concurrency primitives to communicate with python thread.
module Python.Inline.Async
  ( PyAsync
  , PyAsyncCancelled(..)
  , runPyAsync
  , withPyAsync
  , waitPy
  , waitPyCatch
  , cancelPy
  , uninterruptibleCancelPy
  ) where

import Python.Internal.Eval
