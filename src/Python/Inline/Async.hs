-- |
-- Asynchronous computation using python. Normally library tries to
-- execute python code in the same thread. Moreover it use global lock
-- in addition to GIL in order to avoid blocking capability on GIL.
-- This module provide API for working with concurrent python.
-- Its API is heavily modelled after @async@ package.
--
-- Note it's very experimental and not well tested. Also mixing
-- concurrency primitives from two languages makes difficult task of
-- concurrent programming even more complicated.
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

