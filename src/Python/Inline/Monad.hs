-- |
module Python.Inline.Monad
  ( -- * Type classes
    Namespace(..)
  , MonadPy(..)
  ) where

import Python.Internal.Eval
import Python.Internal.Types

-- | Monad which can carry dictionaries or object of global and local
--   python variables around. It's expected that it's some variant of
--   wrapper around 'Py'.
--
--   For @Py@ global variables are 'Main' and local are 'Temp'
class Monad m => MonadPy m where
  -- | Lift @Py@ computation into given monad.
  liftPy :: Py a -> m a
  -- | Provide set of global variables. CPS style is used to avoid
  --   specifying type of global used by monad (and allow picking it
  --   at runtime).
  withGlobals
    :: (forall globals. Namespace globals => globals -> m a)
    -> m a
  -- | Same for local variables
  withLocals
    :: (forall locals. Namespace locals => locals -> m a)
    -> m a

instance MonadPy Py where
  liftPy = id
  withGlobals f = f Main
  withLocals  f = f Temp
