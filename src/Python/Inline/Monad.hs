-- |
module Python.Inline.Monad
  ( -- * Type classes
    Namespace(..)
  , MonadPy(..)
  , evalM
  , execM
  , evalPyFunctionM
  ) where

import Python.Internal.Eval
import Python.Internal.Types

-- | Monad which can carry dictionaries or object of global and local
--   python variables around. It's expected that it's some variant of
--   wrapper around 'Py'.
--
--   For @Py@ global variables are 'Main' and local are 'Temp'
--
--  @since 0.3
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


-- | Analog of 'eval' which uses globals and locals carried by 'MonadPy'
--
--  @since 0.3
evalM
  :: (MonadPy m)
  => PyQuote -- ^ Source code
  -> m PyObject
evalM q =
  withGlobals  $ \globals ->
    withLocals $ \locals  ->
      liftPy $ eval globals locals q

-- | Analog of 'exec' which uses globals and locals carried by
--  'MonadPy'
--
--  @since 0.3
execM
  :: (MonadPy m)
  => PyQuote -- ^ Source code
  -> m ()
execM q =
  withGlobals  $ \globals ->
    withLocals $ \locals  ->
      liftPy $ exec globals locals q

-- | Analog of 'evalPyFunction' which uses globals and locals carried by
--  'MonadPy'.
--
--  @since 0.3
evalPyFunctionM
  :: (MonadPy m)
  => PyQuoteFun -- ^ Source code
  -> m PyObject
evalPyFunctionM q =
  withGlobals $ \globals ->
    liftPy $ evalPyFunction globals q
