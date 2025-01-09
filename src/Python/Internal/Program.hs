-- |
module Python.Internal.Program
  ( Program(..)
  , runProgram
  , progPy
  , progIO
    -- * Control flow
  , abort
  , abortM
  , checkNull
  , finallyProg
  , onExceptionProg
  , takeOwnership
    -- * Allocators
  , withPyAlloca
  , withPyAllocaArray
  , withPyCString
  , withPyCStringLen
  , withPyWCString
  ) where

import Control.Monad.Trans.Cont
import Control.Monad.Trans.Class
import Control.Monad.Catch
import Data.Coerce
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Marshal
import Foreign.C.String
import Foreign.C.Types
import Foreign.Storable

import Python.Internal.Types
import Python.Internal.Util
import Python.Internal.CAPI


-- | This monad wraps 'Py' into 'ContT' in order get early exit,
--   applying @finally@ while avoiding building huge ladders.
newtype Program r a = Program (ContT r Py a)
  deriving newtype (Functor, Applicative, Monad)

runProgram :: Program a a -> Py a
runProgram (Program m) = evalContT m

-- | Does not change masking state
progIO :: IO a -> Program r a
progIO = Program . lift . pyIO

progPy :: Py a -> Program r a
progPy = Program . lift

-- | Early exit from continuation monad.
abort :: r -> Program r a
abort r = Program $ ContT $ \_ -> pure r

-- | Early exit from continuation monad.
abortM :: Py r -> Program r a
abortM m = Program $ ContT $ \_ -> m

-- | If result of computation is NULL return NULL immediately.
checkNull :: Py (Ptr a) -> Program (Ptr a) (Ptr a)
checkNull action = Program $ ContT $ \cnt -> action >>= \case
  NULL -> pure nullPtr
  p    -> cnt p

-- | Evaluate finalizer even if exception is thrown.
finallyProg
  :: Py b -- ^ Finalizer
  -> Program r ()
finallyProg fini = Program $ ContT $ \c -> c () `finally` fini

-- | Evaluate finalizer if exception is thrown.
onExceptionProg
  :: Py b -- ^ Finalizer
  -> Program r ()
onExceptionProg fini = Program $ ContT $ \c -> c () `onException` fini

-- | Decrement reference counter at end of ContT block
takeOwnership :: Ptr PyObject -> Program r (Ptr PyObject)
takeOwnership p = Program $ ContT $ \c -> c p `finally` decref p


----------------------------------------------------------------
-- Allocation in context of `ContT _ Py`
----------------------------------------------------------------

withPyAlloca :: forall a r. Storable a => Program r (Ptr a)
withPyAlloca = coerce (alloca @a @r)

withPyAllocaArray :: forall a r. Storable a => Int -> Program r (Ptr a)
withPyAllocaArray = coerce (allocaArray @a @r)

withPyCString :: forall r. String -> Program r CString
withPyCString = coerce (withCString @r)

withPyWCString :: forall r. String -> Program r (Ptr CWchar)
withPyWCString = coerce (withWCString @r)

withPyCStringLen :: forall r. String -> Program r CStringLen
withPyCStringLen = coerce (withCStringLen @r)
