-- |
module Python.Internal.Program
  ( Program
    -- * Control flow
  , abort
  , abortM
  , checkNull
  , finallyProg
  , onExceptionProg
    -- * Allocators
  , withPyAlloca
  , withPyAllocaArray
  , withPyCString
  , withPyCStringLen
  , withPyWCString
  ) where

import Control.Monad.Trans.Cont
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


-- | Internally we usually wrap 'Py' into 'ContT' in order get early
--   exit and avoid building ladder of 
type Program r a = ContT r Py a


-- | Early exit from continuation monad.
abort :: Monad m => r -> ContT r m a
abort r = ContT $ \_ -> pure r

-- | Early exit from continuation monad.
abortM :: Monad m => m r -> ContT r m a
abortM m = ContT $ \_ -> m

-- | If result of computation is NULL return NULL immediately.
checkNull :: Py (Ptr a) -> Program (Ptr a) (Ptr a)
checkNull action = ContT $ \cnt -> action >>= \case
  NULL -> pure nullPtr
  p    -> cnt p

-- | Evaluate finalizer even if exception is thrown.
finallyProg
  :: Py b -- ^ Finalizer
  -> Program r ()
finallyProg fini = ContT $ \c -> c () `finally` fini

-- | Evaluate finalizer if exception is thrown.
onExceptionProg
  :: Py b -- ^ Finalizer
  -> Program r ()
onExceptionProg fini = ContT $ \c -> c () `onException` fini


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
