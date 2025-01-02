-- |
module Python.Internal.Program
  ( Program
  , abort
  , abortM
  , withPyAlloca
  , withPyAllocaArray
  , withPyCString
  , withPyCStringLen
  ) where

import Control.Monad.Trans.Cont
import Data.Coerce
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Marshal
import Foreign.C.String
import Foreign.Storable

import Python.Internal.Types


-- | Internally we usually wrap 'Py' into 'ContT' in order get early
--   exit and avoid building ladder of 
type Program r a = ContT r Py a


-- | Early exit from continuation monad.
abort :: Monad m => r -> ContT r m a
abort r = ContT $ \_ -> pure r

-- | Early exit from continuation monad.
abortM :: Monad m => m r -> ContT r m a
abortM m = ContT $ \_ -> m


----------------------------------------------------------------
-- Allocation in context of `ContT _ Py`
----------------------------------------------------------------

withPyAlloca :: forall a r. Storable a => Program r (Ptr a)
withPyAlloca = coerce (alloca @a @r)

withPyAllocaArray :: forall a r. Storable a => Int -> Program r (Ptr a)
withPyAllocaArray = coerce (allocaArray @a @r)

withPyCString :: forall r. String -> Program r CString
withPyCString = coerce (withCString @r)

withPyCStringLen :: forall r. String -> Program r CStringLen
withPyCStringLen = coerce (withCStringLen @r)
