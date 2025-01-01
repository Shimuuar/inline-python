-- |
module Python.Internal.Util where

import Control.Monad.Trans.Cont
import Data.Char
import Data.Coerce
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Marshal
import Foreign.C.Types
import Foreign.C.String
import Foreign.Storable

import Python.Internal.Types


withWCtring :: String -> (Ptr CWchar -> IO a) -> IO a
withWCtring = withArray0 (CWchar 0) . map (fromIntegral . ord)


----------------------------------------------------------------
-- Allocation in context of `ContT _ Py`
----------------------------------------------------------------

withPyAlloca :: forall a r. Storable a => ContT r Py (Ptr a)
withPyAlloca = coerce (alloca @a @r)

withPyAllocaArray :: forall a r. Storable a => Int -> ContT r Py (Ptr a)
withPyAllocaArray = coerce (allocaArray @a @r)

withPyCString :: forall r. String -> ContT r Py CString
withPyCString = coerce (withCString @r)

withPyCStringLen :: forall r. String -> ContT r Py CStringLen
withPyCStringLen = coerce (withCStringLen @r)
