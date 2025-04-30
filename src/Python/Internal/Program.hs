{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
module Python.Internal.Program
  ( Program(..)
  , runProgram
  , progPy
  , progIO
  , progIOBracket
    -- * Control flow
  , abort
  , abortM
  , abortOnNull
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
    -- * Helpers
  , pyobjectStrAsHask
  ) where

import Control.Monad
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

import Language.C.Inline          qualified as C
import Language.C.Inline.Unsafe   qualified as CU

import Python.Internal.Types
import Python.Internal.Util
import Python.Internal.CAPI

----------------------------------------------------------------
C.context (C.baseCtx <> pyCtx)
C.include "<inline-python.h>"
----------------------------------------------------------------


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

progIOBracket :: ((a -> IO r) -> IO r) -> Program r a
progIOBracket = coerce

-- | Early exit from continuation monad.
abort :: r -> Program r a
abort r = Program $ ContT $ \_ -> pure r

-- | Early exit from continuation monad.
abortM :: Py r -> Program r a
abortM m = Program $ ContT $ \_ -> m

-- | Perform early exit if pointer is null
abortOnNull :: r -> Py (Ptr a) -> Program r (Ptr a)
abortOnNull r action = Program $ ContT $ \cnt -> action >>= \case
  NULL -> pure r
  p    -> cnt p

-- | If result of computation is NULL return NULL immediately.
checkNull :: Py (Ptr a) -> Program (Ptr a) (Ptr a)
checkNull = abortOnNull nullPtr

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


----------------------------------------------------------------
-- More complicated helpers
----------------------------------------------------------------

-- | Call @__str__@ method of object and return haskell
--   string. Returns Nothing if exception was raisede
pyobjectStrAsHask :: Ptr PyObject -> Py (Maybe String)
pyobjectStrAsHask p_obj = runProgram $ do
  p_str <- takeOwnership <=< abortOnNull Nothing $ Py [CU.block| PyObject* {
    PyObject *s = PyObject_Str($(PyObject *p_obj));
    if( PyErr_Occurred() ) {
        PyErr_Clear();
    }
    return s;
    } |]
  c_str <- abortOnNull Nothing $ Py [CU.block| const char* {
    const char* s = PyUnicode_AsUTF8($(PyObject *p_str));
    if( PyErr_Occurred() ) {
        PyErr_Clear();
    }
    return s;
    } |]
  progIO $ Just <$> peekCString c_str
