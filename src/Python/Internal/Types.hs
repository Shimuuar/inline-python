{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
-- |
-- Definition of data types used by inline-python. They are moved to
-- separate module since some are required for @inline-c@'s context
-- and we need context for
module Python.Internal.Types
  ( -- * Data type
    PyObject(..)
  , PyError(..)
  , Py(..)
  , finallyPy
  , catchPy
  , maskPy_
  , throwPy
    -- * inline-C
  , pyCtx
    -- * Patterns
  , pattern IPY_OK
  , pattern IPY_ERR_COMPILE
  , pattern IPY_ERR_PYTHON
  , pattern NULL
  ) where

import Control.Exception
import Control.Monad.IO.Class
import Data.Coerce
import Data.Map.Strict           qualified as Map
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types
import Language.C.Types
import Language.C.Inline.Context


----------------------------------------------------------------
-- Primitives
----------------------------------------------------------------

-- | Some python object. Since almost everything in python is mutable
--   it could only be accessed only in IO monad.
newtype PyObject = PyObject (ForeignPtr PyObject)

-- | Python exception converted to haskell
data PyError = PyError String
  deriving stock (Show)

instance Exception PyError


-- | Monad for code which is interacts directly with python
--   interpreter. It's needed because in multithreaded runtime one
--   can't call python's C function from any thread. We need to send
--   it for execution on designated OS thread. 
newtype Py a = Py (IO a)
  deriving newtype (Functor,Applicative,Monad,MonadIO,MonadFail)
-- See NOTE: [Python and threading]

catchPy :: forall e a. Exception e => Py a -> (e -> Py a) -> Py a
catchPy = coerce (catch @e @a)

finallyPy :: forall a b. Py a -> Py b -> Py a
finallyPy = coerce (finally @a @b)

maskPy_ :: forall a. Py a -> Py a
maskPy_ = coerce (mask_ @a)

throwPy :: Exception e => e -> Py a
throwPy = Py . throwIO

----------------------------------------------------------------
-- inline-C
----------------------------------------------------------------

-- | @inline-c@ context for mapping
pyCtx :: Context
pyCtx = mempty { ctxTypesTable = Map.fromList tytabs } where
  tytabs =
    [ (TypeName "PyObject", [t| PyObject |])
    ]


----------------------------------------------------------------
-- Patterns
----------------------------------------------------------------

pattern IPY_OK, IPY_ERR_PYTHON, IPY_ERR_COMPILE :: CInt
-- | Success
pattern IPY_OK          = 0
-- | Python exception raised
pattern IPY_ERR_PYTHON  = 1
-- | Error while compiling python source to byte code. Normally it
--   shouldn't happen.
pattern IPY_ERR_COMPILE = 2


pattern NULL :: Ptr a
pattern NULL <- ((== nullPtr) -> True) where
  NULL = nullPtr
