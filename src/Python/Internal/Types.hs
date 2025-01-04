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
  , unsafeWithPyObject
  , PyThreadState
  , PyError(..)
  , Py(..)
  , catchPy
  , finallyPy
  , onExceptionPy
  , throwPy
  , tryPy
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
import Data.Int
import Data.Map.Strict           qualified as Map
import Foreign.Ptr
import Foreign.C.Types
import GHC.ForeignPtr

import Language.C.Types
import Language.C.Inline.Context


----------------------------------------------------------------
-- Primitives
----------------------------------------------------------------

-- | Pointer tag
data PyThreadState

-- | Some python object. Since almost everything in python is mutable
--   it could only be accessed only in IO monad.
newtype PyObject = PyObject (ForeignPtr PyObject)

unsafeWithPyObject :: forall a. PyObject -> (Ptr PyObject -> Py a) -> Py a
unsafeWithPyObject = coerce (unsafeWithForeignPtr @PyObject @a)

-- | Python exception converted to haskell.
data PyError
  = PyError String String
    -- ^ Python exception. Contains exception type and message as strings.
  | UncovertablePyError
    -- ^ Python error could not be converted to haskell for some reason
  | FromPyFailed
    -- ^ Conversion from python value to failed because python type is
    --   invalid.
  deriving stock (Show)

instance Exception PyError


-- | Monad for code which is interacts directly with python
--   interpreter. During its execution python global interpreter lock
--   (GIL) is held, async exceptions are masked. It's also always
--   executed on bound thread if RTS supports one.
--
--   It's needed in order to distinguish between code that needs such
--   guarantees and plain IO.
newtype Py a = Py (IO a)
  deriving newtype (Functor,Applicative,Monad,MonadIO,MonadFail)
-- See NOTE: [Python and threading]

catchPy :: forall e a. Exception e => Py a -> (e -> Py a) -> Py a
catchPy = coerce (catch @e @a)

finallyPy :: forall a b. Py a -> Py b -> Py a
finallyPy = coerce (finally @a @b)

onExceptionPy :: forall a b. Py a -> Py b -> Py a
onExceptionPy = coerce (onException @a @b)

throwPy :: Exception e => e -> Py a
throwPy = Py . throwIO

tryPy :: forall e a. Exception e => Py a -> Py (Either e a)
tryPy = coerce (try @e @a)

----------------------------------------------------------------
-- inline-C
----------------------------------------------------------------

-- | @inline-c@ context for mapping
pyCtx :: Context
pyCtx = mempty { ctxTypesTable = Map.fromList tytabs } where
  tytabs =
    [ ( TypeName "PyObject",      [t| PyObject      |])
    , ( TypeName "PyThreadState", [t| PyThreadState |])
    , ( TypeName "PyCFunction"
      , [t| FunPtr (Ptr PyObject -> Ptr PyObject -> IO (Ptr PyObject)) |])
    , ( TypeName "PyCFunctionFast"
      , [t| FunPtr (Ptr PyObject -> Ptr (Ptr PyObject) -> Int64 -> IO (Ptr PyObject)) |])
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
