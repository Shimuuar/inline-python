{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
-- |
-- Evaluation of python expressions
module Python.Internal.Eval
  ( -- * Evaluator
    PyEvalReq(..)
  , EvalStatus(..)
  , toPythonThread
  , pythonInterpreter
  , runPy
  , unPy
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Cont
import Data.Char
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe

import Language.C.Inline          qualified as C
import Language.C.Inline.Unsafe   qualified as CU
import Language.Haskell.TH.Lib    qualified as TH
import Language.Haskell.TH.Syntax qualified as TH

import Python.Types
import Python.Internal.Types


----------------------------------------------------------------
C.context (C.baseCtx <> pyCtx)
C.include "<inline-python.h>"
----------------------------------------------------------------

-- NOTE: [Python evaluation]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Interaction with python interpreter and haskell's threading drives
-- important design decisions:
--
--  1. Python is essentially single threaded. Before any interaction
--     with interpreter one must acquire GIL.
--
--  2. PyGILState_{Ensure,Release} use thread local state.
--
-- And here we run into problem. Haskell's runtime migrates green
-- threads freely between OS threads. We can't use that API! Still we
-- want to be able to evaluate python expressions in normal IO monad.
--
-- So only solution is create separate bound thread and use MVars to
-- send data between threads.


-- | Status of evaluation request for python interprereter
data EvalStatus
  = Pending   -- ^ Request has not started evaluation
  | Running   -- ^ Evaluation is in progress
  | Cancelled -- ^ Request was cancelled
  | Done      -- ^ Request finished evaluation
  deriving stock Show

-- | Evaluation request sent to
data PyEvalReq = forall a. PyEvalReq
  { prog   :: (Py a)
  , retval :: MVar (Either SomeException a)
  , status :: MVar EvalStatus
  }

-- | Python evaluator reads messages from this MVar
toPythonThread :: MVar PyEvalReq
toPythonThread = unsafePerformIO newEmptyMVar
{-# NOINLINE toPythonThread #-}

-- | ThreadId of thread running python's interpreter
pythonInterpreter :: MVar ThreadId
pythonInterpreter = unsafePerformIO newEmptyMVar
{-# NOINLINE pythonInterpreter #-}

-- | Execute python action.
runPy :: Py a -> IO a
runPy py = do
  retval <- newEmptyMVar
  status <- newMVar Pending
  (do putMVar toPythonThread $ PyEvalReq{ prog=py, ..}
      takeMVar retval >>= \case
        Left  e -> throwIO e
        Right a -> pure a
    ) `catch` onExc status
 where
   onExc :: MVar EvalStatus -> SomeException -> IO b
   onExc status e = do
     modifyMVar_ status $ \case
       Pending   -> pure Cancelled
       Cancelled -> pure Cancelled
       Done      -> pure Done
       Running   -> Cancelled <$ [CU.exp| void { PyErr_SetInterrupt() } |]
     throwIO e


-- | Execute python action. This function is unsafe and should be only
--   called in thread of interpreter.
unPy :: Py a -> IO a
unPy (Py io) = io
