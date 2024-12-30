{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE TemplateHaskell           #-}
-- |
-- Evaluation of python expressions
module Python.Internal.Eval
  ( -- * Evaluator
    PyEvalReq(..)
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


-- | Evaluation request sent to
data PyEvalReq = forall a. PyEvalReq (Py a) (MVar a)

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
runPy (Py io) = io

-- | Execute python action.
unPy :: Py a -> IO a
unPy (Py io) = io




