{-# LANGUAGE TemplateHaskell #-}
-- |
-- Quasiquoters for embedding python expression into haskell programs.
-- Python is statement oriented and heavily relies on mutable state.
-- This means we need several different quasiquoters.
module Python.Inline.QQ
  ( pymain
  , py_
  , pye
  , pyf
  ) where

import Control.Monad.Trans.Cont
import Control.Monad.Trans.Class
import Language.Haskell.TH.Quote

import Python.Internal.EvalQQ
import Python.Internal.Eval
import Python.Internal.Types

-- | Evaluate python code in context of main module. All variables
--   defined in this block will remain visible. This quasiquote
--   doesn't return any python value.
--
--   This quote creates object of type @IO ()@
pymain :: QuasiQuoter
pymain = QuasiQuoter
  { quoteExp  = \txt -> [| runPy $ do
      p_main <- basicMainDict
      src    <- $(expQQ Exec txt) p_main
      pyExecExpr p_main p_main src
      |]
  , quotePat  = error "quotePat"
  , quoteType = error "quoteType"
  , quoteDec  = error "quoteDec"
  }

-- | Evaluate python code in context of main module. All variables
--   defined in this block will be discarded. This quasiquote doesn't
--   return any python value.
--
--   This quote creates object of type @IO ()@
py_ :: QuasiQuoter
py_ = QuasiQuoter
  { quoteExp  = \txt -> [| runPy $ evalContT $ do
      p_globals <- lift basicMainDict
      p_locals  <- takeOwnership =<< lift basicNewDict
      lift $ do
        src <- $(expQQ Exec txt) p_locals
        pyExecExpr p_globals p_locals src
      |]
  , quotePat  = error "quotePat"
  , quoteType = error "quoteType"
  , quoteDec  = error "quoteDec"
  }

-- | Evaluate single python expression. It only accepts single
--   expressions same as python's @eval@.
--
--   This quote creates object of type @IO PyObject@
pye :: QuasiQuoter
pye = QuasiQuoter
  { quoteExp  = \txt -> [| runPy $ evalContT $ do
      p_globals <- lift basicMainDict
      p_locals  <- takeOwnership =<< lift basicNewDict
      lift $ do
        src <- $(expQQ Eval txt) p_locals
        pyEvalExpr p_globals p_locals src
      |]
  , quotePat  = error "quotePat"
  , quoteType = error "quoteType"
  , quoteDec  = error "quoteDec"
  }

-- | Another quasiquoter which works around that sequence of python
--   statements doesn't have any value associated with it.  Content of
--   quasiquote is function body. So to get value out of it one must
--   call return
pyf :: QuasiQuoter
pyf = QuasiQuoter
  { quoteExp  = \txt -> [| runPy $ evalContT $ do
      p_globals <- lift basicMainDict
      p_locals  <- takeOwnership =<< lift basicNewDict
      p_kwargs  <- takeOwnership =<< lift basicNewDict
      lift $ do
        -- Create function in p_locals
        src <- $(expQQ Fun txt) p_kwargs
        pyExecExpr p_globals p_locals src
        -- Look up function
        p_fun <- getFunctionObject p_locals >>= \case
          NULL -> error "INTERNAL ERROR: _inline_python_ must be present"
          p    -> pure p
        -- Call python function we just constructed
        callFunctionObject p_fun p_kwargs >>= \case
          NULL  -> throwPy =<< convertPy2Haskell
          p_res -> newPyObject p_res
      |]
  , quotePat  = error "quotePat"
  , quoteType = error "quoteType"
  , quoteDec  = error "quoteDec"
  }
