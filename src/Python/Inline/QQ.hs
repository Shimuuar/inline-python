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

import Language.Haskell.TH.Quote

import Python.Internal.EvalQQ
import Python.Internal.Eval


-- | Evaluate python code in context of main module. All variables
--   defined in this block will remain visible. This quasiquote
--   doesn't return any python value.
--
--   This quote creates object of type @IO ()@
pymain :: QuasiQuoter
pymain = QuasiQuoter
  { quoteExp  = \txt -> [| runPy $ evaluatorPymain $(expQQ Exec txt) |]
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
  { quoteExp  = \txt -> [| runPy $ evaluatorPy_ $(expQQ Exec txt) |]
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
  { quoteExp  = \txt -> [| runPy $ evaluatorPye $(expQQ Eval txt) |]
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
  { quoteExp  = \txt -> [| runPy $ evaluatorPyf $(expQQ Fun txt) |]
  , quotePat  = error "quotePat"
  , quoteType = error "quoteType"
  , quoteDec  = error "quoteDec"
  }
