{-# LANGUAGE TemplateHaskell #-}
-- |
-- Quasiquoters for embedding python expression into haskell programs.
-- Python is statement oriented and heavily relies on mutable state.
-- This means we need several different quasiquoters.
module Python.Inline.QQ
  ( pymain
  , py_
  , pye
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
  { quoteExp  = \txt -> [| runPy $ do p_main <- basicMainDict
                                      src   <- $(expQQ Exec (unindent txt)) p_main
                                      pyEvalInMain p_main p_main src
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
  { quoteExp  = \txt -> [| runPy $ do p_globals <- basicMainDict
                                      p_locals  <- basicNewDict
                                      src   <- $(expQQ Exec (unindent txt)) p_locals
                                      res   <- pyEvalInMain p_globals p_locals src
                                      basicDecref p_locals
                                      return res
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
  { quoteExp  = \txt -> [| runPy $ do p_env <- basicNewDict
                                      src   <- $(expQQ Eval (unindent txt)) p_env
                                      res   <- pyEvalExpr p_env src
                                      basicDecref p_env
                                      return res
                         |]
  , quotePat  = error "quotePat"
  , quoteType = error "quoteType"
  , quoteDec  = error "quoteDec"
  }
