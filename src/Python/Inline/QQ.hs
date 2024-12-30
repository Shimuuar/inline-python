{-# LANGUAGE TemplateHaskell #-}
-- |
-- Quasiquoters for embedding python expression into haskell programs.
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
pymain :: QuasiQuoter
pymain = QuasiQuoter
  { quoteExp  = \txt -> [| runPy $ do p_main <- basicMainDict
                                      src   <- $(expQQ "exec" (unindent txt)) p_main
                                      pyEvalInMain p_main p_main src
                         |]
  , quotePat  = error "quotePat"
  , quoteType = error "quoteType"
  , quoteDec  = error "quoteDec"
  }

-- | Evaluate python code in context of main module. All variables
--   defined in this block will be discarded. This quasiquote doesn't
--   return any python value.
py_ :: QuasiQuoter
py_ = QuasiQuoter
  { quoteExp  = \txt -> [| runPy $ do p_globals <- basicMainDict
                                      p_locals  <- basicNewDict
                                      src   <- $(expQQ "exec" (unindent txt)) p_locals
                                      pyEvalInMain p_globals p_locals src
                         |]
  , quotePat  = error "quotePat"
  , quoteType = error "quoteType"
  , quoteDec  = error "quoteDec"
  }

-- | Evaluate single python expression
pye :: QuasiQuoter
pye = QuasiQuoter
  { quoteExp  = \txt -> [| runPy $ do p_env <- basicNewDict
                                      src   <- $(expQQ "eval" (unindent txt)) p_env
                                      pyEvalExpr p_env src
                         |]
  , quotePat  = error "quotePat"
  , quoteType = error "quoteType"
  , quoteDec  = error "quoteDec"
  }
