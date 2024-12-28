{-# LANGUAGE TemplateHaskell #-}
-- |
-- Quasiquoters for embedding python expression into haskell programs.
module Python.Inline.QQ
  ( py
  , pye
  ) where

import Language.Haskell.TH.Quote

import Python.Internal.Eval

py :: QuasiQuoter
py = QuasiQuoter
  { quoteExp  = \txt -> [| do p_env <- basicMainDict
                              src   <- $(expQQ "exec" (unindent txt)) p_env
                              pyEvalInMain p_env src
                         |]
  , quotePat  = error "quotePat"
  , quoteType = error "quoteType"
  , quoteDec  = error "quoteDec"
  }

pye :: QuasiQuoter
pye = QuasiQuoter
  { quoteExp  = \txt -> [| do p_env <- basicNewDict
                              src   <- $(expQQ "eval" (unindent txt)) p_env
                              pyEvalExpr p_env src
                         |]
  , quotePat  = error "quotePat"
  , quoteType = error "quoteType"
  , quoteDec  = error "quoteDec"
  }
