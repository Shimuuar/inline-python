{-# LANGUAGE TemplateHaskell #-}
-- |
module Python.QQ where

import Control.Exception
import Control.Monad.Trans.Cont
import Control.Monad.IO.Class
import Data.Char
import Data.Map.Strict qualified as Map
import Foreign.Ptr
import Foreign.Marshal
import Foreign.C.String
import Foreign.C.Types
import System.Environment

import Language.C.Inline         qualified as C
import Language.C.Inline.Context qualified as C
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax qualified as TH

import Python

py :: QuasiQuoter
py = QuasiQuoter
  { quoteExp  = \txt -> [| pyEvalStr $(TH.lift (unindent txt)) |]
  , quotePat  = error "quotePat"
  , quoteType = error "quoteType"
  , quoteDec  = error "quoteDec"
  }

pye :: QuasiQuoter
pye = QuasiQuoter
  { quoteExp  = \txt -> [| pyEvalStrE $(TH.lift (unindent txt)) |]
  , quotePat  = error "quotePat"
  , quoteType = error "quoteType"
  , quoteDec  = error "quoteDec"
  }
