{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Interface to python's @eval@ and @exec@
module Python.Inline.Eval
  ( -- * Python execution
    eval
  , exec
    -- * Source code
  , PyQuote(..)
  , Namespace(..)
  , Main(..)
  , Temp(..)
    -- ** Data types
  , Code
  , codeFromText
  , codeFromString
  , DictBinder

  ) where

import Python.Internal.Types
import Python.Internal.Eval

