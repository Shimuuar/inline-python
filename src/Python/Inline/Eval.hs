{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Interface to python's @eval@ and @exec@ which gives programmer
-- control over local and global variables.
module Python.Inline.Eval
  ( -- * Python execution
    eval
  , exec
    -- * Source code
  , PyQuote(..)
  , Code
  , codeFromText
  , codeFromString
  , DictBinder
    -- * Variable namespaces
  , Namespace(..)
  , Main(..)
  , Temp(..)
  , Dict(..)
  , Module(..)
  ) where

import Python.Internal.Types
import Python.Internal.Eval
