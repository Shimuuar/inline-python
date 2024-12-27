{-# LANGUAGE CApiFFI           #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
-- |
module Python.Types where

import Foreign.Ptr
import Foreign.ForeignPtr
import Language.C.Inline.Context qualified as C
import Language.C.Types   qualified as C
import Data.Map.Strict qualified as Map
import Language.Haskell.TH.Quote

newtype PyObject = PyObject (ForeignPtr PyObject)
