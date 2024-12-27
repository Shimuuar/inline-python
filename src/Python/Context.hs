{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
-- |
module Python.Context where

import Foreign.Ptr
import Foreign.ForeignPtr
import Language.C.Inline.Context qualified as C
import Language.C.Types   qualified as C
import Data.Map.Strict qualified as Map
import Language.Haskell.TH.Quote

import Python.Types

pyCtx :: C.Context
pyCtx = mempty { C.ctxTypesTable = Map.fromList tytabs } where
  tytabs =
      [ (C.TypeName "PyObject", [t| PyObject |])
--      , (TypeName "Rcomplex", [t| Complex Double |])
      ]
