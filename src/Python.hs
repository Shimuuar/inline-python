{-# LANGUAGE TemplateHaskell #-}
-- |
module Python where

import Language.C.Inline qualified as C


C.include "<Python.h>"
