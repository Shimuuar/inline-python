-- |
-- Tests for variable scope and names
module TST.Run(tests) where

import Control.Monad
import Test.Tasty
import Test.Tasty.HUnit
import Python.Inline
import Python.Inline.QQ
import TST.Util

tests :: TestTree
tests = testGroup "Run python"
  [ testCase "Empty QQ" [py_| |]
  , testCase "Second init is noop" initializePython
  , testCase "Python exceptions are converted" $ throwsPy [py_| 1 / 0 |]
  , testCase "Scope pymain->any" $ do
      [pymain|
             x = 12
             x
             |]
      -- Visible
      [py_| x |]
      _ <- [pye| x |]
      [pymain|
        x
        del x
        |]
      -- Disappears
      [pymain|
        try:
            x
            assert False, "x shouln't be visible"
        except NameError:
            pass
        |]
      [py_|
        try:
            x
            assert False, "x shouln't be visible"
        except NameError:
            pass
        |]
  , testCase "Scope py_->any" $ do
      [py_|
        x = 12
        x
        |]
      -- Not visible
      throwsPy $ void [pye| x |]
      [py_|
        try:
            x
            assert False, "x shouln't be visible (1)"
        except NameError:
            pass
        |]
      [pymain|
        try:
            x
            assert False, "x shouln't be visible (2)"
        except NameError:
            pass
        |]
  , testCase "Import py_->any" $ do
      [py_|
        import sys
        sys
        |]
      -- Not visible
      throwsPy $ void [pye| sys |]
      [py_|
        try:
            sys
            assert False, "sys shouln't be visible (1)"
        except NameError:
            pass
        |]
      [pymain|
        try:
            sys
            assert False, "sys shouln't be visible (2)"
        except NameError:
            pass
        |]
  , testCase "Scope pyf->any" $ do
      _ <- [pyf|
        x = 12
        x
        return 12
        |]
      -- Not visible
      throwsPy $ void [pye| x |]
      [py_|
        try:
            x
            assert False, "x shouln't be visible (1)"
        except NameError:
            pass
        |]
      [pymain|
        try:
            x
            assert False, "x shouln't be visible (2)"
        except NameError:
            pass
        |]
  ]
