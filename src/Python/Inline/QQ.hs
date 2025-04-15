{-# LANGUAGE TemplateHaskell #-}
-- |
-- Quasiquoters for embedding python expression into haskell programs.
-- Python is statement oriented and heavily relies on mutable state.
-- This means we need several different quasiquoters.
--
--
-- == Syntax in quasiquotes
--
-- Note on syntax. Python's grammar is indentation sensitive and
-- quasiquote is passed to 'QuasiQuoter' without any adjustment. So
-- this seemingly reasonable code:
--
-- > foo = [py_| do_this()
-- >             do_that()
-- >           |]
--
-- results in following source code.
--
-- >  do_this()
-- >             do_that()
--
-- There's no sensible way to adjust indentation, since we don't know
-- original indentation of first line of quasiquote in haskell's code.
-- Thus rule: __First line of multiline quasiquote must be empty__.
-- This is correct way to write code:
--
-- > foo = [py_|
-- >         do_this()
-- >         do_that()
-- >         |]
module Python.Inline.QQ
  ( pymain
  , py_
  , pye
  , pyf
  ) where

import Language.Haskell.TH.Quote

import Python.Internal.EvalQQ


-- | Evaluate sequence of python statements. It works in the same way
--   as python's @exec@. All module imports and all variables defined
--   in this quasiquote will be visible to later quotes.
--
--   It creates value of type @Py ()@
pymain :: QuasiQuoter
pymain = QuasiQuoter
  { quoteExp  = \txt -> [| uncurry evaluatorPymain $(expQQ Exec txt) |]
  , quotePat  = error "quotePat"
  , quoteType = error "quoteType"
  , quoteDec  = error "quoteDec"
  }

-- | Evaluate sequence of python statements. All module imports and
--   all variables defined in this quasiquote will be discarded and
--   won't be visible in later quotes.
--
--   It creates value of type @Py ()@
py_ :: QuasiQuoter
py_ = QuasiQuoter
  { quoteExp  = \txt -> [| uncurry evaluatorPy_ $(expQQ Exec txt) |]
  , quotePat  = error "quotePat"
  , quoteType = error "quoteType"
  , quoteDec  = error "quoteDec"
  }

-- | Evaluate single python expression. It only accepts single
--   expressions same as python's @eval@.
--
--   This quote creates object of type @Py PyObject@
pye :: QuasiQuoter
pye = QuasiQuoter
  { quoteExp  = \txt -> [| uncurry evaluatorPye $(expQQ Eval txt) |]
  , quotePat  = error "quotePat"
  , quoteType = error "quoteType"
  , quoteDec  = error "quoteDec"
  }

-- | Another quasiquoter which works around that sequence of python
--   statements doesn't have any value associated with it.  Content of
--   quasiquote is function body. So to get value out of it one must
--   call return
pyf :: QuasiQuoter
pyf = QuasiQuoter
  { quoteExp  = \txt -> [| uncurry evaluatorPyf $(expQQ Fun txt) |]
  , quotePat  = error "quotePat"
  , quoteType = error "quoteType"
  , quoteDec  = error "quoteDec"
  }
