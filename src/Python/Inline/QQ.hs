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
--
--
-- == Variable scope
--
-- Python has two copes: global and local variables. Both are simply
-- @dict[str,Any]@. Quasiquoters use different dictionaries for
-- globals and locals. If tighter control over variables scope is
-- required APIs from "Python.Inline.Eval" should be used instead.
module Python.Inline.QQ
  ( -- * Evalution in @Py@ monad
    pymain
  , py_
  , pye
  , pyf
    -- * Creation of @PyQuote@
    -- $PyQuote
  , pycode
  , pyfun
  , pySource
  ) where

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax qualified as TH

import Python.Internal.EvalQQ
import Python.Internal.Eval
import Python.Internal.Types


-- | Evaluate sequence of python statements. It uses python's @exec@.
--   Both global and local scope for this quasiquoter are variables of
--   @\__main__@ module. Any variables including imported modules will
--   remain visible to later quasiquotes.
--
--   It creates value of type @Py ()@
pymain :: QuasiQuoter
pymain = QuasiQuoter
  { quoteExp  = \txt -> [| exec Main Main $(expQQ Exec txt) |]
  , quotePat  = error "quotePat"
  , quoteType = error "quoteType"
  , quoteDec  = error "quoteDec"
  }

-- | Evaluate sequence of python statements. Global variables for this
--   quasiquoter are one defined in @\__main__@ module and locals use
--   newly allocated dictionary. It will be discarded after execution
--   so variables defined in this quasiquote are visible only inside
--   of it.
--
--   It creates value of type @Py ()@
py_ :: QuasiQuoter
py_ = QuasiQuoter
  { quoteExp  = \txt -> [| exec Main Temp $(expQQ Exec txt) |]
  , quotePat  = error "quotePat"
  , quoteType = error "quoteType"
  , quoteDec  = error "quoteDec"
  }

-- | Evaluate single python expression. It only accepts single
--   expressions same as python's @eval@. Its globals are variables in
--   @\__main__@ module and locals are new dictionary same as in @py_@.
--
--   This quote creates object of type @Py PyObject@
pye :: QuasiQuoter
pye = QuasiQuoter
  { quoteExp  = \txt -> [| eval Main Temp $(expQQ Eval txt) |]
  , quotePat  = error "quotePat"
  , quoteType = error "quoteType"
  , quoteDec  = error "quoteDec"
  }

-- | Another quasiquoter which works around that sequence of python
--   statements doesn't have any value associated with it. Content of
--   quasiquote is function body. So to get value out of it one must
--   call return. Its globals are variables in @\__main__@ module and
--   locals are new dictionary same as in @py_@.
--
--   This quote creates object of type @Py PyObject@
pyf :: QuasiQuoter
pyf = QuasiQuoter
  { quoteExp  = \txt -> [| evalPyFunction Main $(expQQ Fun txt) |]
  , quotePat  = error "quotePat"
  , quoteType = error "quoteType"
  , quoteDec  = error "quoteDec"
  }

-- $PyQuote
--
-- 'PyQuote' wraps python code and bould haskell variables. It could
-- be evaluated using 'Python.Inline.Eval.exec',
-- 'Python.Inline.Eval.eval', 'Python.Inline.Eval.evalPyFunction'.


-- | Create quote of python code. It captures haskell variables in the
--   same way as rest of quasiquotes and creates value of type
--   'Python.Inline.Eval.PyQuote'.
--
--   @since 0.2@
pycode :: QuasiQuoter
pycode = QuasiQuoter
  { quoteExp  = \txt -> expQQ Exec txt
  , quotePat  = error "quotePat"
  , quoteType = error "quoteType"
  , quoteDec  = error "quoteDec"
  }

-- | Create quote of python code suitable for use with
--   'Python.Inline.Eval.exec'
--
--   It creates value of type @PyQuoteFun@
--
--   @since 0.3@
pyfun :: QuasiQuoter
pyfun = QuasiQuoter
  { quoteExp  = \txt -> expQQ Fun txt
  , quotePat  = error "quotePat"
  , quoteType = error "quoteType"
  , quoteDec  = error "quoteDec"
  }

-- | Create value of type 'PyQuote' from file. Created quote doesn't
--   capture any variables.
--
--   @since 0.3
pySource :: FilePath -> TH.Q TH.Exp
pySource path = do
  TH.addDependentFile path
  [| PyQuote { code   = $(TH.lift =<< TH.runIO (codeFromString <$> readFile path))
             , binder = mempty
             } |]

