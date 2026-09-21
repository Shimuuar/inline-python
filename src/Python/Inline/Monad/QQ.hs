{-# LANGUAGE TemplateHaskell #-}
-- |
-- These quasiquotes are analogous to ones defined in
-- "Python.Inline.QQ" but they produce splices which are polymorphic
-- in 'MonadPy'. For 'Py' in particular they behave identically. 
module Python.Inline.Monad.QQ
  ( -- * Evalution in @Py@ monad
    pymain
  , py_
  , pye
  , pyf
    -- * Creation of @PyQuote@
  , pycode
  , pyfun
  , pySource
  ) where


import Language.Haskell.TH.Quote

import Python.Internal.EvalQQ
import Python.Internal.Eval
import Python.Inline.Monad
import Python.Inline.QQ (pycode, pyfun, pySource)



-- | Evaluate sequence of python statements. It uses python's @exec@.
--   Both global and local scope for this quasiquoter are global
--   variables for 'MonadPy'
--
--   It creates value of type @MonadPy m => m ()@
pymain :: QuasiQuoter
pymain = QuasiQuoter
  { quoteExp  = \txt -> [|
     withGlobals $ \globals -> 
       liftPy $ exec globals globals $(expQQ Exec txt)
     |]
  , quotePat  = error "quotePat"
  , quoteType = error "quoteType"
  , quoteDec  = error "quoteDec"
  }

-- | Evaluate sequence of python statements. Global and local
--   variables for this quasiquoter are determined by 'MonadPy'
--   instance.
--
--   It creates value of type @MonadPy m => m ()@
py_ :: QuasiQuoter
py_ = QuasiQuoter
  { quoteExp  = \txt -> [|
      withGlobals $ \globals ->
        withLocals $ \locals -> 
          liftPy $ exec globals locals $(expQQ Exec txt)
      |]
  , quotePat  = error "quotePat"
  , quoteType = error "quoteType"
  , quoteDec  = error "quoteDec"
  }

-- | Evaluate single python expression. It only accepts single
--   expressions same as python's @eval@. Its globals are variables
--   are determined by 'MonadPy' instance.
--
--   This quote creates object of type @MonadPy m => m PyObject@
pye :: QuasiQuoter
pye = QuasiQuoter
  { quoteExp  = \txt -> [|
      withGlobals $ \globals ->
        withLocals $ \locals -> 
          liftPy $ eval globals locals $(expQQ Eval txt)
      |]
  , quotePat  = error "quotePat"
  , quoteType = error "quoteType"
  , quoteDec  = error "quoteDec"
  }

-- | Another quasiquoter which works around that sequence of python
--   statements doesn't have any value associated with it. Content of
--   quasiquote is function body. So to get value out of it one must
--   call return. Its globals are determined by 'MonadPy' instance. 
--   Just like python function it always creates new scope.
--
--   This quote creates object of type @Py PyObject@
pyf :: QuasiQuoter
pyf = QuasiQuoter
  { quoteExp  = \txt -> [|
      withGlobals $ \globals ->
        liftPy $ evalPyFunction globals $(expQQ Fun txt)
      |]
  , quotePat  = error "quotePat"
  , quoteType = error "quoteType"
  , quoteDec  = error "quoteDec"
  }
