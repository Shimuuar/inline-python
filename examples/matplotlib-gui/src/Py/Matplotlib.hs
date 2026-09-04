{-# LANGUAGE QuasiQuotes #-}
-- |
module Py.Matplotlib where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

import Python.Inline
import Python.Inline.QQ
import Python.Inline.Eval
import Py.Extra


----------------------------------------------------------------
-- Matplotlib monad
----------------------------------------------------------------

-- | Monad for interaction with matplotlib. It ensures that 
newtype Matplotlib a = Matplotlib { unMatplotlib :: MatplotlibCtx -> Py a }
  deriving stock Functor
  deriving (Applicative,Monad,MonadIO) via ReaderT MatplotlibCtx Py

instance MonadPy Matplotlib where
  liftPy py = Matplotlib $ \_ -> py
  usingGlobalLocal action = Matplotlib $ \ctx ->
    (action ctx.globals ctx.locals).unMatplotlib ctx

runMatplotlib :: MatplotlibCtx -> Matplotlib a -> Py a
runMatplotlib ctx (Matplotlib f) = f ctx

data MatplotlibCtx = MatplotlibCtx
  { globals :: Dict
  , locals  :: Dict
  , figure  :: PyObject
  }


newMatplotlibCtx
  :: PyObject -- ^ Figure object
  -> Py MatplotlibCtx
newMatplotlibCtx fig = do
  globals <- Dict <$> [pye| {} |]
  locals  <- Dict <$> [pye| {} |]
  -- Populate globals
  exec globals globals [pycode|
    import matplotlib        as mpl
    import matplotlib.pyplot as plt
    import numpy             as np

    fig = fig_hs
    ax  = fig_hs.add_subplot(1,1,1)
    |]
  -- Run
  pure MatplotlibCtx
    { globals = globals
    , locals  = locals
    , figure  = fig
    }
