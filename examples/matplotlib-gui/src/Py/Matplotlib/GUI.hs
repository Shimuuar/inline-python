{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ViewPatterns          #-}
-- |
-- Utilities for working with Tk-based matplotlib UI.
--
-- Main trouble with Tk based UI is: on the one hand we need to let
-- Tk main loop to take control of the UI. On the other we need to
-- return control to haskell side to perform plotting. Interruption is
-- done by writing char into pipe.
module Py.Matplotlib.GUI
  ( withMatplotlibGUI
  ) where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception       (SomeAsyncException(..))
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Typeable
import Data.Function
import Language.Haskell.TH.Syntax qualified as TH

import Python.Inline
import Python.Inline.QQ
import Python.Inline.Eval
import Py.Matplotlib




----------------------------------------------------------------
-- Async commands
----------------------------------------------------------------

newtype CallResult a = CallResult (MVar (Either SomeException a))

newCallResult :: IO (CallResult a)
newCallResult = CallResult <$> newEmptyMVar

waitCallResult :: CallResult a -> IO a
waitCallResult (CallResult v) = takeMVar v >>= \case
  Left  e -> throwM e
  Right a -> pure a


----------------------------------------------------------------
-- Controlling GUI
----------------------------------------------------------------

-- | Command sent for GUI thread
data Command
  = Plot (Py ()) !(CallResult ())

-- | Handle for interactions with GUI
data GUI = GUI
  { chan :: TMVar Command  -- ^ 1-message channel to send message to GUI thread
  , app  :: PyObject       -- ^ Tkinter application
  }

doPlot :: GUI -> Py () -> IO ()
doPlot gui py = do
  lock <- newCallResult
  atomically $ putTMVar gui.chan $ Plot py lock
  interruptGUI   gui
  waitCallResult lock


interruptGUI :: GUI -> IO ()
interruptGUI GUI{app} = runPy [py_| app_hs.root.event_generate('<<PLOT>>', when='tail') |]

withMatplotlibGUI
  :: ((Matplotlib () -> IO ()) -> IO a) -- ^ Function for calling plotting library
  -> IO a
withMatplotlibGUI callback = do
  initializePython
  -- Load python adapter
  runPyInMain $ do
    mdl <- createModule "mplgui" $(pySource "py/mplgui.py")
    [pymain|
       import matplotlib as mpl
       mplgui = mdl_hs
       |]
  bracket startGUI stopGUI $ \(gui, ctx) -> do
    withAsync (runPyInMain $ guiThread gui) $ \a_gui -> do
      link a_gui
      handle (bonk gui.app)
        $ callback $ (doPlot gui . runMatplotlib ctx)
  where
    bonk app (SomeException e)
      -- Sigh. We need to jump through these hoops...
      | Just (SomeAsyncException e')      <- cast e
      = bonk app (SomeException e')
      -- Don't throw exception back!
      | Just (_::ExceptionInLinkedThread) <- cast e
      = throwM e
      -- Else we need to generate tkinter event to make sure that app
      -- exits tkinter main loop into python callback and is able to
      -- receive async exception
      | otherwise
      = do runPy [py_| app_hs.root.event_generate('<<BONK>>', when='tail') |]
           throwM e



-- | Thread which calls GUI and perform interactions with it
guiThread
  :: GUI      -- ^ Handle
  -> Py ()
guiThread gui = fix $ \loop -> do
  -- Enter main loop
  [py_| app_hs.mainloop() |]
  (fromPy' =<< [pye| app_hs.exit_reason is None |]) >>= \case
    -- GUI is stopped on python side.
    True  -> error "GUI stopped"
    -- We interrupted by signal
    False -> do
      Plot py (CallResult lock) <- liftIO $ atomically (takeTMVar gui.chan)
      liftIO . putMVar lock =<< try py
      loop
  where
    app = gui.app


startGUI :: IO (GUI, MatplotlibCtx)
startGUI = do
  chan <- newEmptyTMVarIO
  app  <- runPyInMain [pye| mplgui.App() |]
  ctx  <- runPyInMain $ newMatplotlibCtx =<< [pye| app_hs.fig |]
  let gui = GUI { chan = chan
                , app  = app
                }
  return (gui, ctx)

stopGUI :: (GUI,MatplotlibCtx) -> IO ()
stopGUI (GUI{app},_)= do
  runPyInMain [py_| app_hs.root.destroy() |]
