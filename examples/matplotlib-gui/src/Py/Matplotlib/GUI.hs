{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
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
import Control.Exception
import Control.Monad
import Data.Function
import Foreign.C.Types
import System.Posix.Types as Unix
import System.Posix.IO    as Unix
import System.IO
import Language.Haskell.TH.Syntax qualified as TH

import Python.Inline
import Python.Inline.QQ
import Python.Inline.Eval
import Py.Matplotlib


----------------------------------------------------------------
-- Sending signals to python app
----------------------------------------------------------------

-- | Pipe which is used for sending signals to python side
data PyPipe = PyPipe
  { read  :: !Fd     -- ^ Read end. Python reads from it.
  , write :: !Handle -- ^ Write end. Haskell writes to it.
  }

-- | Bracket for creatiob of pipe
withPipe :: (PyPipe -> IO a) -> IO a
withPipe action = bracket Unix.createPipe fini $ \(h_read, h_write) -> do
  h <- Unix.fdToHandle h_write
  action PyPipe{ read = h_read, write = h }
  where
    fini (h1,h2) = Unix.closeFd h1 `finally` Unix.closeFd h2

-- | Get number of file descriptor of read end.
pyPipeReadFD :: PyPipe -> CInt
pyPipeReadFD p = let Fd k = p.read in k

-- | Ask GUI to pause execution and to retun control to haskell side.
interruptGUI :: PyPipe -> IO ()
interruptGUI p = hPutStr p.write "s" >> hFlush p.write

-- | Ask GUI to stop application and to retun control to haskell side.
stopGUI :: PyPipe -> IO ()
stopGUI p = hPutStr p.write "q" >> hFlush p.write


----------------------------------------------------------------
-- Async commands
----------------------------------------------------------------

newtype CallResult a = CallResult (MVar (Either SomeException a))

newCallResult :: IO (CallResult a)
newCallResult = CallResult <$> newEmptyMVar

waitCallResult :: CallResult a -> IO a
waitCallResult (CallResult v) = takeMVar v >>= \case
  Left  e -> throwIO e
  Right a -> pure a

performCall :: CallResult a -> IO a -> IO ()
performCall (CallResult v) io = do
  putMVar v =<< try io


----------------------------------------------------------------
-- Controlling GUI
----------------------------------------------------------------

-- | Current stat of GUI accoring to haskell side
data GuiState
  = NotStarted      -- ^ GUI hasn't beed started yet
  | TkMainLoop      -- ^ Tk main loop is running
  | TkInterruptSent -- ^ Interrupt requested
  | TkStopSent      -- ^ Stop of GUI is requested
  deriving Show

-- | Command sent for GUI thread
data Command
  = Plot (Py ()) !(CallResult ())

-- | Handle for interactions with GUI
data GUI = GUI
  { chan :: TMVar Command
  , st   :: TVar  GuiState
  , pipe :: !PyPipe
  , app  :: PyObject
  }


-- | Thread which calls GUI and perform interactions with it
guiThread
  :: GUI      -- ^ Handle
  -> IO ()
guiThread gui = fix $ \loop -> do
  -- State transition 
  atomically $ do
    readTVar gui.st >>= \case
      NotStarted      -> writeTVar gui.st TkMainLoop
      TkMainLoop      -> error "guiThread: internal error"
      TkInterruptSent -> writeTVar gui.st TkMainLoop
      TkStopSent      -> signalExit
  -- Enter main loop
  runPyInMain [py_| app_hs.mainloop() |]
  runPy (fromPy' =<< [pye| app_hs.exit_reason is None |]) >>= \case
    -- GUI is stopped on python side. We need to exit and signal error
    -- to main thread
    True  -> do
      runPyInMain [py_| app_hs.root.destroy() |]
      signalExit
    -- We interrupted UI
    False -> do
      Plot py lock <- atomically (takeTMVar gui.chan)
      performCall lock $ runPyInMain py
      loop
  where
    app = gui.app
    signalExit = error "GUI stopped"

doPlot :: GUI -> Py () -> IO ()
doPlot gui py = do
  lock <- newCallResult
  atomically $ do
    readTVar gui.st >>= \case
      NotStarted      -> retry
      TkMainLoop      -> writeTVar gui.st TkInterruptSent
      TkInterruptSent -> retry
      TkStopSent      -> error "GUI stopped"
    putTMVar gui.chan $ Plot py lock
  interruptGUI gui.pipe
  waitCallResult lock


withMatplotlibGUI
  :: ((Matplotlib () -> IO ()) -> IO a) -- ^ Function for calling plotting library
  -> IO a
withMatplotlibGUI callback = withPipe $ \pipe -> do
  -- Make sure python is initialized
  initializePython
  -- Load python adapter
  runPyInMain $ do
    Module mdl <- loadOkaMpl
    [pymain|
       import matplotlib as mpl
       okampl = mdl_hs
       |]
  -- Set up all threads
  chan      <- newEmptyTMVarIO
  var_state <- newTVarIO NotStarted
  app       <- let h = pyPipeReadFD pipe in runPyInMain [pye| okampl.App(h_hs) |]
  ctx       <- runPyInMain $ newMatplotlibCtx =<< [pye| app_hs.fig |]
  let gui = GUI { chan = chan
                , st   = var_state
                , pipe = pipe
                , app  = app
                }
  let stop = join $ atomically $ do
        readTVar gui.st >>= \case
          NotStarted      -> pure $ pure ()
          TkMainLoop      -> do writeTVar gui.st TkStopSent
                                pure $ stopGUI gui.pipe
          TkInterruptSent -> do writeTVar gui.st TkStopSent
                                pure $ stopGUI gui.pipe
          TkStopSent      -> pure $ pure ()
  withAsync (guiThread gui) $ \a_gui ->
    do link a_gui
       a <- callback (doPlot gui . runMatplotlib ctx)
       a <$ stop
     `onException` stop



----------------------------------------------------------------
-- Using python wrapper
----------------------------------------------------------------

okampl_code :: PyQuote
okampl_code = PyQuote
  { code = $(
      do let path = "py/okampl.py"
         TH.addDependentFile path
         TH.lift =<< TH.runIO (codeFromString <$> readFile path)
      )
  , binder = mempty
  }

loadOkaMpl :: Py Module
loadOkaMpl = do
  mdl <- [pyf|
    import importlib.util
    spec = importlib.util.spec_from_loader("dyn", loader=None)
    return importlib.util.module_from_spec(spec)
    |]
  exec (Module mdl) (Module mdl) okampl_code
  return (Module mdl)
