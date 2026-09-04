{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ViewPatterns          #-}
module Main where

import Control.Monad

import Python.Inline
import Python.Inline.Monad.QQ

import Py.Matplotlib.GUI
import Py.Extra




main :: IO ()
main = do
  withMatplotlibGUI $ \runM -> do
    runM [pymain|
      a  = 1.0
      xs = np.linspace(0,1)
      |]
    --
    forever $ do
      print "Press enter"
      runM $ do
        [py_|
            plt.plot(xs,xs**a)
            a *= 1.03
            |]
      _ <- getLine
      return ()

