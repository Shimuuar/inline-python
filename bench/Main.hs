{-# LANGUAGE QuasiQuotes #-}
module Main where

import Test.Tasty.Bench

import Python.Inline
import Python.Inline.QQ


main :: IO ()
main = withPython $ do
  py_int <- runPy [pye| 123456 |]
  defaultMain
    [ bench "FromPy Int" $ whnfIO $ runPy $ fromPy' @Int py_int
    ]
