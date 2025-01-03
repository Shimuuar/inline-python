{-# LANGUAGE QuasiQuotes #-}
module Main where

import Test.Tasty       hiding (defaultMain)
import Test.Tasty.Bench

import Python.Inline
import Python.Inline.QQ


main :: IO ()
main = withPython $ do
  py_int <- [pye| 123456 |]
  defaultMain
    [ bench "FromPy Int" $ whnfIO $ fromPy' @Int py_int
    ]
