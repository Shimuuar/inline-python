{-# LANGUAGE QuasiQuotes #-}
module Main where

import Python
import Python.QQ
import Python.Literal

main :: IO ()
main = withPython $ do
  [py|
     x = 3
     y = 100
     print(x+y,(x,y))
     |]
  --
  r <- [pye|x * y|]
  print =<< fromPy @Int r
