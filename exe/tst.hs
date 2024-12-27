{-# LANGUAGE QuasiQuotes #-}
module Main where

import Python
import Python.QQ

main :: IO ()
main = withPython $ do
  [py|
     x = 3
     y = 100
     print(x+y,(x,y))
     |]
  --
  [py|
     print(x*y,(x,y))
     |]
  print("----")
