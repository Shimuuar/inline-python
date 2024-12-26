module Main where

import Python

main :: IO ()
main = withPython $ do
  py_Foo
