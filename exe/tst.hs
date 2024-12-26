module Main where

import Python

main :: IO ()
main = do
  print sizeof_PyConfig
  -- 
  cfg <- py_new_PyConfig
  py_InitializeFromConfig cfg
  py_Foo
