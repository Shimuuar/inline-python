{-# LANGUAGE QuasiQuotes #-}

-- {-# OPTIONS_GHC -ddump-splices #-}
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
  let z = 1000000 :: Int
  r <- [pye|x * y + z_hs|]
  print =<< fromPy @Int r
