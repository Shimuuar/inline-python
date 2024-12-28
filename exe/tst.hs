{-# LANGUAGE QuasiQuotes #-}

-- {-# OPTIONS_GHC -ddump-splices #-}
module Main where

import Python.Inline
import Python.Inline.QQ

main :: IO ()
main = withPython $ do
  [py|
     x = 3
     y = 100
     import math
     print(x+y,(x,y))
     |]
  --
  let z = 1000000 :: Int
      q = 1.2     :: Double
  r1 <- [pye|x * y + z_hs|]
  print =<< fromPy @Int    r1
  print =<< fromPy @Double r1
  [py| print(type(math.sin(q_hs))) |]
  r2 <- [pye| math.sin(q_hs) |]
  print =<< fromPy @Int    r2
  print =<< fromPy @Double r2
