{-# LANGUAGE QuasiQuotes #-}

-- {-# OPTIONS_GHC -ddump-splices #-}
module Main where

import Control.Exception
import Python.Inline
import Python.Inline.QQ

main :: IO ()
main = withPython $ do
  [py|
     x = 3
     y = 1000
     import math
     import gc
     import sys
     print(x+y,(x,y))
     |]
  --
  -- let z = 1000000 :: Int
  --     q = 1.2     :: Double
  -- r1 <- [pye|x * y + z_hs|]
  -- print =<< fromPy @Int    r1
  -- print =<< fromPy @Double r1
  -- r2 <- [pye| math.sin(q_hs) |]
  -- print =<< fromPy @Int    r2
  -- print =<< fromPy @Double r2
  let sin_ = pure @IO . sin @Double
      foo :: Int -> Int -> IO Int
      foo a b = pure $ 1000*a+b
  [py|
     try:
         print( sin__hs(()))
     except Exception() as e:
         print("OOPS", e)
     |] `catch` (\(e::PyError) -> print ("OUCH",e))
  [py| print(foo_hs(1,2)) |]
