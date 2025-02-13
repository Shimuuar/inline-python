-- |
module TST.Callbacks (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Python.Inline
import Python.Inline.QQ

import TST.Util

tests :: TestTree
tests = testGroup "Callbacks"
  [ testCase "Function(arity 0)" $ runPy $ do
      let double = pure 2 :: IO Int
      [py_|
         # OK
         assert double_hs() == 2
         # Wrong arg number
         try:
             double_hs(1,2,3)
         except TypeError as e:
             pass
         |]
  , testCase "Function(arity=1)" $ runPy $ do
      let double = pure . (*2) :: Int -> IO Int
      [py_|
         # OK
         assert double_hs(3) == 6
         # Invalid arg
         try:
             double_hs(None)
         except TypeError as e:
             pass
         # Wrong arg number
         try:
             double_hs(1,2,3)
         except TypeError as e:
             pass
         |]
  , testCase "Function(arity=2)" $ runPy $ do
     let foo :: Int -> Double -> IO Int
         foo x y = pure $ x + round y
     [py_|
         assert foo_hs(3, 100.2) == 103
         assert foo_hs(3, 100)   == 103
         # Invalid arg
         try:
             foo_hs(None, 100)
         except TypeError as e:
             pass
         # Wrong arg number
         try:
             foo_hs(1,2,3)
         except TypeError as e:
             pass
         |]
    ----------------------------------------
  , testCase "Py function(arity 0)" $ do
      let fun = [pye| 0 |]
      runPy [py_| assert fun_hs() == 0 |]
  , testCase "Py function(arity=1)" $ runPy $ do
      let double (n::Int) = [pye| n_hs * 2 |]
      [py_| assert double_hs(3) == 6 |]
  , testCase "Py function(arity=2)" $ runPy $ do
      let foo (x::Int) (y::Int) = [pye| x_hs * y_hs |]
      [py_| assert foo_hs(3, 100) == 300 |]
    ----------------------------------------
  , testCase "Haskell exception in callback(arity=1)" $ runPy $ do
       let foo :: Int -> IO Int
           foo y = pure $ 10 `div` y
       throwsPy [py_| foo_hs(0) |]
  , testCase "Haskell exception in callback(arity=2)" $ runPy $ do
      let foo :: Int -> Int -> IO Int
          foo x y = pure $ x `div` y
      throwsPy [py_| foo_hs(1, 0) |]
    ----------------------------------------
  , testCase "Call python in callback (arity=1)" $ runPy $ do
      let foo :: Int -> IO Int
          foo x = do Just x' <- runPy $ fromPy =<< [pye| 100 // x_hs |]
                     pure x'
      [py_|
        assert foo_hs(5) == 20
        |]
  , testCase "Call python in callback (arity=2" $ runPy $ do
      let foo :: Int -> Int -> IO Int
          foo x y = do Just x' <- runPy $ fromPy =<< [pye| x_hs // y_hs |]
                       pure x'
      [py_|
        assert foo_hs(100,5) == 20
        |]
    ----------------------------------------
  , testCase "runPyInMain in runPyInMain (arity=1)" $ do
      let foo :: Int -> IO Int
          foo x = do Just x' <- runPyInMain $ fromPy =<< [pye| 100 // x_hs |]
                     pure x'
      runPyInMain [py_|
        assert foo_hs(5) == 20
        |]
  , testCase "runPyInMain in runPy (arity=1)" $ do
      let foo :: Int -> IO Int
          foo x = do Just x' <- runPyInMain $ fromPy =<< [pye| 100 // x_hs |]
                     pure x'
      runPy [py_|
        assert foo_hs(5) == 20
        |]
    ----------------------------------------
  , testCase "No leaks (arity=1)" $ runPy $ do
      let foo :: Int -> IO Int
          foo y = pure $ 10 * y
      [py_|
        import sys
        x = 123456
        old_refcount = sys.getrefcount(x)
        foo_hs(x)
        assert old_refcount == sys.getrefcount(x)
        |]
  , testCase "No leaks (arity=2)" $ runPy $ do
      let foo :: Int -> Int -> IO Int
          foo x y = pure $ x * y
      [py_|
        import sys
        x = 123456
        old_refcount = sys.getrefcount(x)
        foo_hs(1,x)
        assert old_refcount == sys.getrefcount(x)
        |]
  ]
