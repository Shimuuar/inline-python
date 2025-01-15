-- | This library allows to embed as quasiquotes and execute arbitrary
-- python code in haskell programs. Take for example following program:
--
-- > {-# LANGUAGE QuasiQuotes #-}
-- > import Python.Inline
-- > import Python.Inline.QQ
-- >
-- > main :: IO ()
-- > main = withPython $ do
-- >   let input = [1..10] :: [Int]
-- >   let square :: Int -> Py Int
-- >       square x = pure (x * x)
-- >   print =<< runPy $ do
-- >     fromPy' @[Int] =<< [pye| [ square_hs(x) for x in input_hs ] |]
--
-- Quasiquotation 'Python.Inline.QQ.pye' captures variables @input@
-- and @square@ from environment and produces python object which
-- `fromPy'` converts to haskell list. As one expect it would output:
--
-- > [1,4,9,16,25,36,49,64,81,100]
--
-- Module "Python.Inline.QQ" provides several quasiquoters with
-- different semantics but general rules are:
--
--  1. All python variables ending with @_hs@ are captured from
--     environment and converted to python objects according to their
--     'ToPy' instance.
--
--  2. Syntax errors in embedded python will be caught during
--     compilation.
--
--  3. All code interacting with python must be in 'Py' monad which
--     could be run using 'runPy'.
--
--  4. Python interpreter must be initialized before calling any
--     python code.
module Python.Inline
  ( -- * Interpreter initialization
    -- $initialization
    initializePython
  , finalizePython
  , withPython
    -- * Core data types
  , Py
  , runPy
  , runPyInMain
  , PyObject
  , PyError(..)
  , PyException(..)
    -- * Conversion between haskell and python
    -- $conversion
  , toPy
  , fromPyEither
  , fromPy
  , fromPy'
  , ToPy
  , FromPy
    -- * Troubleshooting
    -- $troubleshooting
  ) where

import Python.Inline.Literal
import Python.Internal.Types
import Python.Internal.Eval


-- $initialization
--
-- Python supports being initialized and shut down multiple times. 
-- This however has caveats. Quoting it documentation:
--
-- >  Bugs and caveats: The destruction of modules and objects in
-- >  modules is done in random order; this may cause destructors
-- >  (__del__() methods) to fail when they depend on other objects
-- >  (even functions) or modules. Dynamically loaded extension
-- >  modules loaded by Python are not unloaded. Small amounts of
-- >  memory allocated by the Python interpreter may not be freed (if
-- >  you find a leak, please report it). Memory tied up in circular
-- >  references between objects is not freed. Some memory allocated
-- >  by extension modules may not be freed. Some extensions may not
-- >  work properly if their initialization routine is called more
-- >  than once.
--
-- More importantly for this library. All pointers held by 'PyObject'
-- becomes invalid after interpreter is shut down. If GC tries to run
-- finalizers after interpreter is intialized again program will
-- surely segfault.
--
-- For that reason it's only possible to initialize python once and
-- attempts to initialize python after is was shut down will raise
-- exceptions.


-- $conversion
--
-- Python objects are opaque blobs and accessing them may involve
-- running arbitrary python code. Most notable iteration protocol or
-- any of dunder methods. For that reason conversion from python to
-- haskell must happen in 'Py' monad. Conversion also always performs
-- full copy. Conversion from haskell to python is stateful as well.


-- $troubleshooting
--
-- Here's list of common problems and solutions and workarounds.
--
-- 1. __@inline-python@ cannot find libraries__
--
-- @inline-python@ may look for modules in wrong place. Set
-- environment variables @PYTHONHOME@ or @PYTHONPATH@ to point it
-- right way.
--
--
-- 2. __Linker error in GHCi__
--
-- Attempting to import library using C extensions from ghci may
-- result in linker failing to find symbols from @libpython@ like
-- @PyFloat_Type@ or some other. Only known workaround is to set
-- @LD_PRELOAD=/path/to/libpython3.XX.so@ environment variable.
