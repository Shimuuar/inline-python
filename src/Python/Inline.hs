-- | This library allows to embed as quasiquotes and execute arbitrary
-- python code in haskell programs. Take for example following program:
--
-- > {-# LANGUAGE QuasiQuotes #-}
-- > import Control.Monad
-- > import Python.Inline
-- > import Python.Inline.QQ
-- >
-- > main :: IO ()
-- > main = withPython $ do
-- >   let input = [1..10] :: [Int]
-- >   let square :: Int -> Py Int
-- >       square x = pure (x * x)
-- >   print <=< runPy $ do
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
--
-- 1. __@inline-python@ cannot find libraries__
--
-- @inline-python@ may look for modules in wrong place. Set
-- environment variables @PYTHONHOME@ or @PYTHONPATH@ to point it
-- right way.
--
--
-- 2. __Picking correct python interpreter__
--
-- Python's version @inline-python@ uses is determined by @libpython3@
-- it's linked with. This is decided when package is build. Normally
-- @pkg-config@ is used and this means using whatever distribution is
-- shipping. It's also possible to use @python3-config@ program by
-- specifying manual cabal flag @-fpython3-config@.
--
-- If it's desired that @inline-python@ should use python installed by
-- conda\/uv\/etc @inline-python@ should be built in environment where
-- desired python version is active and use @-fpython3-config@ flag.
-- This could be done by adding following to cabal.project:
--
-- > constraints: inline-python -fpython3-config
--
--
-- 3. __Linker error in GHCi__
--
-- Attempting to import library using C extensions from ghci may
-- result in linker failing to find symbols from @libpython3@ like
-- @PyFloat_Type@ or some other. There are multiples known
-- workarounds. @libpython3.XX.so@ should be one @inline-python@ was
-- built with.
--
-- - export @LD_PRELOAD=\/path\/to\/libpython3.XX.so@ environment variable. This
-- works fine most of the time but it will also impact programs called from
-- your repl (e.g. using 'process').
--
-- - you can load the relevant symbol only in the current process space using
-- 
-- > System.Posix.DynamicLinker.dlopen "/path/to/libpython3.XX.so" [System.Posix.DynamicLinker.RTLD_NOW, System.Posix.DynamicLinker.RTLD_GLOBAL]
--
-- Note that in ghci, you don't need to explicitly import
-- "System.Posix.DynamicLinker" to run this command. If you use this
-- extensively in your project, it is recommended to add that in your
-- @.ghci@, global or local, either as an unconditionally executed
-- command, or as a macro, such as:
--
-- > :def setupPythonDLL \_ -> "" <$ System.Posix.DynamicLinker.dlopen "path/to//lib/libpython3.so" [System.Posix.DynamicLinker.RTLD_NOW, System.Posix.DynamicLinker.RTLD_GLOBAL]@
--
-- 4. __GHCi reload__
--
-- Even if you reload ghci (using @:reload@), the python environment stays
-- initialised. As a result, the python modules imported using 'Python.Inline.QQ.pymain' won't
-- be reloaded, which is often source of confusion.
--
-- You can use python's @importlib.reload(m)@. For example:
--
-- >
-- >         [pymain|
-- >           import json
-- >           import importlib
-- >           import pandas
-- >           import mylib
-- >
-- >           # This force reloads mylib
-- >           importlib.reload(mylib)
-- >       |]
-- 
--
-- Note that there is a performance drawback, the side effects of 'import' are
-- redone and python does not give much guarantee about what is happening here.
-- Use it with caution. We recommend using @importlib.reload@ only during
-- development and not in production.
--
-- 5. __Asynchronous exceptions__
--
-- The code run by 'runPy' is not interruptible by Haskell asynchronous
-- exceptions and may block indefinitely. If your code call any Haskell
-- function as callback, they won't receive asynchronous exception either. See
-- https://github.com/Shimuuar/inline-python/issues/48 for details and
-- workarounds.
