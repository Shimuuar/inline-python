{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Interface to python's @eval@ and @exec@
module Python.Inline.Eval
  ( -- * Python execution
    eval
  , exec
    -- * Source code
  , PyQuote(..)
  , Namespace(..)
  , Main(..)
  , Temp(..)
    -- ** Data types
  , Code
  , codeFromText
  , codeFromString
  , DictBinder

  ) where

import Foreign.Ptr

import Language.C.Inline          qualified as C
import Language.C.Inline.Unsafe   qualified as CU

import Python.Internal.Types
import Python.Internal.Eval
import Python.Internal.EvalQQ
import Python.Internal.CAPI
import Python.Internal.Program

----------------------------------------------------------------
C.context (C.baseCtx <> pyCtx)
C.include "<inline-python.h>"
----------------------------------------------------------------


-- | Evaluate python expression
eval :: (Namespace global, Namespace local)
     => global  -- ^ Data type providing global variables dictionary
     -> local   -- ^ Data type providing local variables dictionary
     -> PyQuote -- ^ Source code
     -> Py PyObject
eval globals locals q = runProgram $ do
  p_py      <- unsafeWithCode q.code
  p_globals <- takeOwnership =<< progPy (basicNamespaceDict globals)
  p_locals  <- takeOwnership =<< progPy (basicNamespaceDict locals)
  progPy $ do
    q.binder.bind p_locals
    p_res <- Py [C.block| PyObject* {
      PyObject* globals = $(PyObject* p_globals);
      PyObject* locals  = $(PyObject* p_locals);
      // Compile code
      PyObject *code = Py_CompileString($(char* p_py), "<interactive>", Py_eval_input);
      if( PyErr_Occurred() ) {
          return NULL;
      }
      // Evaluate expression
      PyObject* r = PyEval_EvalCode(code, globals, locals);
      Py_DECREF(code);
      return r;
      }|]
    checkThrowPyError
    newPyObject p_res
{-# SPECIALIZE eval :: Main -> Temp -> PyQuote -> Py PyObject #-}

-- | Evaluate sequence of python statements
exec :: (Namespace global, Namespace local)
     => global  -- ^ Data type providing global variables dictionary
     -> local   -- ^ Data type providing local variables dictionary
     -> PyQuote -- ^ Source code
     -> Py ()
exec globals locals q = runProgram $ do
  p_py      <- unsafeWithCode q.code
  p_globals <- takeOwnership =<< progPy (basicNamespaceDict globals)
  p_locals  <- takeOwnership =<< progPy (basicNamespaceDict locals)
  progPy $ do
    q.binder.bind p_locals
    Py[C.block| void {
      PyObject* globals = $(PyObject* p_globals);
      PyObject* locals  = $(PyObject* p_locals);
      // Compile code
      PyObject *code = Py_CompileString($(char* p_py), "<interactive>", Py_file_input);
      if( PyErr_Occurred() ){
          return;
      }
      // Execute statements
      PyObject* res = PyEval_EvalCode(code, globals, locals);
      Py_XDECREF(res);
      Py_DECREF(code);
      } |]
    checkThrowPyError
{-# SPECIALIZE exec :: Main -> Main -> PyQuote -> Py () #-}
{-# SPECIALIZE exec :: Main -> Temp -> PyQuote -> Py () #-}



-- | Type class for values representing python dictionaries containing
--   global or local variables.
class Namespace a where
  -- | Returns dictionary object. Caller takes ownership of returned
  --   object.
  basicNamespaceDict :: a -> Py (Ptr PyObject)


-- | Namespace for the top level code execution.
data Main = Main


instance Namespace Main where
  -- NOTE: dupe of basicMainDict
  basicNamespaceDict _ =
    throwOnNULL =<< Py [CU.block| PyObject* {
      PyObject* main_module = PyImport_AddModule("__main__");
      if( PyErr_Occurred() )
          return NULL;
      PyObject* dict = PyModule_GetDict(main_module);
      Py_XINCREF(dict);
      return dict;
      }|]

-- | Temporary namespace which get destroyed after execution
data Temp = Temp

instance Namespace Temp where
  basicNamespaceDict _ = basicNewDict
