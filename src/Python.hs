{-# LANGUAGE CApiFFI                  #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE QuasiQuotes              #-}
{-# LANGUAGE TemplateHaskell          #-}
-- |

-- {-# OPTIONS_GHC -ddump-splices #-}
module Python where

import Control.Exception
import Control.Monad.Trans.Cont
import Control.Monad.IO.Class
import Data.Char
import Data.Map.Strict qualified as Map
import Foreign.Ptr
import Foreign.ForeignPtr
import GHC.ForeignPtr     (unsafeWithForeignPtr)
import Foreign.Marshal
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.C.String
import Foreign.C.Types
import System.Environment
import System.Process
import System.Exit

import Language.C.Inline         qualified as C
import Language.C.Inline.Context qualified as C
import Language.C.Types   qualified as C

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax qualified as TH


import Python.Types
import Python.Context
import Python.Literal

import Paths_inline_python

C.context (C.baseCtx <> pyCtx)
C.include "<inline-python.h>"

----------------------------------------------------------------
-- Interpreter initialization
----------------------------------------------------------------

-- | Initialize python interpreter
initializePython :: IO ()
initializePython = do
  -- NOTE: I'd like more direct access to argv
  argv0 <- getProgName
  argv  <- getArgs
  let n_argv = fromIntegral $ length argv + 1
  -- FIXME: For some reason sys.argv is initialized incorrectly. No
  --        easy way to debug. Will do for now
  r <- evalContT $ do
    p_argv0  <- ContT $ withWCtring argv0
    p_argv   <- traverse (ContT . withWCtring) argv
    ptr_argv <- ContT $ withArray (p_argv0 : p_argv)
    liftIO [C.block| int {
      // Noop is interpreter is already initialized
      if( Py_IsInitialized() ) {
          return 0;
      }
      // Now fill config
      PyStatus status;
      PyConfig cfg;
      PyConfig_InitPythonConfig( &cfg );
      //--
      status = PyConfig_SetBytesString(&cfg, &cfg.program_name, "XX");
      if (PyStatus_Exception(status)) { goto error; }
      //--
      status = PyConfig_SetArgv(&cfg,
          $(int       n_argv),
          $(wchar_t** ptr_argv)
      );
      if( PyStatus_Exception(status) ) { goto error; };
      // Initialize interpreter
      status = Py_InitializeFromConfig(&cfg);
      PyConfig_Clear(&cfg);
      return PyStatus_Exception(status);
      // Error case
      error:
      PyConfig_Clear(&cfg);
      return 1;
      } |]
  case r of 0 -> pure ()
            _ -> error "Failed to initialize interpreter"


finalizePython :: IO ()
finalizePython = [C.exp| void { Py_Finalize(); } |]

withPython :: IO a -> IO a
withPython = bracket_ initializePython finalizePython

----------------------------------------------------------------
-- Objects
----------------------------------------------------------------

newPyObject :: Ptr PyObject -> IO PyObject
newPyObject = fmap PyObject . newForeignPtr py_XDECREF

foreign import capi "inline-python.h &inline_py_XDECREF" py_XDECREF :: FunPtr (Ptr PyObject -> IO ())





data PyError = PyError String
  deriving stock (Show)

instance Exception PyError

pyEvalStr :: String -> IO ()
pyEvalStr py = mask_ $ evalContT $ do
  p_py  <- ContT $ withCString py
  p_err <- ContT $ alloca @(Ptr CChar)
  r <- liftIO
    [C.block| int {
       PyObject *e_type, *e_value, *e_trace;
       // Compile code
       PyObject *code = Py_CompileString($(char* p_py), "<interactive>", Py_file_input);
       if( code == 0 ){
           PyErr_Fetch( &e_type, &e_value, &e_trace);
           inline_py_export_exception(e_type, e_value, e_trace, $(char** p_err));
           return $(int py_err_compile);
       }
       // Execute in context of main
       PyObject* main_module = PyImport_AddModule("__main__");
       PyObject* globals     = PyModule_GetDict(main_module);
       //
       PyObject* r = PyEval_EvalCode(code, globals, globals);
       Py_XDECREF(r);
       if( PyErr_Occurred() ) {
           PyErr_Fetch( &e_type, &e_value, &e_trace);
           inline_py_export_exception(e_type, e_value, e_trace, $(char** p_err));
           return $(int py_err_eval);
       }
       return 0;
       }|]
  liftIO $ case r of
    PY_OK          -> return ()
    PY_ERR_COMPILE -> peek p_err >>= \case
      p | nullPtr == p -> throwIO $ PyError "Compile error"
        | otherwise    -> do
            s <- peekCString p
            throwIO $ PyError s
    PY_ERR_EVAL -> peek p_err >>= \case
      p | nullPtr == p -> throwIO $ PyError "Compile error"
        | otherwise    -> do
            s <- peekCString p
            throwIO $ PyError s
    _ -> error $ "pyEvalStr: unexpected error: " ++ show r
  where
    py_err_compile = PY_ERR_COMPILE
    py_err_eval    = PY_ERR_EVAL

pyEvalStrE :: String -> IO PyObject
pyEvalStrE py = mask_ $ evalContT $ do
  p_py  <- ContT $ withCString py
  p_err <- ContT $ alloca @(Ptr CChar)
  p_res <- ContT $ alloca @(Ptr PyObject)
  r <- liftIO
    [C.block| int {
       PyObject *e_type, *e_value, *e_trace;
       // Compile code
       PyObject *code = Py_CompileString($(char* p_py), "<interactive>", Py_eval_input);
       if( code == 0 ){
           PyErr_Fetch( &e_type, &e_value, &e_trace);
           inline_py_export_exception(e_type, e_value, e_trace, $(char** p_err));
           return $(int py_err_compile);
       }
       // Execute in context of main
       PyObject* main_module = PyImport_AddModule("__main__");
       PyObject* globals     = PyModule_GetDict(main_module);
       //
       PyObject* r = PyEval_EvalCode(code, globals, globals);
       Py_INCREF(r);
       *$(PyObject** p_res) = r;
       if( PyErr_Occurred() ) {
           PyErr_Fetch( &e_type, &e_value, &e_trace);
           inline_py_export_exception(e_type, e_value, e_trace, $(char** p_err));
           return $(int py_err_eval);
       }
       return 0;
       }|]
  liftIO $ case r of
    PY_OK          -> newPyObject =<< peek p_res
    PY_ERR_COMPILE -> peek p_err >>= \case
      p | nullPtr == p -> throwIO $ PyError "Compile error"
        | otherwise    -> do
            s <- peekCString p
            throwIO $ PyError s
    PY_ERR_EVAL -> peek p_err >>= \case
      p | nullPtr == p -> throwIO $ PyError "Compile error"
        | otherwise    -> do
            s <- peekCString p
            throwIO $ PyError s
    _ -> error $ "pyEvalStr: unexpected error: " ++ show r
  where
    py_err_compile = PY_ERR_COMPILE
    py_err_eval    = PY_ERR_EVAL


unindent :: String -> String
unindent py = case ls of
  [] -> ""
  _  -> unlines $ drop n <$> ls
  where
    n  = minimum [ length (takeWhile (==' ') s) | s <- ls ]
    ls = filter (any (not . isSpace)) $ lines py

pattern PY_OK, PY_ERR_COMPILE, PY_ERR_EVAL :: CInt
pattern PY_OK          = 0
pattern PY_ERR_COMPILE = 1
pattern PY_ERR_EVAL    = 2


expQQ :: String -> String -> TH.Q TH.Exp
expQQ mode src = do
  script <- liftIO $ getDataFileName "py/bound-vars.py"
  antis  <- liftIO $ do
    (code, stdout, stderr) <- readProcessWithExitCode "python" [script, mode] src
    case code of
      ExitSuccess   -> pure $ words stdout
      ExitFailure{} -> error stderr
  --  
  error $ show antis
  TH.lift src

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

withWCtring :: String -> (Ptr CWchar -> IO a) -> IO a
withWCtring = withArray0 (CWchar 0) . map (fromIntegral . ord)
