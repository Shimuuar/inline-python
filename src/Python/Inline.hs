-- |
module Python.Inline
  ( initializePython
  , finalizePython
  , withPython
  , PyObject
  , Literal(..)
  , toPy
  , fromPy
  ) where


import Python.Types
import Python.Inline.Literal

import Python.Internal.Init
