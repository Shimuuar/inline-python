0.3.0.0 [XXXX.XX.XX]
--------------------
* Support for asynchronous execution added in module `Python.Inline.Async`.  It
  uses API modelled after `async` package. Such computations could be
  interrupted using `cancelPy` even when they're running python code or haskell
  callback/
* When threaded runtime is used `runPyInMain` could be reliably interrupted by
  asynchronous exceptions.
* Python module `inline_python` is now available. It contains exception types
  used by library: `AsyncCancelled` and `HaskellError` which wraps haskell
  exception from callback.
* `Python.Inline.Monad` and `Python.Inline.Monad.QQ` modules providing monadic
  API. It uses `MonadPy` type class which allows user code to carry around
  python dictionaries with global and local scopes.
* Package now uses `Custom` build type. It now supports configuring python using
  `python3-config` instead of `pkg-config` when `-fpython3-config` manual cabal
  flag is set. Default behavior is unchanged.
* Haskell exception raised in haskell callback will be rethrown if not caught by
  python instead of being converted to `PyError`.
* Memory leak in exception handling is fixed. Python exception object were never
  freed when exception propagated to haskell side.

0.2.1.0 [2026.01.13]
----------------
* `From/ToPy` instance for `Integer`&`Natural` added.
* `vector-0.13.2` is required.
* Python>=3.10 is supported. Boolean marshaling with python<3.12 is
  fixed. Previously it caused crashes on counter decrement.
* Documentation fixes.

0.2 [2025.05.04]
----------------
* `FromPy`/`ToPy` instances added for: `Complex`, both strict and lazy `Text` &
  `ByteString`, `ShortByteString`, `Maybe a`.
* Module `Python.Inline.Eval` added which support for eval/exec with user
  supplied global and local variables.
* QuasiQuotes `Python.Inline.QQ.pycode` added for creating `PyQuote` data type.

0.1.1.1 [2025.03.10]
--------------------
* Crash of python's main thread when one attempts to interrupt it fixed.

0.1.1 [2025.02.13]
------------------
* Number of deadlocks in `runPyInMain` fixed:
  - It no longer deadlocks if exception is thrown
  - Nested calls no longer deadlock.
  - Calling it from python callback.
* `ToPy` instance added for `Py b`, `a -> Py b`, `a1 -> a2 -> Py b`


0.1 [2025.01.18]
----------------
Initial release
