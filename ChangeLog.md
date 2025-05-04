0.2 [2025.05.04]
----------------
* `FromPy`/`ToPy` instances added for: `Complex`, both strict and lazy `Text` &
  `ByteString`, `ShortByteString`, `Maybe a`
* Module `Python.Inline.Eval` added which support for eval/exec with user
  supplied global and local variables.
* QuasiQuotes `Python.Inline.QQ.pycode` added for creating `PyQuote` data type.

0.1.1.1 [2025.03.10]
--------------------
* Crash of python's main thread when one attempts to interrupt it fixed.

0.1.1 [2025.02.13]
------------------
* Number of deadlocks in `runPyInMain` fixed:
  - It no longer deadlocks is exception is thrown
  - Nested calls no longer deadlock.
  - Calling it from python callback.
* `ToPy` instance added for `Py b`, `a -> Py b`, `a1 -> a2 -> Py b`


0.1 [2025.01.18]
----------------
Initial release
