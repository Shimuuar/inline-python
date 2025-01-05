#pragma once

#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <Rts.h>


// Available since 3.13
#ifndef PyCFunctionFast
typedef _PyCFunctionFast PyCFunctionFast;
#endif

// Available since 3.13
//
// We define here compat dummy which always says No
#ifndef Py_IsFinalizing
#define Py_IsFinalizing(x) 0
#endif



// ================================================================
// Callbacks
// ================================================================

// Wrap haskell callback using METH_NOARGS calling convention
PyObject *inline_py_callback_METH_NOARGS(PyCFunction fun);

// Wrap haskell callback using METH_O calling convention
PyObject *inline_py_callback_METH_O(PyCFunction fun);

// Wrap haskell callback using METH_FASTCALL calling convention
PyObject *inline_py_callback_METH_FASTCALL(PyCFunctionFast fun);



// ================================================================
// Marhsalling
// ================================================================

// Unpack iterable into array of PyObjects. Iterable must contain
// exactly N elements.
//
// On success returns 0 and fills `out` with N PyObjects
//
// On failure return -1. Content of out is then undefined and it
// doesn't contain live python objects. If failure is due to python
// exception it's not cleared.
int inline_py_unpack_iterable(
    PyObject  *iterable,
    int        n,
    PyObject **out
    );
