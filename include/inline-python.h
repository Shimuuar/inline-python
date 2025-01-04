#pragma once

#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <Rts.h>


// Use new stable API from 
#ifndef PyCFunctionFast
typedef _PyCFunctionFast PyCFunctionFast;
#endif

// ----------------------------------------------------------------
// Standard status codes

#define IPY_OK          0
#define IPY_ERR_PYTHON  1
#define IPY_ERR_COMPILE 2



// ================================================================
// Callbacks
// ================================================================

// Wrap haskell callback using METH_O calling convention
PyObject *inline_py_callback_METH_O(PyCFunction fun);

// Wrap haskell callback using METH_FASTCALL calling convention
PyObject *inline_py_callback_METH_FASTCALL(PyCFunctionFast fun);



// ================================================================
// Callbacks
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
