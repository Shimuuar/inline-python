#pragma once

#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <Rts.h>


// ----------------------------------------------------------------
// Standard status codes

#define IPY_OK          0
#define IPY_ERR_PYTHON  1
#define IPY_ERR_COMPILE 2

// ----------------------------------------------------------------

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

// Allocate python function object which carrries its own PyMethodDef.
// Returns function object or NULL with error raised.
//
// See NOTE: [Creation of python functions]
PyObject *inline_py_function_wrapper(PyCFunction fun, int flags);

// Free malloc'd buffer inside PyCapsule
void inline_py_free_capsule(PyObject*);

