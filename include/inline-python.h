#pragma once

#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <Rts.h>


// Standard status codesu
#define INLINE_PY_OK          0
#define INLINE_PY_ERR_COMPILE 1
#define INLINE_PY_ERR_EVAL    2



// This macro checks for errors. If python exception is raised it
// clear it and returns 1 otherwise retruns 0
#define INLINE_PY_SIMPLE_ERROR_HANDLING() do {      \
    if( PyErr_Occurred() ) {                        \
          PyObject *e_type, *e_value, *e_trace;     \
          PyErr_Fetch(&e_type, &e_value, &e_trace); \
          return 1;                                 \
    }                                               \
    return 0;                                       \
} while(0)

// Convert python exception into form suitable for haskell
void inline_py_export_exception(
    PyObject *e_type,
    PyObject *e_value,
    PyObject *e_trace,
    char** p_msg
    );

// Unpack iterable into array of PyObjects. Iterable must contain
// exactly N elements.
//
// On success returns 0 and fills `out` with N PyObjects
//
// On failure returns -1. Python exception is not cleared. It's
// responsibility of caller to deal with it. Content of `out` is
// undefined in this case.
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

