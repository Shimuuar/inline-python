#pragma once

#define PY_SSIZE_T_CLEAN
#include <Python.h>

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
