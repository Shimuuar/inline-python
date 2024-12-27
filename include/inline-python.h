#pragma once

#define PY_SSIZE_T_CLEAN
#include <Python.h>


// Convert python exception into form suitable for haskell
void inline_py_export_exception(
    PyObject *e_type,
    PyObject *e_value,
    PyObject *e_trace,
    char** p_msg
    );
