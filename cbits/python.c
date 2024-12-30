#include <inline-python.h>
#include <stdlib.h>

void inline_py_export_exception(
    PyObject *e_type,
    PyObject *e_value,
    PyObject *e_trace,
    char** p_msg
    )
{
    // Convert to python string object
    PyObject *e_str = PyObject_Str(e_value);
    if( 0 == e_str ) {
        *p_msg = 0;
        return;
    }
    // Convert to UTF8 C string
    const char *err_msg = PyUnicode_AsUTF8(e_str);
    if( 0 == e_str ) {
        *p_msg = 0;
        return;
    }
    // Copy message
    int n  = strlen(err_msg);
    *p_msg = malloc(n+1);
    strcpy(*p_msg, err_msg);
    return;
}

PyObject *inline_py_function_wrapper(PyCFunction fun, int flags) {
    PyMethodDef *meth = malloc(sizeof(PyMethodDef));
    meth->ml_name  = "[inline_python]";
    meth->ml_meth  = fun;
    meth->ml_flags = flags;
    meth->ml_doc   = "Wrapper constructed by inline-python";
    // Python wrapper which carries PyMethodDef
    PyObject* meth_obj = PyCapsule_New(meth, NULL, &inline_py_free_capsule);
    if( PyErr_Occurred() )
        return NULL;
    // Python function
    PyObject* f = PyCFunction_New(meth, meth_obj);
    Py_DECREF(meth_obj);
    return f;    
}

// HACK: Simply copied from GHC sources. I hope it's stable enough
void freeHaskellFunctionPtr (void* ptr);

void inline_py_free_capsule(PyObject* py) {
    PyMethodDef *meth = PyCapsule_GetPointer(py, NULL);
    // HACK: We want to release wrappers created by wrapper. It
    //       doesn't seems to be nice and stable C API
    freeHaskellFunctionPtr(meth->ml_meth);
    free(meth);
}

