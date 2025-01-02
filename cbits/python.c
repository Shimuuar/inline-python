#include <inline-python.h>
#include <stdlib.h>

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

int inline_py_unpack_iterable(PyObject *iterable, int n, PyObject **out) {
    // Initialize iterator. If object is not an iterable we treat this
    // as not an exception but as a conversion failure
    PyObject* iter = PyObject_GetIter( iterable );
    if( PyErr_Occurred() ) {
        PyErr_Clear();
        return -1;
    }
    if( !PyIter_Check(iter) ) {
        goto err_iter;
    }
    // Fill out with NULL. This way we can call XDECREF on them
    for(int i = 0; i < n; i++) {
        out[i] = NULL;
    }
    // Fill elements
    for(int i = 0; i < n; i++) {
        out[i] = PyIter_Next(iter);
        if( NULL==out[i] ) {
            goto err_elem;
        }
    }
    // End of iteration
    PyObject* end = PyIter_Next(iter);
    if( NULL != end || PyErr_Occurred() ) {
        goto err_end;
    }
    return 0;
    //----------------------------------------
err_end:
    Py_XDECREF(end);
err_elem:
    for(int i = 0; i < n; i++) {
        Py_XDECREF(out[i]);
    }
err_iter:
    Py_DECREF(iter);
    return -1;
}

void inline_py_free_capsule(PyObject* py) {
    PyMethodDef *meth = PyCapsule_GetPointer(py, NULL);
    // HACK: We want to release wrappers created by wrapper. It
    //       doesn't seems to be nice and stable C API
    freeHaskellFunctionPtr(meth->ml_meth);
    free(meth);
}

