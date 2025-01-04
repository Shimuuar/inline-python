#include <inline-python.h>
#include <stdlib.h>

// ================================================================
// Callbacks
//
// General idea: we store function pointer (haskell's FunPtr) in
// PyCapsule and use to call function. Most importantly we must
// release GIL before calling into haskell. Haskell callback will
// happen on different thread (on threaded RTS). So it'll have to
// reacquire GIL there.
// ================================================================

// Same wrapper works for METH_O and METH_NOARGS
static PyObject* callback_METH_CFunction(PyObject* self, PyObject* arg) {
    PyObject    *res;
    PyCFunction *fun = PyCapsule_GetPointer(self, NULL);
Py_BEGIN_ALLOW_THREADS
    res = (*fun)(self, arg);
Py_END_ALLOW_THREADS
    return res;
}

static PyObject* callback_METH_FASTCALL(PyObject* self, PyObject** args, Py_ssize_t nargs) {
    PyObject        *res;
    PyCFunctionFast *fun = PyCapsule_GetPointer(self, NULL);
Py_BEGIN_ALLOW_THREADS
    res = (*fun)(self, args, nargs);
Py_END_ALLOW_THREADS
    return res;
}

static void capsule_free_FunPtr(PyObject* capsule) {
    PyCFunction *fun = PyCapsule_GetPointer(capsule, NULL);
    // We call directly to haskell RTS to free FunPtr. Only question
    // is how stable is this API.
    freeHaskellFunctionPtr(*fun);
    free(fun);
}

static PyMethodDef method_METH_NOARGS = {
    .ml_name  = "[inline_python]",
    .ml_meth  = callback_METH_CFunction,
    .ml_flags = METH_NOARGS,
    .ml_doc   = "Wrapper for haskell callback"
};

static PyMethodDef method_METH_O = {
    .ml_name  = "[inline_python]",
    .ml_meth  = callback_METH_CFunction,
    .ml_flags = METH_O,
    .ml_doc   = "Wrapper for haskell callback"
};

static PyMethodDef method_METH_FASTCALL = {
    .ml_name  = "[inline_python]",
    .ml_meth  = (PyCFunction)callback_METH_FASTCALL,
    .ml_flags = METH_FASTCALL,
    .ml_doc   = "Wrapper for haskell callback"
};

PyObject *inline_py_callback_METH_NOARGS(PyCFunction fun) {
    PyCFunction *buf = malloc(sizeof(PyCFunction));
    *buf = fun;
    PyObject* self = PyCapsule_New(buf, NULL, &capsule_free_FunPtr);
    if( PyErr_Occurred() )
        return NULL;
    // Python function
    PyObject* f = PyCFunction_New(&method_METH_NOARGS, self);
    Py_DECREF(self);
    return f;
}

PyObject *inline_py_callback_METH_O(PyCFunction fun) {
    PyCFunction *buf = malloc(sizeof(PyCFunction));
    *buf = fun;
    PyObject* self = PyCapsule_New(buf, NULL, &capsule_free_FunPtr);
    if( PyErr_Occurred() )
        return NULL;
    // Python function
    PyObject* f = PyCFunction_New(&method_METH_O, self);
    Py_DECREF(self);
    return f;
}

PyObject *inline_py_callback_METH_FASTCALL(PyCFunctionFast fun) {
    PyCFunctionFast *buf = malloc(sizeof(PyCFunctionFast));
    *buf = fun;
    PyObject* self = PyCapsule_New(buf, NULL, &capsule_free_FunPtr);
    if( PyErr_Occurred() )
        return NULL;
    // Python function
    PyObject* f = PyCFunction_New(&method_METH_FASTCALL, self);
    Py_DECREF(self);
    return f;
}


// ================================================================
// Marshalling
// ================================================================

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

