#include <inline-python.h>
#include <stdlib.h>

#include "MachDeps.h"


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


PyObject* inline_py_Integer_ToPy(
    void*  buf,
    size_t size,
    int    sign
    )
{
    PyObject* num =
#if PY_MINOR_VERSION < 13
        _PyLong_FromByteArray(buf, size,
                              1, // Little endian
                              0  // Unsigned
        );
#else
        PyLong_FromNativeBytes(buf, size,
                               Py_ASNATIVEBYTES_LITTLE_ENDIAN |
                               Py_ASNATIVEBYTES_UNSIGNED_BUFFER
        );
#endif
    if( sign ) {
        PyObject* neg = PyNumber_Negative(num);
        Py_DECREF(num);
        return neg;
    } else {
        return num;
    }
}


ssize_t inline_py_Long_ByteSize(PyObject* p) {
    // See NOTE: [Integer encoding/decoding]
    //
    // PyLong_AsNativeBytes allows to compute buffer size but it does
    // so according to python's memory layout
#if WORD_SIZE_IN_BITS == 32
    const int shiftW = 2;
#elif WORD_SIZE_IN_BITS == 64
    const int shiftW = 3;
#else
#error "Something wrong with MachDeps.h"
#endif
    const int     shift = shiftW + 3;
    const ssize_t mask  = (1<<shift) - 1;
    const ssize_t bits  = _PyLong_NumBits(p);
    if( bits & mask ) {
        return ((bits >> shift) + 1) << shiftW;
    } else {
        return (bits >> shift) << shiftW;
    }
}

void inline_py_Integer_FromPy(
    PyObject* p,
    void*     buf,
    size_t    size
    )
{
    // N.B. _PyLong_AsByteArray changed signature in 3.13
#if PY_MINOR_VERSION < 13
    _PyLong_AsByteArray((PyLongObject*)p, buf, size,
                        1, // little_endian
                        0  // is_signed
        );
#else
    PyLong_AsNativeBytes(p, buf, size, -1);
#endif
}


static Py_tss_t key_py_async = Py_tss_NEEDS_INIT;

void inline_py_init_state(void *stack) {
    int r = PyThread_tss_set(&key_py_async, stack);
    if( 0 != r ) {
        fprintf(stderr, "inline-python: fatal error: setting thread local storage failed\n");
        exit(1);
    }
}

void inline_py_free_state(void) {
    int r = PyThread_tss_set(&key_py_async, NULL);
    if( 0 != r ) {
        fprintf(stderr, "inline-python: fatal error: setting thread local storage failed\n");
        exit(1);
    }
}

void* inline_py_get_state(void) {
    return PyThread_tss_get(&key_py_async);
}


void inline_py_initialize(void) {
    int r = PyThread_tss_create(&key_py_async);
    if( 0 != r ) {
        fprintf(stderr, "inline-python: fatal error: Failed to initialized thread local storage\n");
        exit(1);
    }
}

// ================================================================
// inline_python module

PyObject* (*inline_py_haskell_error_repr)(void*);
PyObject* (*inline_py_haskell_error_tyrepr)(void*);

PyObject* inline_py_AsyncCancelled() {
    static PyObject* AsyncCancelled = 0;
    if( AsyncCancelled == 0 ) {
        AsyncCancelled = PyErr_NewException("inline_python.AsyncCancelled", PyExc_BaseException, 0);
    }
    return AsyncCancelled;
}

typedef struct {
    PyBaseExceptionObject obj;
    void *exception_stableptr;
} HaskellError;

static void haskell_error_dealloc(PyObject *op) {
    HaskellError *self = (HaskellError*) op;
    hs_free_stable_ptr(self->exception_stableptr);
    Py_TYPE(self)->tp_free(self);
}

static PyObject* haskell_error_repr(PyObject *op) {
    HaskellError *self = (HaskellError*) op;
    PyObject* exc_repr = inline_py_haskell_error_repr(self->exception_stableptr);
    PyObject* exc_ty   = inline_py_haskell_error_tyrepr(self->exception_stableptr);
    PyObject* repr     = PyUnicode_FromFormat("<inline_python.HaskellError: %S: %S>", exc_ty, exc_repr);
    Py_DECREF(exc_repr);
    Py_DECREF(exc_ty);
    return repr;
}

static PyTypeObject HaskellError_Type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    .tp_name      = "inline_python.HaskellError",
    .tp_basicsize = sizeof(HaskellError),
    .tp_flags     = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_DISALLOW_INSTANTIATION,
    .tp_doc       = PyDoc_STR("Wrapper for a haskell exception object"),
    .tp_dealloc   = haskell_error_dealloc,
    .tp_repr      = haskell_error_repr
};


PyObject* inline_py_HaskellError() {
    static int initialized = 0;
    if( 0 == initialized ) {
        if( PyType_Ready(&HaskellError_Type) != 0) {
            // It should success always and we don't have any reasonable
            // way of handing error.
            exit(1);
        }
        initialized = 1;
    }
    return (PyObject*)&HaskellError_Type;
}

PyObject* inline_py_HaskellError_create(void* exc_ptr) {
    PyTypeObject *base = HaskellError_Type.tp_base;
    PyObject *args     = PyTuple_New(0);
    // Call __new__
    PyObject *obj = base->tp_new(&HaskellError_Type, args, NULL);
    if( !obj ) {
        goto err;
    }
    // Call __init__
    if( 0 != base->tp_init(obj, args, NULL) ) {
        goto err;
    }
    // Set custom fields
    HaskellError* h_err = (HaskellError*)obj;
    h_err->exception_stableptr = exc_ptr;
    Py_DECREF(args);
    return obj;
err:
    Py_DECREF(args);
    return NULL;
}

void* inline_py_HaskellError_get_stableptr(PyObject* err) {
    HaskellError *h_err = (HaskellError*) err;
    return h_err->exception_stableptr;
}



static PyMethodDef inline_python_methods[] = {
    {NULL, NULL, 0, NULL}
};

static int inline_python_module_exec(PyObject *m) {
    // Initialize module only once
    static int initialized = 0;
    if( initialized ) {
        return 0;
    }
    initialized = 1;
    //
    if (PyModule_AddObjectRef(m, "AsyncCancelled", inline_py_AsyncCancelled()) < 0) {
        return -1;
    }
    //
    HaskellError_Type.tp_base = (PyTypeObject*)PyExc_Exception;
    HaskellError_Type.tp_new  = NULL;
    if (PyModule_AddObjectRef(m, "HaskellError", inline_py_HaskellError()) < 0) {
        return -1;
    }
    return 0;
}

static PyModuleDef_Slot inline_python_module_slots[] = {
    {Py_mod_exec, inline_python_module_exec},
    {0, NULL}
};

static struct PyModuleDef inline_python_module = {
    .m_base    = PyModuleDef_HEAD_INIT,
    .m_name    = "inline_python",
    .m_size    = 0,
    .m_slots   = inline_python_module_slots,
    .m_methods = inline_python_methods,
};

PyMODINIT_FUNC PyInit_inline_python(void)
{
    return PyModuleDef_Init(&inline_python_module);
}
