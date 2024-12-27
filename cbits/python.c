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
