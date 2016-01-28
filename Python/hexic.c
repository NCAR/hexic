#include <Python.h>

#define NPY_NO_DEPRECATED_API NPY_1_7_API_VERSION
#include <numpy/arrayobject.h>

#include "hexic.h"



static PyObject *py_hexic_invert(PyObject *self, PyObject *args) {
  PyObject *observations;
  if (!PyArg_ParseTuple(args, "O", observations)) return NULL;
  return Py_BuildValue("i", 0);
}

static PyMethodDef hexic_methods[] = {
  {"invert", py_hexic_invert, METH_VARARGS, "A general purpose Milne-Eddington spectral line inversion code"},
  {NULL, NULL, 0, NULL}
};

PyMODINIT_FUNC inithexic(void) {
  PyObject *m = Py_InitModule("hexic", hexic_methods);
  import_array();
}
