#include <Python.h>

#define NPY_NO_DEPRECATED_API NPY_1_7_API_VERSION
#include <numpy/arrayobject.h>

#include "hexic.h"



static PyObject *py_hexic_invert(PyObject *self, PyObject *args) {
  PyArrayObject *observations_array = NULL;
  double *observations;
  npy_intp *shape;
  PyArray_Descr *dtype;
  int status;

  if (!PyArg_ParseTuple(args, "O!", &PyArray_Type, &observations_array)) return NULL;

  // TODO: check observations array is:
  //   - type double

  dtype = PyArray_DTYPE(observations_array);

  if (PyArray_NDIM(observations_array) != 4) {
    printf("wrong number of dimensions\n");
  }
  shape = PyArray_SHAPE(observations_array);
  if (shape[0] != 6) {
    printf("wrong number of filters\n");
  }
  if (shape[1] != 4) {
    printf("wrong number of polarization states\n");
  }
  observations = (double *) PyArray_DATA(observations_array);

  status = hexic_invert(observations, shape[3], shape[4], shape[0]);

  return Py_BuildValue("i", status);
}

static PyMethodDef hexic_methods[] = {
  {"invert", py_hexic_invert, METH_VARARGS,
   "A general purpose Milne-Eddington spectral line inversion code"},
  {NULL, NULL, 0, NULL}
};

PyMODINIT_FUNC inithexic(void) {
  PyObject *m = Py_InitModule("hexic", hexic_methods);
  import_array();
}
