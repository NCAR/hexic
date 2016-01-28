#include <Python.h>

static int gcd(int x, int y) {
  int g = y;
  while (x > 0) {
    g = x;
    x = y % x;
    y = g;
  }
  return g;
}

static PyObject *spam_gcd(PyObject *self, PyObject *args) {
  int x, y, g;
  if (!PyArg_ParseTuple(args, "ii", &x, &y)) return NULL;
  g = gcd(x, y);
  return Py_BuildValue("i", g);
}

static PyMethodDef spammethods[] = {
  {"gcd", spam_gcd, METH_VARARGS, "greatest common divisor"},
  {NULL, NULL, 0, NULL}
};

PyMODINIT_FUNC initspam(void) {
  PyObject *m = Py_InitModule("spam", spammethods);
}
