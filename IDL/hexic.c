#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef strlcpy
#undef strlcpy
#endif

#ifdef strlcat
#undef strlcat
#endif

#include "idl_export.h"

#include "hexic.h"



static IDL_VPTR IDL_hexic_invert(int argc, IDL_VPTR *argv, char *argk) {
  IDL_VPTR vptr_image_cube = argv[0];
  float *image_cube;
  int width, height, n_spectra, status;

  IDL_ENSURE_ARRAY(vptr_image_cube);
  IDL_ENSURE_SIMPLE(vptr_image_cube);

  if (vptr_image_cube->value.arr->n_dim != 4) {
    IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_LONGJMP, "image cube must be 4-dimensional");
  }
  if (vptr_image_cube->type != IDL_TYP_DOUBLE) {
    IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_LONGJMP, "image cube must be of type double");
  }

  image_cube = (float *) vptr_image_cube->value.arr->data;
  n_spectra = vptr_image_cube->value.arr->dim[0];
  width = vptr_image_cube->value.arr->dim[1];
  height = vptr_image_cube->value.arr->dim[2];

  status = hexic_invert(image_cube, width, height, n_spectra);

  return IDL_GettmpLong(status);
}


int IDL_Load(void) {
  /*
   * These tables contain information on the functions and procedures
   * that make up the analysis DLM. The information contained in these
   * tables must be identical to that contained in mg_analysis.dlm.
   */
  static IDL_SYSFUN_DEF2 function_addr[] = {
    { IDL_hexic_invert, "HEXIC_INVERT", 1, 1, IDL_SYSFUN_DEF_F_KEYWORDS, 0 },
  };

  /*
   * Register our routines. The routines must be specified exactly the same
   * as in mg_introspection.dlm.
   */
  return IDL_SysRtnAdd(function_addr, TRUE, IDL_CARRAY_ELTS(function_addr));
}