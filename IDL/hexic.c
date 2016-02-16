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
  IDL_VPTR vptr_observations = argv[0];
  IDL_VPTR vptr_results, vptr_synthetic;
  double *observations, *results, *synthetic;
  int width, height, n_filters, status;
  IDL_MEMINT results_dims[3];

  IDL_ENSURE_ARRAY(vptr_observations);
  IDL_ENSURE_SIMPLE(vptr_observations);

  if (vptr_observations->value.arr->n_dim != 4) {
    IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_LONGJMP, "image cube must be 4-dimensional");
  }
  if (vptr_observations->type != IDL_TYP_DOUBLE) {
    IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_LONGJMP, "image cube must be of type double");
  }

  observations = (double *) vptr_observations->value.arr->data;
  n_filters = vptr_observations->value.arr->dim[0];
  width = vptr_observations->value.arr->dim[2];
  height = vptr_observations->value.arr->dim[3];

  status = hexic_invert(observations, width, height, n_filters, results, synthetic);
  results_dims[0] = 11;
  results_dims[1] = width;
  results_dims[2] = height;
  vptr_results = IDL_ImportArray(3, results_dims, IDL_TYP_DOUBLE, (UCHAR *) results, NULL, NULL);
  return(vptr_results);
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