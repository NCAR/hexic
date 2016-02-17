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
  int n_args, width, height, n_filters, status;
  double *observations, *results, *synthetic;
  IDL_VPTR vptr_observations = argv[0];
  IDL_VPTR vptr_results, vptr_synthetic, vptr_status;
  IDL_MEMINT results_dims[IDL_MAX_ARRAY_DIM];
  IDL_MEMINT synthetic_dims[IDL_MAX_ARRAY_DIM];
  IDL_ARRAY *synthetic_array;

  typedef struct {
    IDL_KW_RESULT_FIRST_FIELD;
    IDL_VPTR status;
    int status_present;
    IDL_VPTR synthetic;
    int synthetic_present;
  } KW_RESULT;

  static IDL_KW_PAR kw_pars[] = {
    { "STATUS", IDL_TYP_LONG, 1, IDL_KW_OUT,
      IDL_KW_OFFSETOF(status_present), IDL_KW_OFFSETOF(status) },
    { "SYNTHETIC", IDL_TYP_DOUBLE, 1, IDL_KW_OUT,
      IDL_KW_OFFSETOF(synthetic_present), IDL_KW_OFFSETOF(synthetic) },
    { NULL },
  };
  KW_RESULT kw;

  IDL_ENSURE_ARRAY(vptr_observations);
  IDL_ENSURE_SIMPLE(vptr_observations);

  if (vptr_observations->value.arr->n_dim != 4) {
    IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_LONGJMP, "image cube must be 4-dimensional");
  }
  if (vptr_observations->type != IDL_TYP_DOUBLE) {
    IDL_Message(IDL_M_NAMED_GENERIC, IDL_MSG_LONGJMP, "image cube must be of type double");
  }

  n_args = IDL_KWProcessByOffset(argc, argv, argk, kw_pars, (IDL_VPTR *) NULL, 1, &kw);

  observations = (double *) vptr_observations->value.arr->data;
  n_filters = vptr_observations->value.arr->dim[0];
  width = vptr_observations->value.arr->dim[2];
  height = vptr_observations->value.arr->dim[3];

  status = hexic_invert(observations, width, height, n_filters, &results, &synthetic);

  results_dims[0] = 11;
  results_dims[1] = width;
  results_dims[2] = height;
  vptr_results = IDL_ImportArray(3, results_dims, IDL_TYP_DOUBLE, (UCHAR *) results, (IDL_ARRAY_FREE_CB) free, NULL);

  if (kw.synthetic_present) {
    kw.synthetic->type = IDL_TYP_DOUBLE;
    kw.synthetic->flags = IDL_V_ARR;

    synthetic_array = (IDL_ARRAY *) calloc(sizeof(IDL_ARRAY), 1);
    synthetic_array->elt_len = 8;
    synthetic_array->arr_len = 8 * n_filters * 4 * width * height;
    synthetic_array->n_elts = n_filters * 4 * width * height;

    synthetic_array->data = (UCHAR *) synthetic;

    synthetic_array->n_dim = 4;
    synthetic_array->dim[0] = n_filters;
    synthetic_array->dim[1] = 4;
    synthetic_array->dim[2] = width;
    synthetic_array->dim[3] = height;

    synthetic_array->free_cb = (IDL_ARRAY_FREE_CB) free;

    kw.synthetic->value.arr = synthetic_array;
  }

  if (kw.status_present) {
    kw.status->type = IDL_TYP_LONG;
    kw.status->value.l = status;
  }

  IDL_KW_FREE;

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