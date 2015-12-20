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


static IDL_VPTR IDL_hexic_invert(int argc, IDL_VPTR *argv, char *argk) {
  return IDL_GettmpLong(0);
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