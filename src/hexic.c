#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "hexic.h"
#include "FC.h"

int hexic_invert(double *observations, int width, int height, int n_filters) {
  run_hexic(observations, &width, &height, &n_filters);
  return 1;
}
