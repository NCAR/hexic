#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "hexic.h"
#include "FC.h"

int hexic_invert(double *observations, int width, int height, int n_filters,
                 double *results, double *synthetic) {
  // allocate output variables
  results = (double *) calloc(11 * width * height, sizeof(double));
  synthetic = (double *) calloc(n_filters * 4 * width * height, sizeof(double));

  run_hexic(observations, &width, &height, &n_filters, results, synthetic);

  return 1;
}
