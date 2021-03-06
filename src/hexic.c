#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "hexic.h"
#include "FC.h"

int hexic_invert(int mode,
                 double *observations, int width, int height, int n_filters,
                 double **results, double **synthetic,
                 double *model,
                 double *weights, double *noise,
                 double *scattered_light,
                 char *line_filename, int len_line_filename,
                 int nwavelengths, double *filters,
                 int *free) {
  int status;

  // allocate output variables
  *results = (double *) calloc(11 * width * height, sizeof(double));
  *synthetic = (double *) calloc(n_filters * 4 * width * height, sizeof(double));

  status = run_hexic(&mode, observations, &width, &height, &n_filters,
                     *results, *synthetic,
                     model, weights, noise, scattered_light,
                     line_filename, &len_line_filename,
                     &nwavelengths, filters, free);

  return status;
}
