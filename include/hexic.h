#include "FC.h"

int hexic_invert(int mode,
                 double *observations, int width, int height, int n_filters,
                 double **results, double **synthetic,
                 double *model,
                 double *weights, double *noise,
                 double *scattered_light,
                 int *free);

// Fortran routine
int run_hexic(int *mode,
              double *observations, int *width, int *height, int *n_filters,
              double *results, double *synthetic,
              double *model,
              double *weights, double *noise,
              double *scattered_light,
              int *free);
