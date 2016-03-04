#include "FC.h"

int hexic_invert(double *observations, int width, int height, int n_filters,
                 double **results, double **synthetic,
                 double *model, int *free);

// Fortran routine
int run_hexic(double *observations, int *width, int *height, int *n_filters,
              double *results, double *synthetic, double *model, int *free);
