#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#include "hexic.h"
#include "FC.h"

int hexic_invert(float *image_cube, int width, int height, int n_spectra) {
  // SUBROUTINE run_hexic(OBSERVATIONS, Nx, Ny, Nfilt)
  // REAL(DP), DIMENSION(NFILT, 4, Nx, Ny)  :: OBSERVATIONS, SYNTHETIC
  run_hexic(image_cube, width, height, n_spectra);
  return 1;
}
