#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#include "hexic.h"

int hexic_invert(float *image_cube, int width, int height, int n_spectra) {
  for (int h = 0; h < height; h++) {
    for (int w = 0; w < width; w++) {
      for (int s = 0; s < n_spectra; s++) {
        printf("%0.1f\n", image_cube[h * width * height + w * height + s]);
      }
    }
  }
  return 1;
}
