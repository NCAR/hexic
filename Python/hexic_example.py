import numpy

import hexic


def main():
    # TODO: read some example FITS file
    nx = 2
    ny = 2
    n_filters = 6
    n_pol_states = 4
    observations = numpy.arange(0., n_filters * n_pol_states * nx * ny, 1.0, dtype='d')
    observations = observations.reshape((n_filters, n_pol_states, nx, ny))

    # compute the inversion
    result = hexic.invert(observations)


if __name__ == '__main__':
    main()
