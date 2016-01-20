; docformat = 'rst'

pro hexic_invert_example
  compile_opt strictarr

  ; TODO: read FITS files, etc. instead of creating random data
  n_filters = 2
  nx = 2
  ny = 2
  n_polarization_states = 4
  ;observations = randomu(seed, ny, nx, n_polarization_states, n_filters, /double)
  observations = dindgen(ny, nx, n_polarization_states, n_filters)

  ; call hexic_invert
  status = hexic_invert(observations)
  help, status
end
