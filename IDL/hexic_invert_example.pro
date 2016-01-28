; docformat = 'rst'

pro hexic_invert_example
  compile_opt strictarr

  ; TODO: read FITS files, etc. instead of creating random data
  n_filters = 6
  n_polarization_states = 4
  data_dir = '/Users/rce/work/HEXIC/GIT/hexic/OBS/hmi/'
  SPAWN, 'ls -1 ' + data_dir + '*.fits', fits_files

  ; Area to invert in the full disk HMI image
  x1 = 2100
  x2 = 2101
  y1 = 1960
  y2 = 1961
  nx = x2 - x1 + 1
  ny = y2 - y1 + 1


observations = dblarr(n_filters, n_polarization_states, nx, ny)

                                ; Reading HMI files with Stokes
                                ; profiles data and storing the region
                                ; to invert in OBSERVATIONS                            


  FOR j = 0, N_ELEMENTS(fits_files)-1 DO BEGIN
     res = FITSIO_READ_IMAGE(fits_files[j])
     observations[ j MOD n_filters, j/n_filters, *, *] = res[x1:x2,y1:y2]  
  ENDFOR
  ; call hexic_invert


 SAVE, observations, filename = 'observations.save'
; RESTORE, 'observations.save'



  status = hexic_invert(observations)
  help, status
end
