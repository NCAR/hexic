; docformat = 'rst'

pro hexic_invert_example, use_save=use_save
  compile_opt strictarr

  line_filename='/Users/rce/work/HEXIC/GIT/hexic/USER_FILES/LINE_hmi.txt'
  line_filename='LINE_hmi.txt'

  path_to_filt = '/Users/rce/work/HEXIC/GIT/hexic/USER_FILES/filters_hmi.txt'

  if (keyword_set(use_save)) then begin
    restore, 'observations.sav';, /verbose
    dims = SIZE(observations)
    n_filters = dims[1]
    n_polarization_states = dims[2]

  endif else begin
    ; read FITS files, etc. instead of creating random data
    n_filters = 6
    n_polarization_states = 4
    data_dir = '/Users/rce/work/HEXIC/GIT/hexic/OBS/hmi/'
    SPAWN, 'ls -1 ' + data_dir + '*.fits', fits_files

    ; area to invert in the full disk HMI image
    x1 = 2100
    x2 = 2102
    y1 = 1960
    y2 = 1961
    nx = x2 - x1 + 1
    ny = y2 - y1 + 1

    observations = dblarr(n_filters, n_polarization_states, nx, ny)
;    synthetic = dblarr(n_filters, n_polarization_states, nx, ny)
    status = 0
    ; Reading HMI files with Stokes profiles data and storing the region to
    ; invert in OBSERVATIONS
    PRINT, '--- Reading observations from FITS files'
    for j = 0L, n_elements(fits_files) - 1L do begin
      res = fitsio_read_image(fits_files[j])
      observations[j mod n_filters, j / n_filters, *, *] = res[x1:x2, y1:y2]
   endfor

    SAVE, observations, filename='observations.sav'
    RESTORE, 'observations.sav'
 endelse

  PRINT, '--- Reading instruments filter profiles'
  read_filt, path_to_filt, n_filters, filters


  PRINT, '--- Running inversion over selected field of view.'

  model = [20.0D, 0.0D, 0.0D, 0.5D, 10.0D, 500.0D, 0.0D, 0.1D, 0.9D, 1.0D]
  free  = [   1L,   1L,   1L,   0L,    1L,     1L,   1L,   1L,   1L,   0L]
  weights = [1D, 3.5D, 3.5D, 2.5D]
  noise = [1.0D2, 1.0D2, 1.0D2, 1.0D2]

;  results = hexic_invert(observations, status=status, synthetic=synthetic, $
;                         model=model, weights=weights, noise = noise, free=free)
 results = hexic_invert(observations, status=status, synthetic=synthetic, $
                         model=model, free=free, line_filename = line_filename, filters=filters)
;results = hexic_invert(/synthesis_mode, observations, status=status, synthetic=synthetic, $
;                         model=model, free=free, line_filename = line_filename)
  help, results, status, synthetic


j = 1
k = 1

!P.multi = [0,2,2]
 plot, observations[*,0,j,k], psym = 8
 oplot, synthetic[*,0,j,k]
 plot, observations[*,1,j,k], psym = 8
 oplot, synthetic[*,1,j,k]
 plot, observations[*,2,j,k], psym = 8
 oplot, synthetic[*,2,j,k]
 plot, observations[*,3,j,k], psym = 8
 oplot, synthetic[*,3,j,k]

!P.MULTI = 0

 ; print, (observations - synthetic) / observations * 100.0

end
