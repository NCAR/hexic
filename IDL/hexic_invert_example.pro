; docformat = 'rst'

pro hexic_invert_example, use_save=use_save
  compile_opt strictarr

  if (keyword_set(use_save)) then begin
    restore, 'observations.sav', /verbose
  endif else begin
    ; read FITS files, etc. instead of creating random data
    n_filters = 6
    n_polarization_states = 4
    data_dir = '/Users/rce/work/HEXIC/GIT/hexic/OBS/hmi/'
    SPAWN, 'ls -1 ' + data_dir + '*.fits', fits_files

    ; area to invert in the full disk HMI image
    x1 = 2100
    x2 = 2101
    y1 = 1960
    y2 = 1961
    nx = x2 - x1 + 1
    ny = y2 - y1 + 1

    observations = dblarr(n_filters, n_polarization_states, nx, ny)

    ; Reading HMI files with Stokes profiles data and storing the region to
    ; invert in OBSERVATIONS
    for j = 0L, n_elements(fits_files) - 1L do begin
      res = fitsio_read_image(fits_files[j])
      observations[j mod n_filters, j / n_filters, *, *] = res[x1:x2, y1:y2]
    endfor

    save, observations, filename='observations.sav'
  endelse

  results = hexic_invert(observations, status=status)
  help, results, status
end
