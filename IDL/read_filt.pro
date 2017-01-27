PRO read_filt, path_to_filt, n_filters, filters

filters = DBLARR(n_filters)
filters1 = DBLARR(n_filters)

OPENR, unit, path_to_filt, /GET_LUN

READF, unit, filters
Nwavelengths = 1

WHILE NOT EOF(unit) DO BEGIN

   READF, unit, filters1
   filters = [[filters], [filters1]]
   Nwavelengths = Nwavelengths + 1   
ENDWHILE


END
