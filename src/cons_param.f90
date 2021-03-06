MODULE CONS_PARAM
  !
  ! J M Borrero
  ! Dec 14, 2009
  ! HAO-NCAR for HMI-Stanford
  !

  ! set DEBUG to print debugging output
  logical, parameter :: DEBUG = .true.

  INTEGER,          PARAMETER  :: DP = KIND(1.D0)
  INTEGER,          PARAMETER  :: SP = KIND(1.)
  REAL(DP),   PARAMETER :: DPI = 3.141592653589793238462643
  REAL(DP),   PARAMETER :: D2R = 3.141592653589793238462643/180D0 ! deg -> rad
  REAL(DP),   PARAMETER :: LIGHT = 2.99792458E+10  ! Speed of light (cm/s)
END MODULE CONS_PARAM
