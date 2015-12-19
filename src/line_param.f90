MODULE LINE_PARAM
  !
  ! J M Borrero
  ! Dec 14, 2009
  ! HAO-NCAR for HMI-Stanford
  !
  USE CONS_PARAM
  !
  REAL(DP)                                   :: LANDA0, SHIFT, STEPW
  REAL(DP)                                   :: LAM_START, FILT_SAMP
  REAL(DP)                                   :: g1, g2
  INTEGER                                    :: NUMW, NFILT, j1, j2
  REAL(DP),         ALLOCATABLE              :: WAVE(:)
  !
END MODULE LINE_PARAM
