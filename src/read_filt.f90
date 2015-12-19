SUBROUTINE READ_FILT(FILTERS)

  USE LINE_PARAM
  USE CONS_PARAM
  USE INPUT_PARAM

  IMPLICIT NONE
 
  REAL(DP)                                      :: FILTERS(NUMW,NFILT)
  INTEGER                                       :: I


  OPEN (UNIT=97, FILE = FILT_PATH, ACTION="read")!, FORM='formatted')
  DO I = 1, NUMW
     READ (UNIT=97, FMT=*) FILTERS(I,:)
  ENDDO
  CLOSE(97)
 
END SUBROUTINE READ_FILT
