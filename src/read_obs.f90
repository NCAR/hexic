SUBROUTINE READ_OBS(OBS)

  USE LINE_PARAM
  USE CONS_PARAM
  USE INPUT_PARAM

  IMPLICIT NONE
 
  REAL(DP)                                      :: OBS(NFILT,4)
  INTEGER                                       :: I

  
  OPEN (UNIT=97, FILE = OBS_PATH, ACTION="read")!, FORM='formatted')
  DO I = 1, NFILT
     READ (UNIT=97, FMT=*) OBS(I,1),OBS(I,2),OBS(I,3),OBS(I,4)
  ENDDO
  CLOSE(97)
 
END SUBROUTINE READ_OBS
