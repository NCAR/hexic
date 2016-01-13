SUBROUTINE READ_OBS(OBSERVATIONS)

  USE LINE_PARAM
  USE CONS_PARAM
  USE INPUT_PARAM

  IMPLICIT NONE
 
  REAL(DP)                                      :: OBSERVATIONS(NFILT,4,NPIX)
  INTEGER                                       :: I, K

  
  OPEN (UNIT=97, FILE = OBS_PATH, ACTION="read")!, FORM='formatted')
  DO I = 1, NFILT
     READ (UNIT=97, FMT=*) OBSERVATIONS(I,1, 1),OBSERVATIONS(I,2,1),OBSERVATIONS(I,3,1),OBSERVATIONS(I,4,1)
  ENDDO
  DO k = 2, NPIX ! repeat the Stokes profiles for all NPIX pixels
     OBSERVATIONS(:,:,k) = OBSERVATIONS(:,:,1)
  ENDDO
  CLOSE(97)
 
END SUBROUTINE READ_OBS
