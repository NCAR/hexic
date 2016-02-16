SUBROUTINE READ_SCAT(SCAT)

  USE LINE_PARAM
  USE CONS_PARAM
  USE INPUT_PARAM

  IMPLICIT NONE

  REAL(DP)                                      :: SCAT(NFILT,4)
  INTEGER                                       :: I
  LOGICAL                                       :: FILE_EXISTS

   INQUIRE(FILE=SCAT_PATH, EXIST = file_exists)
   IF (file_exists) THEN
      if (DEBUG) then
          PRINT*, ' --- Reading Scattered Light file...'
      endif
      OPEN (UNIT=97, FILE = SCAT_PATH, ACTION="read")!, FORM='formatted')
      DO I = 1, NFILT
         READ (UNIT=97, FMT=*) SCAT(I,1),SCAT(I,2),SCAT(I,3),SCAT(I,4)
      ENDDO
      CLOSE(97)
   ELSE
      if (DEBUG) then
          PRINT*, " --- No Scattered Light file provided. Assuming ZERO"
      endif
      SCAT(:,:) = 0D0
   ENDIF


END SUBROUTINE READ_SCAT
