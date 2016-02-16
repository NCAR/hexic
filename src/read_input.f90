SUBROUTINE read_input

!
! R. Centeno
! April 15, 2014
!
! Reads paths and filenames of input and output data,
! always located under USER_FILES/INPUT.txt
!
USE CONS_PARAM
USE INPUT_PARAM
USE INV_PARAM

IMPLICIT NONE

  CHARACTER (LEN = 256) :: Line = ' '
  LOGICAL               :: file_exists

  OPEN (UNIT=97, FILE = "/Users/mgalloy/software/hexic/USER_FILES/INPUT.txt", ACTION="read", FORM='formatted')
     READ (UNIT=97, FMT='(A)') Line
     DO WHILE (SCAN(Line,'!#').GT.0)
        READ(UNIT=97, FMT='(A)') Line
     ENDDO
     DO WHILE (TRIM(Line).EQ.'')
        READ(UNIT=97, FMT='(A)') LINE
     ENDDO
     MODE = Line
     READ (UNIT=97, FMT='(A)') Line
     DO WHILE (SCAN(Line,'!#').GT.0)
        READ(UNIT=97, FMT='(A)') Line
     ENDDO
     DO WHILE (TRIM(Line).EQ.'')
        READ(UNIT=97, FMT='(A)') LINE
     ENDDO
     OBS_PATH = Line
     READ (UNIT=97, FMT='(A)') Line
     DO WHILE (SCAN(Line,'!#').GT.0)
        READ(UNIT=97, FMT='(A)') Line
     ENDDO
     DO WHILE (TRIM(Line).EQ.'')
        READ(UNIT=97, FMT='(A)') LINE
     ENDDO
     ATOM_PATH = Line
     READ (UNIT=97, FMT='(A)') Line
     DO WHILE (SCAN(Line,'!#').GT.0)
        READ(UNIT=97, FMT='(A)') Line
     ENDDO
     DO WHILE (TRIM(Line).EQ.'')
        READ(UNIT=97, FMT='(A)') LINE
     ENDDO
     SYN_PATH = Line
     READ (UNIT=97, FMT='(A)') Line
     DO WHILE (SCAN(Line,'!#').GT.0)
        READ(UNIT=97, FMT='(A)') Line
     ENDDO
     DO WHILE (TRIM(Line).EQ.'')
        READ(UNIT=97, FMT='(A)') LINE
     ENDDO
     ATMOSOUT_PATH = Line
     if (DEBUG) then
         PRINT*, '     atmosout = ', atmosout_path
     endif

     READ (UNIT=97, FMT='(A)') Line
     DO WHILE (SCAN(Line,'!#').GT.0)
        READ(UNIT=97, FMT='(A)') Line
     ENDDO
     DO WHILE (TRIM(Line).EQ.'')
        READ(UNIT=97, FMT='(A)') LINE
     ENDDO
     ATMOSIN_PATH = Line
     READ (UNIT=97, FMT='(A)') Line
     DO WHILE (SCAN(Line,'!#').GT.0)
        READ(UNIT=97, FMT='(A)') Line
     ENDDO
     DO WHILE (TRIM(Line).EQ.'')
        READ(UNIT=97, FMT='(A)') LINE
     ENDDO
     WEIGHTS_PATH = Line
     READ (UNIT=97, FMT='(A)') Line
     DO WHILE (SCAN(Line,'!').GT.0)
        READ(UNIT=97, FMT='(A)') Line
     ENDDO
     NOISE_PATH = Line
     READ (UNIT=97, FMT='(A)') Line
     DO WHILE (SCAN(Line,'!#').GT.0)
        READ(UNIT=97, FMT='(A)') Line
     ENDDO
     DO WHILE (TRIM(Line).EQ.'')
        READ(UNIT=97, FMT='(A)') LINE
     ENDDO
     SCAT_PATH = Line
     READ (UNIT=97, FMT='(A)') Line
     DO WHILE (SCAN(Line,'!#').GT.0)
        READ(UNIT=97, FMT='(A)') Line
     ENDDO
     DO WHILE (TRIM(Line).EQ.'')
        READ(UNIT=97, FMT='(A)') LINE
     ENDDO
     FREE_PATH = Line
        READ (UNIT=97, FMT='(A)') Line
     DO WHILE (SCAN(Line,'!#').GT.0)
        READ(UNIT=97, FMT='(A)') Line
     ENDDO
     DO WHILE (TRIM(Line).EQ.'')
        READ(UNIT=97, FMT='(A)') LINE
     ENDDO
     FILT_PATH = Line
  CLOSE(97)

if (DEBUG) then
    IF ((MODE .EQ. 'i') .OR. (MODE .EQ. 'I')) THEN
       PRINT*, ' --- Mode = INVERSION'
    ELSE
       PRINT*, ' --- Mode = SYNTHESIS'
    ENDIF
endif

! Weights and noise (only in inversion mode)
IF ((MODE .EQ. 'i') .OR. (MODE .EQ. 'I')) THEN

   INQUIRE(FILE=WEIGHTS_PATH, EXIST = file_exists)
   IF (file_exists) THEN
      if (DEBUG) then
          PRINT*, ' --- Reading weights file...'
      endif
      CALL READ_WEIGHTS
      if (DEBUG) then
          PRINT*, '     WEIGHTS = ', WEIGHTS(:)
      endif
   ELSE
      if (DEBUG) then
          PRINT*, " --- No WEIGHTS provided. Assuming [1, 10, 10, 3] for [I, Q, U, V]."
      endif
      WEIGHTS(:) = (/0.1,1.,1.,0.3/)
   ENDIF

   INQUIRE(FILE=NOISE_PATH, EXIST = file_exists)
   IF (file_exists) THEN
      if (DEBUG) then
          PRINT*, ' --- Reading NOISE file...'
      endif
      CALL READ_NOISE
      if (DEBUG) then
          PRINT*, '     NOISE = ', NOISE(:)
      endif
   ELSE
      if (DEBUG) then
          PRINT*, " --- No NOISE provided. Assuming [1D-3, 1D-3, 1D-3, 1D-3] of Icont."
      endif
      NOISE(:) = (/1.0D-3, 1.0D-3, 1.0D-3, 1.0D-3/)
   ENDIF
ENDIF


END SUBROUTINE READ_INPUT
