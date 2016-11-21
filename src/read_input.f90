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

  OPEN (UNIT=97, FILE = "/Users/rce/work/HEXIC/GIT/hexic/USER_FILES/INPUT.txt", ACTION="read", FORM='formatted')
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
     ATOM_PATH = Line

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



END SUBROUTINE READ_INPUT
