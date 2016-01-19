SUBROUTINE READ_LINE

!
! R. Centeno
! April 15, 2014
! 
! Reads the input parameter values for the spectral line, in LINEinit.txt
!
USE LINE_PARAM
USE INPUT_PARAM

IMPLICIT NONE

  CHARACTER (LEN = 256) :: Line = ' '
  INTEGER               :: i
  
  OPEN (UNIT=97, FILE = ATOM_PATH, ACTION="read", FORM='formatted')
     READ (UNIT=97, FMT='(A)') Line
     READ (UNIT=97, FMT='(A)') Line
     READ (UNIT=97, FMT='(A)') Line
     READ (UNIT=97, FMT='(I4)') NUMW
     READ (UNIT=97, FMT='(A)') Line
     READ (UNIT=97, FMT='(F5.2)') STEPW
     READ (UNIT=97, FMT='(A)') Line
     READ (UNIT=97, FMT='(F7.2)') LAM_START
!     READ (UNIT=97, FMT='(A)') Line
!     READ (UNIT=97, FMT='(I4)') NFILTS
     READ (UNIT=97, FMT='(A)') Line
     READ (UNIT=97, FMT='(F5.2)') FILT_SAMP
     READ (UNIT=97, FMT='(A)') Line
     READ (UNIT=97, FMT='(F10.5)'), LANDA0
     READ (UNIT=97, FMT='(A)') Line
     READ (UNIT=97, FMT='(F3.1)') g1
     READ (UNIT=97, FMT='(F3.1)') g2
     READ (UNIT=97, FMT='(I2)') j1
     READ (UNIT=97, FMT='(I2)') j2
  CLOSE(97)


END SUBROUTINE READ_LINE
