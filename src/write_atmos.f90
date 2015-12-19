SUBROUTINE WRITE_ATMOS(ATMOS)

  USE CONS_PARAM
  USE INPUT_PARAM

  IMPLICIT NONE

  INTEGER                                       :: I
  REAL(DP) , DIMENSION(10)                      :: ATMOS

	OPEN (UNIT=97, FILE = ATMOSOUT_PATH, ACTION="write", FORM='formatted')
	DO I = 1, 10
	WRITE (UNIT=97, FMT=*) ATMOS(I)
	ENDDO
	CLOSE(97)
END SUBROUTINE WRITE_ATMOS
