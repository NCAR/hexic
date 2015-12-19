SUBROUTINE FREE_INIT 
  !
  ! J M Borrero
  ! Dec 14, 2009
  ! HAO-NCAR for HMI-Stanford
  !
  USE INV_PARAM
  USE LINE_PARAM

  IMPLICIT NONE

  INTEGER, DIMENSION(10)         :: LIST_FREE_PARAMS
  INTEGER                        :: I,J

  !
  FREE(:) = .FALSE.
  NUMFREE_PARAM = 0
  NUMFREE_DEG = 4 * NFILT
  LIST_FREE_PARAMS(:) = 0
  !

  CALL READ_FREE(LIST_FREE_PARAMS)

  DO I=1,10
     IF (LIST_FREE_PARAMS(I).EQ.1) THEN
        FREE(I) = .TRUE.
        NUMFREE_PARAM = NUMFREE_PARAM+1
        NUMFREE_DEG = NUMFREE_DEG - 1
     ENDIF
  ENDDO
  
  ALLOCATE (FREELOC(NUMFREE_PARAM))
  J = 1
  DO I = 1,10
     IF (FREE(I).EQV..TRUE.) THEN
        FREELOC(J) = I
        J = J+1
     ENDIF
  ENDDO



  !FREE(1)                                    ! ETA0
  !FREE(2)                                    ! FIELD INCLINATION
  !FREE(3)                                    ! FIELD AZIMUTH
  !FREE(4)                                    ! DAMPING
  !FREE(5)                                    ! DOPPLER WIDTH
  !FREE(6)                                    ! FIELD STREGNTH
  !FREE(7)                                    ! MAGNETIC LOS VELOCITY
  !FREE(8)                                    ! SOURCE FUNCTION CONTINUUM
  !FREE(9)                                    ! SOURCE FUNCTION GRADIENT
  !FREE(10)                                   ! MAGNETIC FILLING FACTOR
  !FREE(11)                                   ! MACROTURBULENT VELOCITY
END SUBROUTINE FREE_INIT
