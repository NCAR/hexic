MODULE INV_PARAM
  !
  ! J M Borrero
  ! Dec 14, 2009
  ! HAO-NCAR for HMI-Stanford
  !

  USE CONS_PARAM
  
  IMPLICIT NONE

  LOGICAL,  DIMENSION(10)            :: FREE
  INTEGER                            :: ITER, NUMFREE_PARAM, NUMFREE_DEG
  REAL(DP)                           :: SVDTOL
  REAL(DP)                           :: TREIC
  REAL(DP)                           :: RANDOM_JUMP
  INTEGER,     ALLOCATABLE           :: FREELOC(:)
  REAL(DP), DIMENSION(4)             :: WEIGHTS, NOISE

  REAL(DP)                           :: LAMBDA_MIN, LAMBDA_MAX, LAMBDA_START
  REAL(DP)                           :: LAMBDA_UP, LAMBDA_DOWN, LAMBDA_RESET

  REAL(DP)                           :: DELTACHIMIN, N_ABANDON
  REAL(DP), DIMENSION(10)            :: DLIMIT,RLIMIT
  REAL(DP), DIMENSION(10)            :: LOWER_LIMIT,UPPER_LIMIT

  REAL(DP), DIMENSION(10)            :: NORM

  !
END MODULE INV_PARAM
