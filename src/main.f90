PROGRAM main

USE CONS_PARAM
USE INPUT_PARAM
USE LINE_PARAM
USE RAN_MOD
USE INV_PARAM
USE FORWARD

IMPLICIT NONE

  REAL(DP), ALLOCATABLE            :: OBS(:,:), SYN(:,:), SCAT(:,:)
  REAL(DP), ALLOCATABLE            :: DSYN(:, :,:)
  REAL(DP), ALLOCATABLE            :: FILTERS(:,:)
  REAL(DP), DIMENSION(11)          :: MODEL, RES
  REAL(DP), DIMENSION(13)          :: ERR
  REAL(DP), DIMENSION(8)           :: WFILT
  LOGICAL                          :: file_exists

  INTEGER                          :: i, j, convergence_flag

 

  CALL READ_INPUT
  CALL READ_LINE

  ! Allocate memory for the variables
  ALLOCATE(SYN(NFILT, 4), SCAT(NFILT, 4), DSYN(11, NFILT, 4))
  ALLOCATE(FILTERS(NUMW, NFILT))
  FILTERS(:,:) = 0D0
 ! Read instrument filter profiles
  CALL READ_FILT(FILTERS)
  PRINT*, ' --- Reading filter profiles from:'
  PRINT*, '          ', FILT_PATH

  ! Read (guess) model atmosphere
  CALL READ_MODEL(MODEL)
  ! Read observed Stokes profiles if we're performing an inversion
  IF (mode .EQ. 'i') THEN
     ALLOCATE(OBS(NFILT,4)) 
     CALL READ_OBS(OBS)
  ENDIF

  ! Read Scattered light profile
  CALL READ_SCAT(SCAT)
  ! Initialize wavelength vector and free model parameters
  CALL WAVE_INIT
  CALL FREE_INIT
  ! Initialize inversion parameters if in inversion mode
  IF (mode .EQ. 'i') CALL INV_INIT(MAXVAL(OBS(:,1)))

 
  IF (MODE .EQ. 'i') THEN
     CALL NEW_INVERT(OBS, SCAT, MODEL, RES, ERR, CONVERGENCE_FLAG, FILTERS)
     CALL WRITE_ATMOS(RES)
     PRINT*, ' --- RESULTS = ', RES(1:4)
     PRINT*, '               ', RES(5:8)
     PRINT*, '               ', RES(9:10)
     !ATMOSIN_PATH = ATMOSOUT_PATH
     CALL SYNTHESIS(RES, SCAT, .FALSE., SYN, DSYN, FILTERS)
     CALL WRITE_SYN(SYN)

  ELSE ! SYNTHESIS MODE: calculate Stokes profiles from model atmosphere
     CALL READ_MODEL(MODEL)
     CALL SYNTHESIS(MODEL, SCAT, .FALSE., SYN, DSYN, FILTERS)
     CALL WRITE_SYN(SYN)
     PRINT*, '  '
     
     PRINT*, ' --- Synthetic profiles written in: '
     PRINT*, '   ', SYN_PATH
     PRINT*, ' ... according to model atmosphere from: '
     PRINT*, '   ', ATMOSIN_PATH

  ENDIF

 
END PROGRAM main
