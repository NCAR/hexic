integer function run_hexic(OBSERVATIONS, dimX, dimY, Nfilts, RESULTS, SYNTHETIC)

use iso_c_binding, only: c_double, c_int

USE CONS_PARAM
USE INPUT_PARAM
USE LINE_PARAM
USE RAN_MOD
USE INV_PARAM
USE FORWARD

IMPLICIT NONE

! arguments to run_hexic
integer(c_int), intent(in)         :: Nfilts, dimX, dimY
real(c_double), intent(in)         :: OBSERVATIONS(Nfilts, 4, dimX, dimY)
real(c_double), intent(out)        :: SYNTHETIC(Nfilts, 4, dimX, dimY)
real(c_double), intent(out)        :: RESULTS(11, dimX, dimY)

INTEGER                            :: k, l, s, p, convergence_flag
! The next two variables will be passed through the header
REAL(DP), ALLOCATABLE              :: OBS(:,:), SYN(:,:), SCAT(:,:)
REAL(DP), ALLOCATABLE              :: DSYN(:, :,:)
REAL(DP), ALLOCATABLE              :: FILTERS(:,:)
REAL(DP), DIMENSION(11)            :: MODEL, RES
REAL(DP), DIMENSION(13)            :: ERR
REAL(DP), DIMENSION(8)             :: WFILT
LOGICAL                            :: file_exists

 ! ------ Read main input file and atomic line file
  CALL READ_INPUT
  CALL READ_LINE
 Nfilt = Nfilts
 Nx = dimX
 Ny= dimY


 ! ------ Allocate memory for the variables that depend on user input
  ALLOCATE(DSYN(11, NFILT, 4), FILTERS(NUMW, NFILT))
  ALLOCATE(OBS(NFILT, 4), SYN(NFILT, 4), SCAT(NFILT, 4))
  FILTERS(:,:) = 0D0
 ! ------ Read instrument filter profiles
  CALL READ_FILT(FILTERS)

  if (DEBUG) then
      PRINT*, ' --- Reading filter profiles from:'
      PRINT*, '          ', FILT_PATH
  endif

 ! ------ Read (guess) model atmosphere
  CALL READ_MODEL(MODEL)
 ! ------ Read Scattered light profile
  CALL READ_SCAT(SCAT)


 ! ------ Initialize wavelength vector, free inversion model parameters
  CALL WAVE_INIT
  IF (mode .EQ. 'i') THEN
     CALL FREE_INIT
  ENDIF



  IF (MODE .EQ. 'i') THEN ! INVERSION MODE

     ! ------- Loop over all pixels in the FOV. Calls main part of the program NPIX times.

     DO k = 1, Nx
        DO l = 1, Ny
           OBS(:,:) = OBSERVATIONS(:,:,k,l)
           ! Initialize some inversion variables (requires Icont)
           CALL INV_INIT(MAXVAL(OBS(:,1)))
           ! Call inversion module and synthesize best fit to observations
           CALL NEW_INVERT(OBS, SCAT, MODEL, RES, ERR, CONVERGENCE_FLAG, FILTERS)
           CALL SYNTHESIS(RES, SCAT, .FALSE., SYN, DSYN, FILTERS)
           SYNTHETIC(:,:, k, l) = SYN(:,:)
           RESULTS(:, k, l) = RES(:)

           ! ************** These following two calls should be deprecated *************
           ! CALL WRITE_SYN(SYN)
           ! CALL WRITE_ATMOS(RES)
           if (DEBUG) THEN
               PRINT*, ' --- RESULTS = ', RES(1:4)
               PRINT*, '               ', RES(5:8)
               PRINT*, '               ', RES(9:10)
           endif
        ENDDO ! Loop in pixels along X-direction
     ENDDO  ! Loop in pixels along Y-direction

  ELSE ! SYNTHESIS MODE: calculate Stokes profiles from model atmosphere
     ! The synthesis is typically not in a loop. Should be run for Nx=1 and Ny=1.
     CALL READ_MODEL(MODEL)
     CALL SYNTHESIS(MODEL, SCAT, .FALSE., SYN, DSYN, FILTERS)
     CALL WRITE_SYN(SYN)
     if (DEBUG) then
         PRINT*, '  '

         PRINT*, ' --- Synthetic profiles written in: '
         PRINT*, '   ', SYN_PATH
         PRINT*, ' ... according to model atmosphere from: '
         PRINT*, '   ', ATMOSIN_PATH
    endif

  ENDIF
  run_hexic = 1
END function run_hexic
