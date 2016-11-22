integer function run_hexic(IN_MODE, &
                           OBSERVATIONS, dimX, dimY, Nfilts, RESULTS, SYNTHETIC, &
                           IN_MODEL, IN_WEIGHTS, IN_NOISE, IN_SCATTERED_LIGHT, &
                           IN_FREE)

use iso_c_binding, only: c_double, c_int

USE CONS_PARAM
USE INPUT_PARAM
USE LINE_PARAM
USE RAN_MOD
USE INV_PARAM
USE FORWARD

IMPLICIT NONE

! arguments to run_hexic
integer(c_int), intent(in)         :: IN_MODE
integer(c_int), intent(in)         :: Nfilts, dimX, dimY
real(c_double), intent(in)         :: OBSERVATIONS(Nfilts, 4, dimX, dimY)
real(c_double), intent(out)        :: SYNTHETIC(Nfilts, 4, dimX, dimY)
real(c_double), intent(out)        :: RESULTS(10, dimX, dimY)
real(c_double), intent(in)         :: IN_MODEL(10)
real(c_double), intent(in), optional         :: IN_WEIGHTS(4)
real(c_double), intent(in), optional         :: IN_NOISE(4)
real(c_double), intent(in), optional         :: IN_SCATTERED_LIGHT(Nfilts, 4)
integer(c_int), intent(in)         :: IN_FREE(10)

INTEGER                            :: k, l, s, p, convergence_flag
! The next two variables will be passed through the header
REAL(DP), ALLOCATABLE              :: OBS(:,:), SYN(:,:), SCAT(:,:)
REAL(DP), ALLOCATABLE              :: DSYN(:, :,:)
REAL(DP), ALLOCATABLE              :: FILTERS(:,:)
REAL(DP), DIMENSION(10)            :: MODEL, RES
REAL(DP), DIMENSION(13)            :: ERR
REAL(DP), DIMENSION(8)             :: WFILT
LOGICAL                            :: file_exists

 ! ------ Read main input file and atomic line file
  CALL READ_INPUT
  CALL READ_LINE

 Nfilt = Nfilts


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
  MODEL(:) = IN_MODEL(:)

 ! ------ Take Scattered light profile from header input
  IF (.NOT. PRESENT(IN_SCATTERED_LIGHT)) THEN
     SCAT(:,:) = 0.0
  ELSE
     SCAT(:,:) = IN_SCATTERED_LIGHT
     MODEL(10) = 0.8 !!! CAREFUL - this should not be here. Is it necessary?
  ENDIF

 ! ------ Initialize wavelength vector, free inversion model parameters
  CALL WAVE_INIT

  IF (mode .EQ. 'i') THEN
     CALL FREE_INIT(IN_FREE)
     ! ------ Get weights and noise from user input
     IF (.NOT. PRESENT(IN_NOISE)) THEN
        NOISE(:) = 1.0D-3   ! TO DO: Create routine that calculates noise from Observations?
     ELSE
        NOISE(:) = IN_NOISE(:)
     ENDIF
     IF (.NOT. PRESENT(IN_WEIGHTS)) THEN
        WEIGHTS = (/1.0, 3.5, 3.5, 2.5/)
     ELSE
        WEIGHTS(:) = IN_WEIGHTS(:)
     ENDIF
  ENDIF

  IF (MODE .EQ. 'i') THEN ! INVERSION MODE

     ! ------- Loop over all pixels in the FOV. Calls main part of the program NPIX times.

     DO k = 1, dimX
        DO l = 1, dimY
           OBS(:,:) = OBSERVATIONS(:,:,k,l)
           ! Initialize some inversion variables (requires Icont)
           CALL INV_INIT(MAXVAL(OBS(:,1)))
           ! Call inversion module and synthesize best fit to observations
           CALL NEW_INVERT(OBS, SCAT, MODEL, RES, ERR, CONVERGENCE_FLAG, FILTERS)
           CALL SYNTHESIS(RES, SCAT, .FALSE., SYN, DSYN, FILTERS)
           SYNTHETIC(:,:, k, l) = SYN(:,:)
           RESULTS(:, k, l) =  RES(:)

           ! ************** These following two calls should be deprecated *************
           ! CALL WRITE_SYN(SYN)
           ! CALL WRITE_ATMOS(RES)
           if (DEBUG) THEN
               PRINT*, ' --- RESULTS ---- = ', RES(1:4)
               PRINT*, '               ', RES(5:8)
               PRINT*, '               ', RES(9:10)
           endif
        ENDDO ! Loop in pixels along X-direction
     ENDDO  ! Loop in pixels along Y-direction

  ELSE ! SYNTHESIS MODE: calculate Stokes profiles from model atmosphere
     ! The synthesis is typically not in a loop. Should be run for Nx=1 and Ny=1.
     CALL SYNTHESIS(MODEL, SCAT, .FALSE., SYN, DSYN, FILTERS)
     !CALL WRITE_SYN(SYN)
  ENDIF
  run_hexic = 1
END function run_hexic
