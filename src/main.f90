PROGRAM main

USE CONS_PARAM
USE INPUT_PARAM
USE LINE_PARAM
USE RAN_MOD
USE INV_PARAM
USE FORWARD

IMPLICIT NONE

  REAL(DP), ALLOCATABLE            :: OBSERVATIONS(:,:,:), SYNTHETIC(:,:,:), RESULTS(:,:)
  REAL(DP), ALLOCATABLE            :: OBS(:,:), SYN(:,:), SCAT(:,:)
  REAL(DP), ALLOCATABLE            :: DSYN(:, :,:)
  REAL(DP), ALLOCATABLE            :: FILTERS(:,:)
  REAL(DP), DIMENSION(11)          :: MODEL, RES
  REAL(DP), DIMENSION(13)          :: ERR
  REAL(DP), DIMENSION(8)           :: WFILT
  LOGICAL                          :: file_exists

  INTEGER                          :: i, j, k, convergence_flag

 

  CALL READ_INPUT
  CALL READ_LINE

  ! ------ Allocate memory for the variables
  ! First, the variables for the field of view we're going to invert
  ! The value of NPIX should be passed to this program. 
  ALLOCATE(OBSERVATIONS(NFILT, 4, NPIX))   ! input 
  ALLOCATE(RESULTS(11, NPIX), SYNTHETIC(NFILT, 4, NPIX)) !output
 
 ! Allocate memory for the variables for each pixel inversion
  ALLOCATE(SYN(NFILT, 4), SCAT(NFILT, 4), DSYN(11, NFILT, 4))
  ALLOCATE(FILTERS(NUMW, NFILT))
  FILTERS(:,:) = 0D0

 ! ------ Read instrument filter profiles
  CALL READ_FILT(FILTERS)
  PRINT*, ' --- Reading filter profiles from:'
  PRINT*, '          ', FILT_PATH

 ! ------ Read (guess) model atmosphere
  CALL READ_MODEL(MODEL)
 ! ------ Read Scattered light profile
  CALL READ_SCAT(SCAT)


 ! ********* Read observed Stokes profiles if we're performing an inversion ********
  IF (mode .EQ. 'i') THEN
     ALLOCATE(OBS(NFILT,4)) 
     CALL READ_OBS(OBSERVATIONS)
  ENDIF ! **************************************************************************


 ! ------ Initialize wavelength vector, free model parameters and inversion parameters
  CALL WAVE_INIT
  IF (mode .EQ. 'i') THEN 
     CALL FREE_INIT
  ENDIF
 


  IF (MODE .EQ. 'i') THEN ! INVERSION MODE
     
     ! ------- Loop over all pixels in the FOV. Calls main part of the program NPIX times.

     DO k = 1, NPIX

        OBS(:,:) = OBSERVATIONS(:,:,k)
        CALL INV_INIT(MAXVAL(OBS(:,1)))
        ! This should have been called outside the loop but it sets the continuum intensity.

        CALL NEW_INVERT(OBS, SCAT, MODEL, RES, ERR, CONVERGENCE_FLAG, FILTERS)
        CALL SYNTHESIS(RES, SCAT, .FALSE., SYN, DSYN, FILTERS)
        SYNTHETIC(:,:,k) = SYN(:,:)
        RESULTS(:,k) = RES(:)

        ! ************** These following two calls should be deprecated *************
        CALL WRITE_SYN(SYN)
        CALL WRITE_ATMOS(RES)

        PRINT*, ' --- RESULTS = ', RES(1:4)
        PRINT*, '               ', RES(5:8)
        PRINT*, '               ', RES(9:10)
       
     ENDDO

  ELSE ! SYNTHESIS MODE: calculate Stokes profiles from model atmosphere
     ! The synthesis is typically not in a loop. Should be run for NPIX=1.
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
