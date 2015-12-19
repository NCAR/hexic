SUBROUTINE NEW_INVERT(OBS, SCAT, GUESS, RES, ERR, CONVERGENCE_FLAG, FILTERS)

  USE CONS_PARAM
  USE LINE_PARAM
  USE INV_PARAM
  USE INV_UTILS
  USE FORWARD
  USE RAN_MOD

  IMPLICIT NONE

  !---------------------------------------------------------------------

  REAL(DP), INTENT(INOUT),  DIMENSION(10)            :: GUESS
  REAL(DP), INTENT(OUT), DIMENSION(10)               :: RES
  REAL(DP), INTENT(OUT), DIMENSION(13)               :: ERR
  REAL(DP), INTENT(IN), DIMENSION(NUMW, NFILT)       :: FILTERS

  !---------------------------------------------------------------------
  REAL(DP),        DIMENSION(NFILT,4)   :: OBS, SCAT
  REAL(DP),        DIMENSION(10,ITER)   :: MODEL

  REAL(DP),        DIMENSION(10)        :: BESTMODEL, MODELG, LASTGOODMODEL 
  REAL(DP),        DIMENSION(10)        :: DMODEL, BESTMINMODEL
  REAL(DP),        DIMENSION(16)        :: SIGMA
  REAL(DP)                              :: BESTCHI2, LASTGOODCHI2, NEWCHI2
  REAL(DP)                              :: DELTACHI2, BESTMINCHI2
  REAL(DP),     DIMENSION(NFILT,4)      :: SYN, LASTGOODSYN, BESTSYN
  REAL(DP),     DIMENSION(10,NFILT,4)   :: DSYN, LASTGOODDSYN
  REAL(DP),     DIMENSION(ITER)         :: LAMBDA, CHI2
  REAL(DP),     ALLOCATABLE             :: HESS(:,:), DIVC(:)
  REAL(DP)                              :: TOTPOL, ICONT 
  REAL(DP)                              :: LASTGOODLAMBDA, LAMBDAX
  INTEGER                               :: I, J, K, M, NRESET
  INTEGER                               :: DONE, ITSTART, ABANDON
  INTEGER                               :: CONV_FLAG, CONVERGENCE_FLAG, CHANGEVAR_FLAG 
  INTEGER                               :: RESET_FLAG, LAMBDA_EXIT_FLAG
  CHARACTER(LEN=20), PARAMETER          :: FMT = '("6f14.10")'
! ------------------    Some variables for random number initialization
  INTEGER                               :: dt_return(1:8), nseed,clock_count
  INTEGER, dimension(:), allocatable    :: seed

! Allocate memory for the Hessian matrix and the divergence vector. 
  ALLOCATE(HESS(NUMFREE_PARAM,NUMFREE_PARAM),DIVC(NUMFREE_PARAM))

! Continuum intensity

  ICONT = MAXVAL(OBS(:,1))
  PRINT*, ' --- ICONT = ', ICONT

  ! ----- FLAGS FOR THE CODE
  ! By RCE: 
  ! CONVERGENCE_FLAG is sent out to the wrapper with different values depending 
  !   on whether the algorithm converges or not and why.
  ! CHANGEVAR_FLAG decides whether variable change for inversion is implemented (1) or not (0).
  
  CONVERGENCE_FLAG = 0
  CHANGEVAR_FLAG = 0 ! Use new variables or not
  ! ---- END FLAGS

 

  !--- Get seed for random generator
  ! Both values increment with time, so not entirely independent
  CALL date_and_time(values=dt_return)
  CALL system_clock(count=clock_count,count_rate=i,count_max=j)
  
!  CALL random_seed(size=nseed)
!  ALLOCATE(seed(1:nseed))
!  seed(:) = dt_return(8) ! Milliseconds of system clock
!  seed(1) = clock_count ! Something to do with time spent

  !---------------------------------------------------------------------------------
  ! This IF statement checks whether the intensity is large enough to invert the data
  ! Otherwise the pixel is ignored. This is used to avoid pixels off-the-limb
  !---------------------------------------------------------------------------------
  IF (ICONT .GT. TREIC) THEN

!     CALL GET_TOTPOL(OBS,TOTPOL)

     !----------------------------------------------------
     ! Commented by RCE: 
     ! WFA_GUESS overwrites initial guess for Doppler velocity 
     ! eta0, doppler width, azimuth, source function and gradient.
     !----------------------------------------------------

     CALL WFA_GUESS(OBS, GUESS)

     ! Initialization of non-free parameters
    ! IF (FREE(4).EQV..FALSE.) GUESS(4)=0.5D0
    ! IF (FREE(10).EQV..FALSE.) GUESS(10)=1D0

     CALL FINE_TUNE_MODEL(GUESS)

     MODEL(:,1) = GUESS
     BESTMODEL(:) = GUESS

     ! By RCE: Weird default values for lambda and chi2 so that I can find out
     ! when they are not being updated.
     LAMBDA(:) = 33.3333
     CHI2(:) = 33.3333333
     LASTGOODCHI2 = 1E24
     LAMBDA(1) = LAMBDA_START
     LAMBDA_EXIT_FLAG = 0 ! Counter for consecutive iterations where lambda = lambda_min
     BESTCHI2=1E24 ! Best chi2 ever seen
     BESTMINCHI2 = 1E24 ! Best chi2 value actually converged
     BESTMINMODEL = 1E24 ! Best model actually converged
     ITSTART=1 ! Start of current iteration

     I=1

     ! Counter for the number of random resets that the model parameters suffer.
     ! RESET_FLAG to flag when the inversion of a pixel needs to be resetted.
     ! DONE flags the exit criterion for the main loop.
     NRESET=0
     RESET_FLAG=0
     DONE=0

     !-----------------------------
     ! Start LOOP iteration
     !-----------------------------
     DO WHILE ((I.LT.ITER).and.(DONE.EQ.0))       

        ! Restart with new guess?
        IF (RESET_FLAG .NE. 0) THEN
  !          PRINT*, "     -- Enforcing a restart"
           BESTMINCHI2 = BESTCHI2    ! Save current best chi2
           BESTMINMODEL = BESTMODEL  ! Save current best chi2
           ITSTART = I ! Remember where we started
           MODELG = BESTMODEL
           CALL RANDOM_MODEL_JUMP(MODELG)
           CALL FINE_TUNE_MODEL(MODELG)
           MODEL(:,I) = MODELG
           LASTGOODMODEL = MODEL(:,I)
           LASTGOODCHI2 = 1E24
           LAMBDA(I) = LAMBDA_RESET
           NRESET = NRESET + 1
        ENDIF !Restart with a new guess


        !--- Get chi2 only, see if it is better and only calculate derivatives if it is.
        CALL SYNTHESIS(MODEL(:,I), SCAT, .FALSE., SYN, DSYN, FILTERS)
        CALL GET_CHI2(MODEL(:,I), SYN, OBS, NEWCHI2)

        ! Checking for NAN in NEWCHI2
        IF (NEWCHI2.EQ.NEWCHI2+1D0) THEN
           PRINT*,'NaN detected in Subroutine GETCHI2'
           NEWCHI2=2*LASTGOODCHI2
        ENDIF
        CHI2(I) = NEWCHI2
        DELTACHI2 = LASTGOODCHI2 - NEWCHI2  ! change in chi2 value


        IF (DELTACHI2.GE.0.0) THEN ! ---- Things got better

           ! If things get better and lambda equals lambda_min, we add one to the counter
           IF (LAMBDA(I).EQ.LAMBDA_MIN) LAMBDA_EXIT_FLAG = LAMBDA_EXIT_FLAG + 1

           ! Get synthetic profiles and derivatives
           CALL SYNTHESIS(MODEL(:,I),SCAT,.TRUE.,SYN,DSYN, FILTERS)
           ! Normalize derivatives
           CALL NORMALIZE_DSYN(DSYN)
           ! Set to Zero unneeded derivatives
           CALL ZERO_DSYN(DSYN)
           
           LASTGOODLAMBDA = LAMBDA(I)
           LASTGOODMODEL = MODEL(:,I)
           LASTGOODCHI2 = NEWCHI2
           LASTGOODSYN = SYN
           LASTGOODDSYN = DSYN

           IF (NEWCHI2.LT.BESTCHI2) THEN
              BESTMODEL = MODEL(:,I)
              BESTCHI2 = NEWCHI2
              BESTSYN = SYN
           ENDIF

        ELSE        ! ---- Things did not get better
           LAMBDA_EXIT_FLAG = 0
           SYN = LASTGOODSYN
           DSYN = LASTGOODDSYN
           MODEL(:,I) = LASTGOODMODEL
        ENDIF       ! ---- End IF DELTACHI2 >= 0

        ! Levenberg-Marquardt lambda factor
        CALL GET_LAMBDA(DELTACHI2, LAMBDA(I), LAMBDA(I+1))
    
        ! Getting Divergence and Hessian
        CALL GET_DIVC(LASTGOODSYN, OBS, LASTGOODDSYN, DIVC)
        CALL GET_HESS(LASTGOODDSYN, HESS)
     
        ! Get perturbations to old model
        CALL GET_DMODEL(LASTGOODMODEL, DIVC, HESS, LAMBDA(I+1), DMODEL, CONV_FLAG)
        ! Ignore CONV_FLAG and rely on DMODEL set to zero/LAMBDA increase.

        ! Normalizing, trimming and tuning DMODEL
        ! Undoing variable change to obtain the perturbations to
        ! the original model parameters. Overwriting DMODEL!
        CALL NORMALIZE_DMODEL(DMODEL)
        CALL CUT_DMODEL(DMODEL,MODEL)
        MODEL(:,I+1)=LASTGOODMODEL + DMODEL
        CALL FINE_TUNE_MODEL(MODEL(:,I+1))

        !---- Decision making:
        !     Have we converged?
        !     Do we need to try a new initial guess for this pixel?
        !     Do we just carry on with the iterations?

        RESET_FLAG = 0  ! Do we need to reset initial parameters?
        ABANDON = 0  ! Abandon a given restart

        ! a reset (call randomized initial guess) is enforced whenever lambda gets too big.
        IF (LAMBDA(I+1) .GE. LAMBDA_MAX) RESET_FLAG = 1

        ! A given restart is not getting better. Start a new one:
     !   IF ((NRESET .NE. 0) .AND. ((i-itstart) .GE. N_ABANDON) &
     !        .AND. ((LASTGOODCHI2-BESTMINCHI2) .GT. 0)) RESET_FLAG = 1 

        ! If lambda becomes too big and the number of resets is larger than 5, we give up!
        IF ((NRESET .GT. 5) .AND. (LAMBDA(I+1) .GE. LAMBDA_MAX)) DONE = 1
        ! If the best chi2 is too high after N_ABANDON (=5) iterations
!s        IF ((NRESET.EQ.0) .AND. (I .GT. N_ABANDON) .AND. (BESTCHI2 .GT. 20.00)) RESET_FLAG = 1
            
        IF (LAMBDA_EXIT_FLAG .GT. 4) DONE = 1
        I=I+1
     ENDDO ! ----------- END  Main iteration loop

     PRINT*, '     -- Enforced ', NRESET, ' restarts'

     ! Get errors
     IF (BESTCHI2.gt.1d20) THEN
        CONVERGENCE_FLAG = 4 ! Set flag and give up
     ELSE
       
        ! We compute the derivatives with the best model parameters (without
        ! doing the change of variable afterwards), and we compute the Hessian
        ! and the errors from there.
        
        ! Recalculate chi2 and derivatives. No regularization.
        ! JS: Not clear why NEWCHI2 is recalculated.
        CALL GET_CHI2(BESTMODEL, BESTSYN, OBS, NEWCHI2)
        CALL SYNTHESIS(BESTMODEL, SCAT, .TRUE., SYN, LASTGOODDSYN, FILTERS)
        CALL NORMALIZE_DSYN(LASTGOODDSYN)

        ! Compute Hessian
        Call GET_HESS(LASTGOODDSYN, HESS)
        ! This Hessian is constructed with normalized derivatives
        CALL GET_ERR(HESS, BESTCHI2, SIGMA, CONV_FLAG)

        if (CONV_FLAG.NE.0) CONVERGENCE_FLAG = 5 ! ERROR in SVD: Set flag, otherwise proceed.

! Error values that flow out to the main program:

        ERR(1)=BESTCHI2 ! Bestchi2
        ERR(2)=SIGMA(6)  ! Bfield
        ERR(3)=SIGMA(2)  ! Gamma
        ERR(4)=SIGMA(3)  ! Phi
        ERR(5)=SIGMA(7)  ! VLOS
        ERR(6)=SIGMA(10) ! Alpha_mag
        ERR(7)=SIGMA(11) ! Bfield-Gamma
        ERR(8)=SIGMA(12) ! Bfield-Phi
        ERR(9)=SIGMA(13) ! Gamma-Phi
        ERR(10)=SIGMA(14) ! Bfield-Alpha
        ERR(11)=SIGMA(15)! Gamma-Alpha
        ERR(12)=SIGMA(16)! Phi-Alpha

        ! Final Result
        RES = BESTMODEL
        
        ! By RCE, Apr 23, 2010: Juanma's correction to azimuth (which is
        ! rotated by 90 degrees with respect to the convention people want).
!        RES(3) = RES(3) + 90D0
!        IF (RES(3) .GT. 180D0) RES(3) = RES(3) - 180D0

        IF ((I .EQ. ITER).and.(NRESET.eq.0)) THEN 
           CONVERGENCE_FLAG = 2 ! Not converged
        ELSE
           CONVERGENCE_FLAG = 0 ! Found at least one local minimum.
        ENDIF
     ENDIF ! Decent chisq

  ELSE ! Intensity too low to invert
     CONVERGENCE_FLAG = 1

  ENDIF !(Icont GT intensity threshold)
     
END SUBROUTINE NEW_INVERT
