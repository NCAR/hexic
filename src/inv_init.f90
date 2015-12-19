SUBROUTINE INV_INIT(ICONT)
  !
  ! J M Borrero
  ! Dec 14, 2009
  ! HAO-NCAR for HMI-Stanford
  !
  USE INV_PARAM
  IMPLICIT NONE
  REAL(DP)                              :: ICONT


  !
  ITER = 50  !NUM_ITERATIONS
  SVDTOL = 1D-15 !SVD_TOLERANCE
  TREIC = 0.1  ! INTENSITY_THRESHOLD --- this parameter needs to be removed from the code!!!


 !--- Iteration and re-initialization control
  ! The following parameters control the iterations, the convergence criterion and 
  ! if and when a pixel is re-setted (inversion re-initialized with different guess model)


  DELTACHIMIN = 1D-4 ! Min change of chi2 to decrease lambda for.
  LAMBDA_START = 5D0 ! Initial value of lambda
  LAMBDA_RESET = 5D0 ! Value of lambda after a reset
  LAMBDA_DOWN = 5D0 ! How much to decrease lambda for good guess
  LAMBDA_UP = 10D0 ! How much to increase lambda for bad guess
  LAMBDA_MIN = 1D-4 ! Min lambda value
  LAMBDA_MAX = 100D0 ! Max lambda value (exit criterion)
  N_ABANDON = 5 ! Abandon 1st reset if no better than this many iterations.

! Used to be in inv_utils.f90
! Limits for the model parameters. Used in FINE_TUNE_MODEL
! Order: eta0, inclination (deg), azimuth (deg), damping, Doppler width, 
! field strength (gauss), line-of-sight velocity (cm/s), source function, 
! source function gradient, filling factor


  LOWER_LIMIT = (/1D0, 0D0 , 0D0 , 1D-4, 5D0, 1D0, -7D5, 1.5E-1*ICONT, 1.5E-1*ICONT, 0D0/)
  UPPER_LIMIT  = (/8D1, 180D0, 180D0, 5D0, 1D2, 5D3, 7D5, 1.2D0*ICONT, 1.2D0*ICONT, 1D0/)


! Normalization vector for the derivatives, so that the values of the Hessian matrix are 
! similar in magnitude
  !NORM(:)=(/25D0,90D0,90D0,1D0,50D0,1500D0,1E5_DP,0.5D0*ICONT,0.5D0*ICONT,0.5D0/)
 ! NORM(:)=(/0.2D0,7D0,50D0,1D0,0.4D0,50D0,5000D0,0.003D0*ICONT,0.004D0*ICONT,0.5D0/)
  NORM(:)=(/15D0,25D0,25D0,1D0,10D0,1000D0,1D5,0.5D0*ICONT,0.5D0*ICONT,0.5D0/)

! Limits on DMODEL
! I have tried with more generous limits at this is the best combination I could find
  DLIMIT(:)=(/5D0,40D0,40D0,0.2D0,5D0,500D0,10E5_DP,0.5D0*ICONT,0.5D0*ICONT,0.25D0/)
! Relative limits. 0 means don't use
  RLIMIT(:)=(/0.0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0,0D0/)
 


  !
END SUBROUTINE INV_INIT
