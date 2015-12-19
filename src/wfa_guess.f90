SUBROUTINE WFA_GUESS(OBS, GUESS)
  ! J M Borrero
  ! Oct 28, 2007
  ! HAO-NCAR for HMI-Stanford
  USE CONS_PARAM
  USE INV_PARAM
  USE LINE_PARAM
  USE INPUT_PARAM

  IMPLICIT NONE
 
  REAL(DP), INTENT(IN), DIMENSION(NFILT,4)    :: OBS
  REAL(DP), DIMENSION(10)                     :: GUESS
  REAL(DP)                                    :: ICONT, CORE, DEPTH, G_eff, TOTPOL, temp
  REAL(DP), DIMENSION(NFILT)                  :: DerI_DerLam, MASK, Q, U, V, STokesI, WAVEF
  INTEGER                                     :: I, WAVE0, WAVE1, RED, BLUE, VMAX
  LOGICAL                                     :: file_exists

! Assuming filter profiles are centered on spectral line
   DO I = 1, NFILT 
      WAVEF(I) = FILT_SAMP * (I - NFILT/2D0 ) 
   ENDDO


! Effective Lande factor

! ---- CHECK UPPER AND LOWER g and j VALUES!!!!!!
G_eff = 1D0/2D0*(g1+g2)+1D0/4D0*(g1-g2)*(j1*(j1+1)-j2*(j2+1))

PRINT*, ' --- Effective Lande factor = ', G_eff
! Stokes parameters and total polarization
StokesI = OBS(:,1)
Q = OBS(:,2)
U = OBS(:,3)
V = OBS(:,4)

TOTPOL = SUM(SQRT(Q**2D0+ U**2D0+V**2D0))/SUM(StokesI(:))
ICONT = MAXVAL(StokesI)
CORE = MINVAL(StokesI)


! -- Eta0

DEPTH = ICONT - CORE
GUESS(1) = 1D0/DEPTH*ICONT


! -- Doppler Width (width at half depth of line)

MASK(:) = 0.0D0
WHERE (StokesI .LT. ((ICONT+CORE)/2D0)) MASK = 1D0
GUESS(5) = SUM(MASK) * FILT_SAMP / 2D0 
IF (GUESS(5) .GT. 80D0) GUESS(5) = 80D0
! -- Doppler velocity (using the width at half max)
!    (need a better approximation for the strong field case!)
WAVE0 = 0
WAVE1 = 0
DO I = 1, NFILT
  IF ((WAVE0 .EQ. 0) .AND. (MASK(I) .EQ. 1)) WAVE0 = I
  IF ((WAVE1 .EQ. 0) .AND. (MASK(NFILT-I+1) .EQ. 1)) WAVE1=NFILT-I+1
ENDDO


GUESS(7) = (WAVEF(WAVE0)+WAVEF(WAVE1))/2D0*LIGHT/LANDA0/1.0D3

!
! --- Field strength
!

PRINT*, ' --- Total Polarization = ', totpol

RED = MAXLOC(V,1)
BLUE  = MINLOC(V,1)

IF (BLUE .GT. RED) THEN
   temp = RED
   RED = BLUE
   BLUE = temp
ENDIF


IF (TOTPOL .GT. 1.8D-2) THEN   ! Strong field regime
   PRINT*, ' --- STRONG field initialization!'
   GUESS(5) = GUESS(5) /4D0  ! Full Zeeman-split --  Doppler width is overestimated.
   GUESS(1) = GUESS(1) * 2D0 ! Full Zeeman splitting -- eta0 underestimated!
 
   ! field strength obtained directly from Zeeman splitting
  
   GUESS(6) = ABS(RED - BLUE)/2D0 * FILT_SAMP * 1D-3 / (4.67E-13 * G_eff * LANDA0**2)


ELSE   ! Weak field case
   PRINT*, ' --- WEAK field initialization!'
   GUESS(6) = 3.0D2
   GUESS(5) = GUESS(5) /4D0  ! Full Zeeman-split --  Doppler width is overestimated.
   GUESS(1) = GUESS(1) * 2D0
   
ENDIF


IF (MAXVAL(ABS(V)) .LT. 3D0 * NOISE(4)) THEN 
   GUESS(2) = 90.D0
ELSE
   IF (OBS(BLUE,4)/ABS(OBS(BLUE,4)) .EQ. -1) GUESS(2)  = 45.
   IF (OBS(BLUE,4)/ABS(OBS(BLUE,4)) .EQ. 1) GUESS(2)  = 135.
ENDIF

! Azimuth
GUESS(3) = ATAN2(sum(U),sum(Q))*90/DPI

! The source function terms
    GUESS(8) = 0.7D0 * ICONT
    GUESS(9) = 0.3D0 * ICONT

! If there is a scattered light profile provided, start with a value of 0.8 for the filling factor.

    INQUIRE(FILE=SCAT_PATH, EXIST = file_exists)
    IF (file_exists) THEN
       GUESS(10) = 0.8D0 
    ELSE 
       GUESS(10) = 1.0D0
    ENDIF

!Test by RCE on Dec 14, 2015 for HMI DATA:

!GUESS(7) = 110000.0

PRINT*, ' --- GUESS model = ', GUESS(1:4)
PRINT*, '                   ', GUESS(5:8)
PRINT*, '                   ', GUESS(9:10)
PRINT*, ' '
 
END SUBROUTINE WFA_GUESS
