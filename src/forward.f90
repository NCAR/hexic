MODULE FORWARD
  !
  ! J M Borrero
  ! Jan 10, 2007
  ! HAO-NCAR for HMI-Stanford
  !
  ! By RCE, Feb 2011: Implemented changes in the derivatives that J.M Borrero
  ! suggested after changing the voigt.f function by a factor of 2. It affects
  ! the derivatives of PHI and PSI with respect to the Damping and FREC(B,R,P)
  !
  ! By RCE, April 2014: Many changes were introduced in order to generalize the
  ! code so that it can deal with real multiplets.
  ! The code now reads the atomic parameters from a text file, and these parameters
  ! include everything necessary to calculate all of the Zeeman components. 
  ! The calculation of PHIs and PSIs as well as their derivatives, has been modified
  ! to include all of the Zeeman components weighted by the 3-j coefficients.
  ! One bug was found and corrected in ABSMAT (DerRHOQ_DerDAM).
  ! 
  ! May 2014: Introducing filtergraph capability. Nfilt and Numw describe coarse
  ! and fine wavelength grids, respectively. The fine grid is the one over which the 
  ! forward modeling is done. The coarse grid is the one that we get after filtering 
  ! the data with the instruments filter profiles (also the observations wavelength grid)

CONTAINS 
  !!
  !! SUBROUTINE SYNTHESIS
  !!
  SUBROUTINE SYNTHESIS(MODEL,SCAT,DERIVATIVE,SYN,DSYN, FILTERS)
    USE LINE_PARAM
    USE CONS_PARAM
    USE INV_PARAM

    IMPLICIT NONE
    REAL(DP), INTENT(IN),  DIMENSION(10)           :: MODEL
    REAL(DP), INTENT(OUT), DIMENSION(10,NFILT,4)   :: DSYN
    REAL(DP), INTENT(IN),  DIMENSION(NFILT,4)      :: SCAT
    REAL(DP), INTENT(OUT), DIMENSION(NFILT,4)      :: SYN
    REAL(DP), INTENT(IN),  DIMENSION(NUMW, NFILT)  :: FILTERS
    LOGICAL,  INTENT(IN)                           :: DERIVATIVE


    !------------------------------------------------------
    REAL(DP),    DIMENSION(NUMW)       :: ETAI, ETAQ, ETAU, ETAV, RHOQ, RHOU, RHOV
    REAL(DP),    DIMENSION(7,NUMW)     :: DerETAI, DerETAQ, DerETAU, DerETAV
    REAL(DP),    DIMENSION(7,NUMW)     :: DerRHOQ, DerRHOU, DerRHOV
    REAL(DP),    DIMENSION(NUMW)       :: EXTRA, DET_MAT
    REAL(DP),    DIMENSION(9,NUMW,4)   :: DSTOKES_MAG 
    REAL(DP),    DIMENSION(NUMW,4)     :: STOKES_MAG 
    REAL(DP),    DIMENSION(NFILT, 4)   :: STOKES_FILT
    REAL(DP),    DIMENSION(9,NFILT,4)  :: DSTOKES_FILT
    REAL(DP),    DIMENSION(7,NUMW)     :: DEXTRA, DDMAT
    REAL(DP)                           :: S0, S1, ALPHAM
    REAL(DP),    DIMENSION(NUMW)       :: A1, A2, A3, A4, A5, A6, A7
    REAL(DP),    DIMENSION(NUMW)       :: B1, B2, B3, B4, B5, B6, B7
    REAL(DP),    DIMENSION(NUMW)       :: C1, C2, C3, C4, C5, C6
    REAL(DP),    DIMENSION(NUMW)       :: D1, D2, D3, D4, D5, D6
    REAL(DP),    DIMENSION(NUMW)       :: PART1, PART2
    INTEGER                            :: I, J, K, M, test
    !------------------------------------------------------
    S0=MODEL(8)
    S1=MODEL(9)
    ALPHAM=MODEL(10)
    !
    SYN(:,:)=0D0
    DSYN(:,:,:)=0D0
    ETAI(:)=0D0
    ETAQ(:)=0D0
    ETAU(:)=0D0
    ETAV(:)=0D0
    RHOQ(:)=0D0
    RHOU(:)=0D0
    RHOV(:)=0D0
    DerETAI(:,:)=0D0
    DerETAQ(:,:)=0D0
    DerETAU(:,:)=0D0
    DerETAV(:,:)=0D0
    DerRHOQ(:,:)=0D0
    DerRHOU(:,:)=0D0
    DerRHOV(:,:)=0D0
    STOKES_MAG(:,:)=0D0
    EXTRA(:)=0D0
    DET_MAT(:)=0D0
    DSTOKES_MAG(:,:,:)=0D0
    DEXTRA(:,:)=0D0
    DDMAT(:,:)=0D0


    CALL ABSMAT(MODEL, DERIVATIVE, ETAI, ETAQ, ETAU, ETAV, RHOQ, RHOU, RHOV, &
         DerETAI, DerETAQ, DerETAU, DerETAV, DerRHOQ, DerRHOU, DerRHOV)

    ! Common parts
    EXTRA = ETAQ*RHOQ+ETAU*RHOU+ETAV*RHOV
    DET_MAT = (ETAI**2D0)*(ETAI**2D0-ETAQ**2D0-ETAU**2D0-ETAV**2D0+ &
         RHOQ**2D0+RHOU**2D0+RHOV**2D0)-EXTRA**2D0
    !----------------------------------------------------------------------------
    ! Solution to the Unno-Rachkovski equations
    ! This is the Stokes vector coming from the magnetic atmosphere
    !----------------------------------------------------------------------------
    STOKES_MAG(:,1) = S0+(1D0/DET_MAT)*ETAI*(ETAI**2D0+RHOQ**2D0+RHOU**2D0+RHOV**2D0)*S1
    STOKES_MAG(:,2) = -(1D0/DET_MAT)*(ETAI**2D0*ETAQ+ETAI*(ETAV*RHOU-ETAU*RHOV)+RHOQ*EXTRA)*S1
    STOKES_MAG(:,3) = -(1D0/DET_MAT)*(ETAI**2D0*ETAU+ETAI*(ETAQ*RHOV-ETAV*RHOQ)+RHOU*EXTRA)*S1
    STOKES_MAG(:,4) = -(1D0/DET_MAT)*(ETAI**2D0*ETAV+ETAI*(ETAU*RHOQ-ETAQ*RHOU)+RHOV*EXTRA)*S1

    !--------------------------------------------------------------
    ! The Stokes vector gets multiplied by the filter profiles of the
    ! instrument, and then the scattered light profile is added.
    !--------------------------------------------------------------
    

    DO j = 1, Nfilt
       DO k = 1, 4
          STOKES_FILT(j,k) = SUM(FILTERS(:,j) * STOKES_MAG(:,k))
       ENDDO
    ENDDO
    SYN = (1D0-ALPHAM) * SCAT + ALPHAM * STOKES_FILT
    


    !---------------------------------------------------------
    ! Derivatives
    !---------------------------------------------------------
    IF (DERIVATIVE.EQV..TRUE.) THEN
       ! Derivatives of the Stokes Parameters (emering from Magnetic component)
       ! with respect to the 7 free parameters: eta0, gam, phi, dam, dldop, B, Vlos
       ! plus two new dependences: S0, S1. Total 9 free parameters.
       ! First derivatives of EXTRA and DET_MAT
       ! These ones do not depend on S0, S1
       DO I=1,7
          A1 = ETAI
          A2 = ETAQ
          A3 = ETAU
          A4 = ETAV
          A5 = RHOQ
          A6 = RHOU
          A7 = RHOV
          B1 = DerETAI(I,:)
          B2 = DerETAQ(I,:)
          B3 = DerETAU(I,:)
          B4 = DerETAV(I,:)
          B5 = DerRHOQ(I,:)
          B6 = DerRHOU(I,:)
          B7 = DerRHOV(I,:)
          DEXTRA(I,:)=A2*B5+A5*B2+A3*B6+A6*B3+A4*B7+A7*B4
          DDMAT(I,:)=2D0*A1*B1*(A1**2D0-A2**2D0-A3**2D0-A4**2D0+A5**2D0+ &
               A6**2D0+A7**2D0)+2D0*A1**2D0*(A1*B1-A2*B2-A3*B3-A4*B4+A5*B5+ &
               A6*B6+A7*B7)-2D0*EXTRA*DEXTRA(I,:)
       ENDDO
       ! Now derivatives of Stokes I with respect to
       ! 7 regular free parameters
       DO I=1,7
          A1 = ETAI
          B1 = DerETAI(I,:)
          A2 = RHOQ
          B2 = DerRHOQ(I,:)
          A3 = RHOU
          B3 = DerRHOU(I,:)
          A4 = RHOV
          B4 = DerRHOV(I,:)
          DSTOKES_MAG(I,:,1)=S1*((1D0/DET_MAT)*(B1*(A1**2D0+A2**2D0+A3**2D0+&
               A4**2D0)+2D0*A1*(A1*B1+A2*B2+A3*B3+A4*B4))- &
               (DDMAT(I,:)/DET_MAT**2D0)*A1*(A1**2D0+A2**2D0+A3**2D0+A4**2D0))
          !print*,minval(dstokesm(i,:,1)),maxval(dstokesm(i,:,1))
       ENDDO
       ! Derivatives of Stokes Q, U, V with respect to
       ! 7 regular free parameters.
       DO K=2,4
          DO I=1,7
             A1 = ETAI
             B1 = DerETAI(I,:)
             A2 = EXTRA
             B2 = DEXTRA(I,:)
             SELECT CASE (K)
             CASE(2)
                C1=ETAQ
                D1=DerETAQ(I,:)
                C2=ETAV
                D2=DerETAV(I,:)
                C3=RHOU
                D3=DerRHOU(I,:)
                C4=ETAU
                D4=DerETAU(I,:)
                C5=RHOV
                D5=DerRHOV(I,:)
                C6=RHOQ
                D6=DerRHOQ(I,:)
             CASE(3)
                C1=ETAU
                D1=DerETAU(I,:)
                C2=ETAQ
                D2=DerETAQ(I,:)
                C3=RHOV
                D3=DerRHOV(I,:)
                C4=ETAV
                D4=DerETAV(I,:)
                C5=RHOQ
                D5=DerRHOQ(I,:)
                C6=RHOU
                D6=DerRHOU(I,:)
             CASE(4)
                C1=ETAV
                D1=DerETAV(I,:)
                C2=ETAU
                D2=DerETAU(I,:)
                C3=RHOQ
                D3=DerRHOQ(I,:)
                C4=ETAQ
                D4=DerETAQ(I,:)
                C5=RHOU
                D5=DerRHOU(I,:)
                C6=RHOV
                D6=DerRHOV(I,:)
             END SELECT
             PART1 = (1D0/DET_MAT)*(2D0*A1*B1*C1+D1*A1**2D0+&
                  B1*(C2*C3-C4*C5)+A1*(C2*D3+D2*C3-D4*C5-C4*D5)+ &
                  D6*A2+C6*B2)
             PART2 = (DDMAT(I,:)/DET_MAT**2D0)*(A1**2D0*C1+A1* &
                  (C2*C3-C4*C5)+C6*EXTRA)
             DSTOKES_MAG(I,:,K) = S1*(PART2-PART1)
          ENDDO
       ENDDO
       ! Derivatives of I, Q, U, V with respect to S0 and S1
       DSTOKES_MAG(8,:,1) = 1D0
       DSTOKES_MAG(8,:,2:4) = 0D0
       DSTOKES_MAG(9,:,1) = (STOKES_MAG(:,1)-S0)/S1
       DSTOKES_MAG(9,:,2:4) = STOKES_MAG(:,2:4)/S1


       ! We now filter the derivatives with the instruments filter profiles.      
       
       DO I = 1, 9
          DO J = 1, Nfilt
             DO k = 1, 4
                DSYN(I, J, K) = ALPHAM * SUM(FILTERS(:,J) * DSTOKES_MAG(I,:,K)) 
             ENDDO
          ENDDO
       ENDDO

       ! Derivative with respect to the filling factor
       DSYN(10,:,:) = STOKES_FILT - SCAT
       
       
    ENDIF
    

  END SUBROUTINE SYNTHESIS
  !!
  !! SOUBROUTINE ABSMAT
  !!
  SUBROUTINE ABSMAT(MODEL, DERIVATIVE, ETAI, ETAQ, ETAU, ETAV, RHOQ, RHOU, RHOV, &
       DerETAI, DerETAQ, DerETAU, DerETAV, DerRHOQ, DerRHOU, DerRHOV)
    !
    ! J M Borrero
    ! Jan 7, 2007
    ! HAO-NCAR for HMI-Stanford
    !
    USE CONS_PARAM
    USE LINE_PARAM
    USE ZEEMAN
    IMPLICIT NONE
    !----------------------------------------------------------
    REAL(DP), INTENT(IN), DIMENSION(10)     :: MODEL
    LOGICAL, INTENT(IN)                     :: DERIVATIVE
    !----------------------------------------------------------
    REAL(DP),    DIMENSION(NUMW)   :: FRECR, FRECP, FRECB
    REAL(DP),    DIMENSION(NUMW)   :: PHIR, PHIP, PHIB, PSIR, PSIP, PSIB
    REAL(DP),    DIMENSION(NUMW)   :: DerPHIR_DerDAM, DerPHIR_DerFRECR
    REAL(DP),    DIMENSION(NUMW)   :: DerPSIR_DerDAM, DerPSIR_DerFRECR
    REAL(DP),    DIMENSION(NUMW)   :: DerPHIB_DerDAM, DerPHIB_DerFRECB
    REAL(DP),    DIMENSION(NUMW)   :: DerPSIB_DerDAM, DerPSIB_DerFRECB
    REAL(DP),    DIMENSION(NUMW)   :: DerPHIP_DerDAM, DerPHIP_DerFRECP
    REAL(DP),    DIMENSION(NUMW)   :: DerPSIP_DerDAM, DerPSIP_DerFRECP
    REAL(DP),    DIMENSION(NUMW)   :: DerPHIP_DerB, DerPSIP_DerB
    REAL(DP),    DIMENSION(NUMW)   :: DerPHIP_DerDLDOP, DerPSIP_DerDLDOP
    REAL(DP),    DIMENSION(NUMW)   :: DerPHIP_DerVLOS, DerPSIP_DerVLOS
    REAL(DP),    DIMENSION(NUMW)   :: DerPHIB_DerB, DerPSIB_DerB
    REAL(DP),    DIMENSION(NUMW)   :: DerPHIB_DerDLDOP, DerPSIB_DerDLDOP
    REAL(DP),    DIMENSION(NUMW)   :: DerPHIB_DerVLOS, DerPSIB_DerVLOS
    REAL(DP),    DIMENSION(NUMW)   :: DerPHIR_DerB, DerPSIR_DerB
    REAL(DP),    DIMENSION(NUMW)   :: DerPHIR_DerDLDOP, DerPSIR_DerDLDOP
    REAL(DP),    DIMENSION(NUMW)   :: DerPHIR_DerVLOS, DerPSIR_DerVLOS
    REAL(DP),    DIMENSION(NUMW)   :: PHIR_aux, PHIP_aux, PHIB_aux
    REAL(DP),    DIMENSION(NUMW)   :: PSIR_aux, PSIP_aux, PSIB_aux

    REAL(DP),    DIMENSION(NUMW)   :: DerFRECR_DerVLOS, DerFRECR_DerDLDOP, DerFRECR_DerB
    REAL(DP),    DIMENSION(NUMW)   :: DerFRECB_DerVLOS, DerFRECB_DerDLDOP, DerFRECB_DerB
    REAL(DP),    DIMENSION(NUMW)   :: DerFRECP_DerVLOS, DerFRECP_DerDLDOP, DerFRECP_DerB
    !
    REAL(DP),    DIMENSION(NUMW)   :: ETAI, ETAQ, ETAU, ETAV, RHOQ, RHOU, RHOV
    REAL(DP),    DIMENSION(7,NUMW) :: DerETAI, DerETAQ, DerETAU, DerETAV
    REAL(DP),    DIMENSION(7,NUMW) :: DerRHOQ, DerRHOU, DerRHOV
    !
    REAL(DP)                     :: VLOS, DLDOP, BFIELD, DAM, ETA0, GAM, PHI
    REAL                         :: coef
    REAL(DP)                     :: SIN2INC, COS2AZI, SIN2AZI, SININC, COSINC, SINCOSINC
    REAL(DP),    DIMENSION(NUMW) :: ABSOR1, ABSOR3, DISPE1, DISPE3
    INTEGER                      :: m1, m2, m3, index1, index2
    !
    FRECR(:)=0D0
    FRECP(:)=0D0
    FRECB(:)=0D0
    PHIR(:)=0D0
    PHIP(:)=0D0
    PHIB(:)=0D0
    PSIR(:)=0D0
    PSIP(:)=0D0
    PSIB(:)=0D0
    ETAI(:)=0D0
    ETAQ(:)=0D0
    ETAU(:)=0D0
    ETAV(:)=0D0
    RHOQ(:)=0D0
    RHOU(:)=0D0
    RHOV(:)=0D0
    DerETAI(:,:)=0D0
    DerETAQ(:,:)=0D0
    DerETAU(:,:)=0D0
    DerETAV(:,:)=0D0
    DerRHOQ(:,:)=0D0
    DerRHOU(:,:)=0D0
    DerRHOV(:,:)=0D0
    DerPHIR_DerDAM(:)=0D0
    DerPHIR_DerFRECR(:)=0D0
    DerPSIR_DerDAM(:)=0D0 
    DerPSIR_DerFRECR(:)=0D0
    DerPHIB_DerDAM(:)=0D0
    DerPHIB_DerFRECB(:)=0D0
    DerPSIB_DerDAM(:)=0D0
    DerPSIB_DerFRECB(:)=0D0
    DerPHIP_DerDAM(:)=0D0
    DerPHIP_DerFRECP(:)=0D0
    DerPSIP_DerDAM(:)=0D0
    DerPSIP_DerFRECP(:)=0D0
    DerFRECR_DerVLOS(:)=0D0
    DerFRECR_DerDLDOP(:)=0D0
    DerFRECR_DerB(:)=0D0
    DerFRECB_DerVLOS(:)=0D0 
    DerFRECB_DerDLDOP(:)=0D0 
    DerFRECB_DerB(:)=0D0
    DerFRECP_DerVLOS(:)=0D0
    DerFRECP_DerDLDOP(:)=0D0
    DerFRECP_DerB(:)=0D0
    ! By RCE: definitions of another few derivatives
    DerPHIP_DerB(:)=0D0
    DerPSIP_DerB(:)=0D0
    DerPHIP_DerDLDOP(:)=0D0
    DerPSIP_DerDLDOP(:)=0D0
    DerPHIP_DerVLOS(:)=0D0
    DerPSIP_DerVLOS(:)=0D0
    DerPHIB_DerB(:)=0D0
    DerPSIB_DerB(:)=0D0
    DerPHIB_DerDLDOP(:)=0D0
    DerPSIB_DerDLDOP(:)=0D0
    DerPHIB_DerVLOS(:)=0D0
    DerPSIB_DerVLOS(:)=0D0
    DerPHIR_DerB(:)=0D0 
    DerPSIR_DerB(:)=0D0
    DerPHIR_DerDLDOP(:)=0D0
    DerPSIR_DerDLDOP(:)=0D0
    DerPHIR_DerVLOS(:)=0D0 
    DerPSIR_DerVLOS(:)=0D0

    VLOS=0D0
    DLDOP=0D0
    BFIELD=0D0
    DAM=0D0
    ETA0=0D0
    GAM=0D0
    PHI=0D0
    ABSOR1(:)=0D0
    ABSOR3(:)=0D0
    DISPE1(:)=0D0
    DISPE3(:)=0D0
    
    ! Model parameters: magnetic component
    ETA0=MODEL(1)
    GAM=MODEL(2)*D2R
    PHI=MODEL(3)*D2R
    DAM=MODEL(4)
    DLDOP=MODEL(5)
    BFIELD=MODEL(6)
    VLOS=MODEL(7)

! --- By RCE: Here is where we have to take into account that the PI, sigmaR and sigmaB 
! components are composed of individual sub-components if the line is not a real triplet.
! There should be a double loop over the m1 and m2 quantum numbers and a call to the 
! routine threej.f90 in order to determine the weight of each sub-component.


DO index1 = -j1, j1, 1
   DO index2 = -j2, j2, 1

! Debate whether to use voigt_taylor routine or not, to do the voigt profile calculation
! Check whether coef is properly normalized.
      m1 = index1
      m2 = index2
      m3 = m2 - m1
      CALL threej(j1, j2, 1, m1, -m2, m3, coef)
      coef = 3D0 * coef**2D0
      SHIFT =  ((g1*m1 - g2*m2) * 467d-15 *LANDA0**2)*1D3

      IF (m3 .EQ. 0) THEN

         FRECP(:) = (WAVE(:)-1.D3*VLOS*LANDA0/LIGHT+BFIELD*SHIFT)/DLDOP

         IF (coef .NE. 0) THEN ! Everything is multiplied by the 3j-coefficient!
            CALL VOIGT(NUMW, DAM, FRECP, PHIP_AUX, PSIP_AUX)
            PHIP = PHIP + coef * PHIP_AUX
            PSIP = PSIP + coef * PSIP_AUX
            ! derivatives should be calculated here, 
            ! because we have already calculated the aux variables 
            IF (DERIVATIVE.EQV..TRUE.) THEN
               DerPHIP_DerFRECP = coef * (2D0*DAM*PSIP_AUX-2D0*FRECP*PHIP_AUX)
               DerPSIP_DerFRECP = coef * (2D0/DSQRT(DPI) &
                    - 2D0*(DAM*PHIP_AUX+FRECP*PSIP_AUX))
               
               DerPHIP_DerDAM = DerPHIP_DerDAM - DerPSIP_DerFRECP
               DerPSIP_DerDAM = DerPSIP_DerDAM + DerPHIP_DerFRECP
               
               ! Derivatives of the frecuency with respect to
               ! the field strength, LOS velocity, Doppler width.
               DerFRECP_DerB(:) = SHIFT/DLDOP
               DerFRECP_DerDLDOP =  - FRECP/DLDOP
               DerFRECP_DerVLOS = -1D3*LANDA0/(LIGHT*DLDOP)  
               
               ! Derivatives of the PHI and PSI with respect to the
               ! remaining atmospheric parameters
               DerPHIP_DerB = DerPHIP_DerB + DerPHIP_DerFRECP * DerFRECP_DerB
               DerPSIP_DerB = DerPSIP_DerB + DerPSIP_DerFRECP * DerFRECP_DerB
               DerPHIP_DerDLDOP = DerPHIP_DerDLDOP + DerPHIP_DerFRECP * DerFRECP_DerDLDOP
               DerPSIP_DerDLDOP = DerPSIP_DerDLDOP + DerPSIP_DerFRECP * DerFRECP_DerDLDOP
               DerPHIP_DerVLOS = DerPHIP_DerVLOS + DerPHIP_DerFRECP * DerFRECP_DerVLOS
               DerPSIP_DerVLOS = DerPSIP_DerVLOS + DerPSIP_DerFRECP * DerFRECP_DerVLOS
               
            ENDIF ! Calculate derivatives
         ENDIF ! 3j-coef NE 0
      ENDIF ! m3 EQ 0

      IF (m3 .EQ. 1) THEN
         FRECR(:) = (WAVE(:)-1.D3*VLOS*LANDA0/LIGHT+BFIELD*SHIFT)/DLDOP
         IF (coef .NE. 0D0) THEN 
            CALL VOIGT(NUMW, DAM, FRECR, PHIR_AUX, PSIR_AUX)
            PHIR = PHIR + coef * PHIR_AUX
            PSIR = PSIR + coef * PSIR_AUX
            
            IF (DERIVATIVE.EQV..TRUE.) THEN

               DerPHIR_DerFRECR = coef * 2D0 * (DAM*PSIR_AUX - FRECR*PHIR_AUX)
               DerPSIR_DerFRECR = coef * (2D0/DSQRT(DPI) &
                    - 2D0*(DAM*PHIR_AUX+FRECR*PSIR_AUX))
               
               DerPHIR_DerDAM = DerPHIR_DerDAM - DerPSIR_DerFRECR
               DerPSIR_DerDAM = DerPSIR_DerDAM + DerPHIR_DerFRECR
               
               ! Derivatives of the frecuency with respect to
               ! the field strength, LOS velocity, Doppler width.
               DerFRECR_DerB(:) = SHIFT/DLDOP
               DerFRECR_DerDLDOP =  - FRECR/DLDOP
               DerFRECR_DerVLOS = -1D3*LANDA0/(LIGHT*DLDOP)  
               
               ! Derivatives of the PHI and PSI with respect to the
               ! remaining atmospheric parameters
               DerPHIR_DerB = DerPHIR_DerB + DerPHIR_DerFRECR * DerFRECR_DerB
               DerPSIR_DerB = DerPSIR_DerB + DerPSIR_DerFRECR * DerFRECR_DerB
               DerPHIR_DerDLDOP = DerPHIR_DerDLDOP + DerPHIR_DerFRECR * DerFRECR_DerDLDOP
               DerPSIR_DerDLDOP = DerPSIR_DerDLDOP + DerPSIR_DerFRECR * DerFRECR_DerDLDOP
               DerPHIR_DerVLOS = DerPHIR_DerVLOS + DerPHIR_DerFRECR * DerFRECR_DerVLOS
               DerPSIR_DerVLOS = DerPSIR_DerVLOS + DerPSIR_DerFRECR * DerFRECR_DerVLOS
               
            ENDIF ! Calculate derivatives
         ENDIF ! 3j-coef NE 0
      ENDIF ! m3 = 1


      IF (m3 .EQ. -1) THEN

         FRECB(:) = (WAVE(:)-1.D3*VLOS*LANDA0/LIGHT+BFIELD*SHIFT)/DLDOP
         IF (coef .NE. 0) THEN 
            CALL VOIGT(NUMW, DAM, FRECB, PHIB_AUX, PSIB_AUX)
            PHIB = PHIB + coef * PHIB_AUX
            PSIB = PSIB + coef * PSIB_AUX
         
            IF (DERIVATIVE.EQV..TRUE.) THEN
               
               DerPHIB_DerFRECB = coef * 2D0 * (DAM*PSIB_AUX - FRECB*PHIB_AUX)
               DerPSIB_DerFRECB = coef * (2D0/DSQRT(DPI) &
                    - 2D0*(DAM*PHIB_AUX+FRECB*PSIB_AUX))
               
               DerPHIB_DerDAM = DerPHIB_DerDAM - DerPSIB_DerFRECB
               DerPSIB_DerDAM = DerPSIB_DerDAM + DerPHIB_DerFRECB
               
               ! Derivatives of the frecuency with respect to
               ! the field strength, LOS velocity, Doppler width.
               DerFRECB_DerB(:) = SHIFT/DLDOP
               DerFRECB_DerDLDOP =  - FRECB/DLDOP
               DerFRECB_DerVLOS = -1D3*LANDA0/(LIGHT*DLDOP)  
               
               ! Derivatives of the PHI and PSI with respect to the
               ! remaining atmospheric parameters
               DerPHIB_DerB = DerPHIB_DerB + DerPHIB_DerFRECB * DerFRECB_DerB
               DerPSIB_DerB = DerPSIB_DerB + DerPSIB_DerFRECB * DerFRECB_DerB
               DerPHIB_DerDLDOP = DerPHIB_DerDLDOP + DerPHIB_DerFRECB * DerFRECB_DerDLDOP
               DerPSIB_DerDLDOP = DerPSIB_DerDLDOP + DerPSIB_DerFRECB * DerFRECB_DerDLDOP
               DerPHIB_DerVLOS = DerPHIB_DerVLOS + DerPHIB_DerFRECB * DerFRECB_DerVLOS
               DerPSIB_DerVLOS = DerPSIB_DerVLOS + DerPSIB_DerFRECB * DerFRECB_DerVLOS
            ENDIF  ! calculate derivatives
         ENDIF  ! 3j-coef NE 0
      ENDIF  ! m3 EQ -1
      
   ENDDO !index1 - m1 quantum number
ENDDO !index2 - m2 quantum number

!--- By RCE: end of the loops for the magnetic quantum numbers

    ! Common parts
    SIN2INC=DSIN(GAM)**2D0
    COS2AZI=DCOS(2D0*PHI)
    SIN2AZI=DSIN(2D0*PHI)
    SINCOSINC=DSIN(GAM)*DCOS(GAM)
    SININC=DSIN(GAM)
    COSINC=DCOS(GAM)
    ABSOR1=PHIP-0.5D0*(PHIB+PHIR)
    DISPE1=PSIP-0.5D0*(PSIB+PSIR)
    ABSOR3=PHIR-PHIB
    DISPE3=PSIR-PSIB
    !
    ! ETAI:
    !
    ETAI = 1D0+(ETA0/2D0)*(PHIP*SIN2INC+0.5D0*(PHIR+PHIB)*(2D0-SIN2INC))
    !
    ! ETAQ:
    !
    ETAQ = 0.5D0*ETA0*ABSOR1*SIN2INC*COS2AZI
    !
    ! ETAU:
    !
    ETAU = 0.5D0*ETA0*ABSOR1*SIN2INC*SIN2AZI
    !
    ! ETAV:
    !
    COSINC=DCOS(GAM)
    ETAV = -0.5D0*ETA0*ABSOR3*COSINC
    !
    ! RHOQ:
    !
    RHOQ = 0.5D0*ETA0*DISPE1*SIN2INC*COS2AZI
    !
    ! RHOU:
    !
    RHOU = 0.5D0*ETA0*DISPE1*SIN2INC*SIN2AZI
    !
    ! RHOV:
    !
    RHOV = -0.5D0*ETA0*DISPE3*COSINC
    
    IF (DERIVATIVE.EQV..TRUE.) THEN
    
       ! ETAI derivatives:
       !
       DerETAI(1,:) = (ETAI(:)-1D0)/ETA0
       DerETAI(2,:) = ETA0*SINCOSINC*ABSOR1*D2R
       DerETAI(3,:) = 0D0
       DerETAI(4,:) = 0.5D0*ETA0*(DerPHIP_DerDAM*SIN2INC+0.5D0*(DerPHIB_DerDAM+ &
            DerPHIR_DerDAM)*(2D0-SIN2INC))
       DerETAI(5,:) = 0.5D0*ETA0*(DerPHIP_DerDLDOP*SIN2INC+ &
            0.5D0*(DerPHIB_DerDLDOP+DerPHIR_DerDLDOP)*(2D0-SIN2INC))
       DerETAI(6,:) = 0.25D0*ETA0*(DerPHIB_DerB+DerPHIR_DerB)*(2D0-SIN2INC)
       DerETAI(7,:) = 0.5D0*ETA0*(DerPHIP_DerVLOS*SIN2INC+ &
            0.5D0*(DerPHIB_DerVLOS+DerPHIR_DerVLOS)*(2D0-SIN2INC))

       !
       ! ETA Q derivatives
       !
       DerETAQ(1,:) = ETAQ/ETA0
       DerETAQ(2,:) = SINCOSINC*COS2AZI*ETA0*ABSOR1*D2R
       DerETAQ(3,:) = -ETA0*ABSOR1*SIN2INC*SIN2AZI*D2R
       DerETAQ(4,:) = 0.5D0*ETA0*(DerPHIP_DerDAM-0.5D0*(DerPHIB_DerDAM+ &
            DerPHIR_DerDAM))*SIN2INC*COS2AZI
       DerETAQ(5,:) = 0.5D0*ETA0*(DerPHIP_DerDLDOP- &
            0.5D0*(DerPHIB_DerDLDOP+DerPHIR_DerDLDOP))*SIN2INC*COS2AZI
       DerETAQ(6,:) = -0.25D0*ETA0*(DerPHIB_DerB+ DerPHIR_DerB)*SIN2INC*COS2AZI
       DerETAQ(7,:) = 0.5D0*ETA0*(DerPHIP_DerVLOS- &
            0.5D0*(DerPHIB_DerVLOS+DerPHIR_DerVLOS))*SIN2INC*COS2AZI

       !
       ! ETAU derivatives
       !
       DerETAU(1,:) = ETAU/ETA0
       DerETAU(2,:) = SINCOSINC*SIN2AZI*ETA0*ABSOR1*D2R
       DerETAU(3,:) = ETA0*ABSOR1*SIN2INC*COS2AZI*D2R
       DerETAU(4,:) = 0.5D0*ETA0*(DerPHIP_DerDAM-0.5D0*(DerPHIB_DerDAM+ &
            DerPHIR_DerDAM))*SIN2INC*SIN2AZI
       DerETAU(5,:) = 0.5D0*ETA0*(DerPHIP_DerDLDOP- &
            0.5D0*(DerPHIB_DerDLDOP+DerPHIR_DerDLDOP))*SIN2INC*SIN2AZI
       DerETAU(6,:) = -0.25D0*ETA0*(DerPHIB_DerB+ &
            DerPHIR_DerB)*SIN2INC*SIN2AZI
       DerETAU(7,:) = 0.5D0*ETA0*(DerPHIP_DerVLOS- &
            0.5D0*(DerPHIB_DerVLOS+DerPHIR_DerVLOS))*SIN2INC*SIN2AZI

       !
       ! ETAV derivatives
       !
       DerETAV(1,:) = ETAV/ETA0
       DerETAV(2,:) = 0.5D0*ETA0*ABSOR3*SININC*D2R
       DerETAV(3,:) = 0D0
       DerETAV(4,:) = -0.5D0*ETA0*(DerPHIR_DerDAM-DerPHIB_DerDAM)*COSINC
       DerETAV(5,:) = -0.5D0*ETA0*(DerPHIR_DerDLDOP - DerPHIB_DerDLDOP)*COSINC
       DerETAV(6,:) = -0.5D0*ETA0*(DerPHIR_DerB -  DerPHIB_DerB)*COSINC
       DerETAV(7,:) = -0.5D0*ETA0*(DerPHIR_DerVLOS - DerPHIB_DerVLOS)*COSINC
   
       !
       ! RHOQ derivatives:
       !
       DerRHOQ(1,:) = RHOQ/ETA0
       DerRHOQ(2,:) = SINCOSINC*COS2AZI*ETA0*DISPE1*D2R
       DerRHOQ(3,:) = -ETA0*DISPE1*SIN2INC*SIN2AZI*D2R

!By RCE: found bug in one of the derivatives: a DerPHIR_DerDAM should be a DerPSIR_DerDAM
!       DerRHOQ(4,:) = 0.5D0*ETA0*(DerPSIP_DerDAM-0.5D0*(DerPSIB_DerDAM+ &
!            DerPHIR_DerDAM))* SIN2INC*COS2AZI
       DerRHOQ(4,:) = 0.5D0*ETA0*(DerPSIP_DerDAM-0.5D0*(DerPSIB_DerDAM+ &
            DerPSIR_DerDAM))* SIN2INC*COS2AZI
       DerRHOQ(5,:) = 0.5D0*ETA0*(DerPSIP_DerDLDOP- &
            0.5D0*(DerPSIB_DerDLDOP+DerPSIR_DerDLDOP))*SIN2INC*COS2AZI
       DerRHOQ(6,:) = -0.25D0*ETA0*(DerPSIB_DerB+ &
            DerPSIR_DerB)*SIN2INC*COS2AZI
       DerRHOQ(7,:) = 0.5D0*ETA0*(DerPSIP_DerVLOS- &
            0.5D0*(DerPSIB_DerVLOS+DerPSIR_DerVLOS))*SIN2INC*COS2AZI
       !
       ! RHOU derivatives
       !
       
       DerRHOU(1,:) = RHOU/ETA0
       DerRHOU(2,:) = SINCOSINC*SIN2AZI*ETA0*DISPE1*D2R
       DerRHOU(3,:) = ETA0*DISPE1*SIN2INC*COS2AZI*D2R
       DerRHOU(4,:) = 0.5D0*ETA0*(DerPSIP_DerDAM-0.5D0*(DerPSIB_DerDAM+ &
            DerPSIR_DerDAM))*SIN2INC*SIN2AZI
       DerRHOU(5,:) = 0.5D0*ETA0*(DerPSIP_DerDLDOP- &
            0.5D0*(DerPSIB_DerDLDOP+DerPSIR_DerDLDOP))*SIN2INC*SIN2AZI
       DerRHOU(6,:) = -0.25D0*ETA0*(DerPSIB_DerB + DerPSIR_DerB)*SIN2INC*SIN2AZI
       DerRHOU(7,:) = 0.5D0*ETA0*(DerPSIP_DerVLOS- &
            0.5D0*(DerPSIB_DerVLOS+DerPSIR_DerVLOS))*SIN2INC*SIN2AZI
       !
       ! RHOV derivatives
       !
       RHOV = -0.5D0*ETA0*DISPE3*COSINC
       DerRHOV(1,:) = RHOV/ETA0
       DerRHOV(2,:) = 0.5D0*ETA0*DISPE3*SININC*D2R
       DerRHOV(3,:) = 0D0
       DerRHOV(4,:) = -0.5D0*ETA0*(DerPSIR_DerDAM-DerPSIB_DerDAM)*COSINC
       DerRHOV(5,:) = -0.5D0*ETA0*(DerPSIR_DerDLDOP - DerPSIB_DerDLDOP)*COSINC
       DerRHOV(6,:) = -0.5D0*ETA0*(DerPSIR_DerB- DerPSIB_DerB)*COSINC
       DerRHOV(7,:) = -0.5D0*ETA0*(DerPSIR_DerVLOS-DerPSIB_DerVLOS)*COSINC
    ENDIF
    

  ENDSUBROUTINE ABSMAT

  
END MODULE FORWARD

