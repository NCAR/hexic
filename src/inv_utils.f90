MODULE INV_UTILS
  !
  ! J M Borrero
  ! Dec 14, 2009
  ! HAO-NCAR for HMI-Stanford
  !
  ! Massive changes!!!

  ! By RCE (June 8, 2010): in  SUBROUTINE SVDSOL(DMODEL, CONV_FLAG) added 
  ! conv_flag to flag the convergence of the SVD solving routine. If SVD 
  ! doesn't converge, then CONV_FLAG takes value 0. Changes propagate into SVDCMP.f90. 
  ! This flag is an indication for INVERT to set the results of the inversions 
  ! to NaN for a particular pixel.
  !
  ! By RCE, Jan 2011: fixed a bug in the GET_ERROR routine.

CONTAINS

  
  !----------------------------------------------------
   SUBROUTINE GET_CHI2(MODEL,SYN,OBS,C2)
    !
    USE INV_PARAM
    USE CONS_PARAM
    USE LINE_PARAM

    IMPLICIT NONE

    REAL(DP), INTENT(IN),   DIMENSION(NFILT,4)  :: OBS, SYN
    REAL(DP), INTENT(IN),   DIMENSION(10)      :: MODEL
    REAL(DP), INTENT(OUT)                      :: C2
    REAL(DP), DIMENSION(1)                     :: DUM
    INTEGER                                    :: I
    
    C2 = 0D0

    DO I=1,4    
      C2 = C2+(1D0/NUMFREE_DEG)*((WEIGHTS(I)/NOISE(I))**2D0)*SUM(((OBS(:,I)-SYN(:,I)))**2D0)
!      C2 = C2+(WEIGHTS(I)**2D0)*SUM(((OBS(:,I)-SYN(:,I)))**2D0)

    ENDDO

  END SUBROUTINE GET_CHI2

  !------------------------------------------------------

  SUBROUTINE NORMALIZE_DSYN(DSYN)
    ! 
    ! By RCE, May 20, 2011: We use the NORM vector initialized in 
    ! LIM_INIT
    !
    USE CONS_PARAM
    USE INV_PARAM
    USE LINE_PARAM

    IMPLICIT NONE
    REAL(DP), INTENT(INOUT),     DIMENSION(10,NFILT,4)  ::  DSYN
    INTEGER                                            ::  I

    DO I=1,10
       DSYN(I,:,:) = DSYN(I,:,:) * NORM(I)
    ENDDO
  END SUBROUTINE NORMALIZE_DSYN

  !-------------------------------------------------------

  PURE SUBROUTINE ZERO_DSYN (DSYN)
    USE CONS_PARAM
    USE INV_PARAM
    USE LINE_PARAM

    IMPLICIT NONE

    REAL(DP),  INTENT(INOUT),  DIMENSION(10,NFILT,4)   :: DSYN
    INTEGER                                           :: I
    DO I=1,10
       IF (FREE(I).EQV..FALSE.) THEN 
          DSYN(I,:,:) = 0D0
       ENDIF
    ENDDO
    !
  END SUBROUTINE ZERO_DSYN
  !
  !--------------------------------------------------------
  !
  PURE SUBROUTINE GET_LAMBDA(DELTACHI2, LAMBDA_OLD, LAMBDA_NEW)
    !
    USE CONS_PARAM
    USE INV_PARAM

    IMPLICIT NONE

    REAL(DP), INTENT(IN)           :: DELTACHI2,LAMBDA_OLD
    REAL(DP), INTENT(OUT)          :: LAMBDA_NEW

    IF (DELTACHI2 .GE. DELTACHIMIN) THEN ! Things got better
       LAMBDA_NEW = LAMBDA_OLD/LAMBDA_DOWN
       LAMBDA_NEW = MAX(LAMBDA_NEW,LAMBDA_MIN)
    ELSE ! Things got worse
       LAMBDA_NEW = LAMBDA_UP*LAMBDA_OLD
       IF (LAMBDA_OLD .GT. 0.01) LAMBDA_NEW = 2D0*LAMBDA_NEW
    ENDIF
  END SUBROUTINE GET_LAMBDA
  !
  !--------------------------------------------------------
  !
  ! Routine that computes the divergence of chi2
  ! The divergence is -1/2*partial(chi2)/partial(parameter)
  ! So although the derivative has a negative sign, the 
  ! divergence doesn't
  ! JS: Yet, the factor 1/2 appears not to have been applied below.
  ! Similarly the Hessian does not have a factor of 1/2, so math
  ! appears consistent, but comments not.
  
  SUBROUTINE GET_DIVC(SYN, OBS, DSYN, DIVC)
    !
    USE CONS_PARAM
    USE INV_PARAM
    USE LINE_PARAM

    IMPLICIT NONE

    REAL(DP), INTENT(IN), DIMENSION(NFILT,4)           :: OBS, SYN
    REAL(DP), INTENT(IN), DIMENSION(10,NFILT,4)        :: DSYN
    REAL(DP), INTENT(INOUT), DIMENSION(NUMFREE_PARAM) :: DIVC
    INTEGER                                           :: I, J
    !
    DIVC(:) = 0.0D0

    DO I=1,NUMFREE_PARAM
       DO J=1,4
          DIVC(I) = DIVC(I)+(2D0/NUMFREE_DEG)*(WEIGHTS(J)**2D0) &
               *SUM((OBS(:,J)-SYN(:,J)) * DSYN(FREELOC(I),:,J))
       ENDDO
    ENDDO

  END SUBROUTINE GET_DIVC
  !
  !--------------------------------------------------------
  !
  SUBROUTINE RANDOM_MODEL_JUMP(MODELR)
    USE CONS_PARAM
    USE INV_PARAM
    USE RAN_MOD

    IMPLICIT NONE

    REAL(DP), INTENT(INOUT),    DIMENSION(10) :: MODELR
    REAL(DP), DIMENSION(10)                   :: INMODEL
    REAL(DP),                   DIMENSION(10) :: RAN
    INTEGER                                   :: I
    REAL(DP)                                  :: SSUM,SRATIO
    !--------------------------------------------
    ! JM Borrero: Apr 15, 2010
    ! Modified to include and normal distribution
    ! instead of uniform distribution
    !--------------------------------------------

    INMODEL = MODELR ! Save input model

    MODELR(1) = NORMAL(5D0,5D0)
    MODELR(2) = 90D0 + 10D0*NORMAL(0D0,1D0)
    MODELR(3) = inmodel(3)+NORMAL(0D0,45D0)
    MODELR(5) = NORMAL(25D0,10D0)
    MODELR(6) = INMODEL(6) * NORMAL(1D0,0.5D0)
    ssum = (INMODEL(8)+INMODEL(9))*NORMAL(1D0,0.01D0)
    sratio = 0.2D0+0.3D0*ran1()
    MODELR(8) = ssum*sratio/(1+sratio)
    MODELR(9) = ssum/(1+sratio)

    ! Checking that perturbation did not go too far
    CALL FINE_TUNE_MODEL(MODELR)

  END SUBROUTINE RANDOM_MODEL_JUMP
  !
  !--------------------------------------------------------
  !
   SUBROUTINE FINE_TUNE_MODEL(MODEL)
    USE INV_PARAM
    USE CONS_PARAM

    IMPLICIT NONE

    REAL(DP), INTENT(INOUT), DIMENSION(10)    :: MODEL
    INTEGER                                   :: REV, I
    REAL(DP)                                  :: S0,S1,SS

  ! This routine checks that no variable goes wildly out of range.
  ! We use the variable limits defined in INV_INIT (called from wrapper)

  ! Save in/out values for the source function:
    S0 = MODEL(8)
    S1 = MODEL(9)
    SS = S0+S1

    MODEL(8) = MAX(S0,LOWER_LIMIT(8))
    MODEL(9) = SS-MODEL(8) ! Keep sum unchanged

    ! Check all the limits except the angles and non-free parameters
    DO I = 1, 10
      IF ((FREE(I).EQV..TRUE.) .AND. (I .NE. 2) .AND. (I .NE. 3)) THEN
        IF (MODEL(I) .LT. LOWER_LIMIT(I)) MODEL(I) = LOWER_LIMIT(I)
        IF (MODEL(I) .GT. UPPER_LIMIT(I)) MODEL(I) = UPPER_LIMIT(I)
      ENDIF
    ENDDO

    MODEL(8) = MAX(S0,0.15D0*SS) ! Use S0+S1 instead of ICONT
    MODEL(9) = SS-MODEL(8) ! Keep sum unchanged
    

    ! Check the angles independently so that they vary between 0 and 180.
 
    ! All negatives Inclination treated here to be positive and between 0,PI
    IF (MODEL(2).LT.-360D0) THEN
       REV=ABS((INT(MODEL(2)/360D0)))
       MODEL(2)=MODEL(2)+360D0*REV
    ENDIF
    IF (MODEL(2).LT.0D0 .AND. MODEL(2).GT.-90D0) MODEL(2)=-MODEL(2)
    IF (MODEL(2).LT.-90D0 .AND. MODEL(2).GT.-180D0) MODEL(2)=-MODEL(2)
    IF (MODEL(2).LT.-180 .AND. MODEL(2).GT.-270D0) MODEL(2)=360D0+MODEL(2)
    IF (MODEL(2).LT.-270D0 .AND. MODEL(2).GT. -360D0) MODEL(2)=360D0+MODEL(2)
    ! All positive inclination larger than PI or 2PI put between 0,PI
    IF (MODEL(2).GT.360D0) THEN
       REV=INT(MODEL(2)/360D0)
       MODEL(2)=MODEL(2)-360D0*REV
    ENDIF
    IF (MODEL(2).GT.180) MODEL(2)=360D0-MODEL(2)

    ! Shift azimuthal angles
    IF (MODEL(3).GT.360D0) THEN
       REV=INT(MODEL(3)/360D0)
       MODEL(3)=MODEL(3)-360D0*REV
    ENDIF
    IF (MODEL(3).GT.180) MODEL(3)=MODEL(3)-180D0
    IF (MODEL(3).LT.-360D0) THEN
       REV=ABS((INT(MODEL(3)/360D0)))
       MODEL(3)=MODEL(3)+360D0*REV
    ENDIF
    IF (MODEL(3).LT.-180) MODEL(3)=MODEL(3)+180D0
    IF (MODEL(3).LT.0) MODEL(3)=MODEL(3)+180D0    
 
  END SUBROUTINE FINE_TUNE_MODEL
  !
  !--------------------------------------------------------
  !
  SUBROUTINE GET_HESS(DSYN,HESS)

  ! NOTE that the Hessian doesn't have the (1+lambda) Marquardt factor
  ! scaling in the diagonal elements of the matrix. This is now included
  ! in the routine that calculates the model improvement (GET_DMODEL).

    USE CONS_PARAM
    USE INV_PARAM
    USE LINE_PARAM

    IMPLICIT NONE

    REAL(DP), INTENT(IN), DIMENSION(10,NFILT,4)                      :: DSYN
    REAL(DP), INTENT(INOUT), DIMENSION(NUMFREE_PARAM,NUMFREE_PARAM) :: HESS
    REAL(DP)                                                        :: HELP
    INTEGER                                                         :: I, J, K
 
    DO I=1,NUMFREE_PARAM
       DO J=1,I
          HELP=0D0
          DO K=1,4
             HELP=HELP+(WEIGHTS(K)**2D0)*SUM(DSYN(FREELOC(I),:,K)*DSYN(FREELOC(J),:,K))
          ENDDO
          HESS(I,J)=(2D0/NUMFREE_DEG)*HELP
          HESS(J,I)=(2D0/NUMFREE_DEG)*HELP
       ENDDO
    ENDDO

  END SUBROUTINE GET_HESS
  !
  !------------------------------------------------------------
  !
  SUBROUTINE GET_DMODEL(MODEL, DIVC, HESS, LAMBDA, DMODEL, CONV_FLAG)
    !
    ! JS: Appears to solve HESS DMODEL = DIVC
    USE INV_PARAM
    USE CONS_PARAM

    IMPLICIT NONE

    REAL(DP), INTENT(IN), DIMENSION(10)                     :: MODEL
    REAL(DP), INTENT(IN)                                    :: LAMBDA
    REAL(DP), DIMENSION(15*NUMFREE_PARAM)                   :: WORK
    REAL(DP),       DIMENSION(NUMFREE_PARAM,NUMFREE_PARAM)  :: VT, U, WINV, HINV, V, HESS1
    REAL(DP),       DIMENSION(NUMFREE_PARAM)                :: PLUSMODEL, W, DIVC1
    REAL(DP), INTENT(INOUT), DIMENSION(NUMFREE_PARAM)       :: DIVC
    REAL(DP), INTENT(INOUT), DIMENSION(NUMFREE_PARAM,NUMFREE_PARAM) :: HESS
    REAL(DP)                                                :: WMAX, DUM, MX
    INTEGER                                                 :: I, J, INFO
    REAL(DP), DIMENSION(10)                                 :: DMODEL
    INTEGER                                                 :: CONV_FLAG

    DMODEL = 0D0 ! In case parameter is not free or SVD fails.

  ! Marquardt (1+ lambda) factor applied to diagonal of Hessian matrix
    DIVC1=DIVC
    DO I = 1, NUMFREE_PARAM
       DO J = 1, NUMFREE_PARAM
          HESS1(j,i) = HESS(j,i)
       ENDDO
       HESS1(i,i) = HESS1(i,i)*(1D0+LAMBDA)
    ENDDO


    ! By RCE: call LAPACK SVD (it used to call a numerical recipes svd routine)
    CALL DGESVD('A','A',NUMFREE_PARAM,NUMFREE_PARAM,HESS1,NUMFREE_PARAM,W,U &
   ,NUMFREE_PARAM,VT,NUMFREE_PARAM,WORK,15*NUMFREE_PARAM,INFO)

    IF (INFO.NE.0) THEN
       PRINT*,'ERROR IN SVD'
       CONV_FLAG = 1
    ELSE
       CONV_FLAG=0
       ! By RCE: transpose VT matrix to obtain V and use it in SVBKSB
       ! DO I = 1, NUMFREE_PARAM
       !   DO J = 1, NUMFREE_PARAM
       !      V(i,j) = VT(j,i)
       !   ENDDO
       ! ENDDO
   
       WMAX=MAXVAL(W)
       DO I=1,NUMFREE_PARAM
          IF (W(I).LT.SVDTOL*WMAX) W(I)=0D0
       ENDDO
   

       ! Calling numerical recipes backsubstitution routine. NEED TO FIND LAPACK ALTERNATIVE.
       ! CALL SVBKSB(U,W,V,NUMFREE_PARAM,NUMFREE_PARAM,NUMFREE_PARAM, &
       !     NUMFREE_PARAM,DIVC1,PLUSMODEL)
   
       CALL SOLVE_SVD(U,W,VT,NUMFREE_PARAM,DIVC1,PLUSMODEL)

       DO I=1,NUMFREE_PARAM
          MX = PLUSMODEL(I)
          IF ((MX.eq.(MX+1D0)).or.(abs(MX).gt.1D10)) then
             CONV_FLAG=2
          ELSE
             DMODEL(FREELOC(I)) = MX
          ENDIF
       ENDDO
    ENDIF

  END SUBROUTINE GET_DMODEL
  !
  !------------------------------------------------------------
  !
 
  PURE SUBROUTINE NORMALIZE_DMODEL(DMODEL)
    !
    ! By RCE, April 2012: normalization vector defined in LIM_INIT
    !
    USE CONS_PARAM
    USE INV_PARAM

    IMPLICIT NONE

    REAL(DP), INTENT(INOUT), DIMENSION(10)         ::  DMODEL
    INTEGER                                        ::  I

    DO I=1,10
       DMODEL(I) = DMODEL(I)*NORM(I)
    ENDDO
    !
  END SUBROUTINE NORMALIZE_DMODEL
  !
  !------------------------------------------------------------
  !
  PURE SUBROUTINE CUT_DMODEL(DMODEL,MODEL)
    !
    ! BY RCE April 2012: New limits for DMODEL by Jesper Schou.
    ! Defined in INV_INIT
    !
    USE CONS_PARAM
    USE INV_PARAM

    IMPLICIT NONE

    REAL(DP), INTENT(INOUT),   DIMENSION(10)         ::  DMODEL
    REAL(DP), INTENT(IN),   DIMENSION(10)            ::  MODEL
    INTEGER                                          ::  I

    DO I=1,10
     !  IF (RLIMIT(I).EQ.0D0) THEN ! Use absolute limits
     !     DMODEL(I)=MIN(MAX(DMODEL(I),-DLIMIT(I)),DLIMIT(I))
     !  ELSE
     !     DMODEL(I)=MIN(MAX(MODEL(I)+DMODEL(I),MODEL(I)/RLIMIT(I)),MODEL(I)*RLIMIT(I))-MODEL(I)
 !      ENDIF
       IF (ABS(DMODEL(I)) .GT. ABS(DLIMIT(I))) DMODEL(I) = DMODEL(I)/ABS(DMODEL(I))*DLIMIT(I)

    ENDDO
    !   
  END SUBROUTINE CUT_DMODEL
  !
  !------------------------------------------------------------
  !
  SUBROUTINE GET_ERR(HESS, CHI2,SIGMA,CONV_FLAG)
  ! 
  ! The variances and co-variances are the elements of the inverse of the 
  ! Hessian matrix when the algorithm has converged. The errors are scaled
  ! by chi2 over the number of degrees of freedom. 

    USE CONS_PARAM
    USE INV_PARAM
    USE LINE_PARAM

    IMPLICIT NONE

    REAL(DP), DIMENSION(NUMFREE_PARAM,NUMFREE_PARAM)                :: VT, U, COV
    REAL(DP), DIMENSION(15*NUMFREE_PARAM)                           :: WORK
    REAL(DP), DIMENSION(NUMFREE_PARAM)                              :: W
    REAL(DP)                                                        :: CHI2
    REAL(DP), DIMENSION(10,10)                                      :: COV_DUMMY
    REAL(DP), DIMENSION(16)                                         :: SIGMA
    REAL(DP), INTENT(INOUT), DIMENSION(NUMFREE_PARAM,NUMFREE_PARAM) :: HESS
    INTEGER                                                         :: I, J, INFO, NFREE
    INTEGER, INTENT(OUT)                                            :: CONV_FLAG
    !

    ! Use LAPACK for calculating SVD of the HESSIAN
    W(:)=0D0

    CALL DGESVD('A','A',NUMFREE_PARAM,NUMFREE_PARAM,HESS,NUMFREE_PARAM, &
         W,U,NUMFREE_PARAM,VT,NUMFREE_PARAM,WORK,15*NUMFREE_PARAM,INFO)
    IF (INFO.NE.0) THEN
       PRINT*,'ERROR IN SVD'
       CONV_FLAG=1
       SIGMA = 0D0
    ELSE
       CONV_FLAG=0

       DO I=1,NUMFREE_PARAM
          DO J=1,NUMFREE_PARAM
             COV(I,J)=CHI2/DBLE(NUMFREE_DEG)*SUM(U(I,:)*U(J,:)/W(:))
             ! Re-normalizing the derivatives:
             COV(I,J)=COV(I,J)*(NORM(FREELOC(I))*NORM(FREELOC(J)))
          ENDDO
       ENDDO
       !
       DO I=1,NUMFREE_PARAM
          DO J=1,NUMFREE_PARAM
             COV_DUMMY(FREELOC(I),FREELOC(J))=COV(I,J)
          ENDDO
       ENDDO
       !--------------------------------
       ! Variances (square root of)
       !--------------------------------
       DO I=1,10
          SIGMA(I)=SQRT(COV_DUMMY(I,I))
       ENDDO
       !--------------------------------
       ! Covariances (normalized)
       !--------------------------------
       ! B-Gamma
       SIGMA(11)=COV_DUMMY(6,2)/SIGMA(6)/SIGMA(2)
       ! B-Phi
       SIGMA(12)=COV_DUMMY(6,3)/SIGMA(6)/SIGMA(3)
       ! Gamma-Phi
       SIGMA(13)=COV_DUMMY(2,3)/SIGMA(2)/SIGMA(3)
       ! B-Alpha
       SIGMA(14)=COV_DUMMY(6,10)/SIGMA(6)/SIGMA(10)
       ! Gamma-Alpha
       SIGMA(15)=COV_DUMMY(2,10)/SIGMA(10)/SIGMA(2)
       ! Phi-Alpha
       SIGMA(16)=COV_DUMMY(3,10)/SIGMA(3)/SIGMA(10)
    ENDIF
  END SUBROUTINE GET_ERR
  !-------------------------------------------
  
END MODULE INV_UTILS
