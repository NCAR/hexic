MODULE ZEEMAN

! R. Centeno
! April 2014
!
! This module calculates the 3j-coefficients for the Zeeman components of the multiplet. 
! Translated to FORTRAN from a routine that I got from M. Collados
!
! It is used in the generalization of the VFISV code to allow the inversion of general
! multiplets (that are not necessarily pure triplets).
!
  !---------------------------------------------------

CONTAINS

  SUBROUTINE threej (j1, j2, j3, m1, m2, m3, coef)
    
    IMPLICIT NONE
    
    
    INTEGER,  INTENT(IN)        :: j1, j2, j3, m1, m2, m3
    REAL                    :: coef, coef_aux
    INTEGER                 :: kmin1, kmin2, kmin
    INTEGER                 :: kmax1, kmax2, kmax3, kmax
    REAL                    :: term1, term2, term
    INTEGER                 :: msign, j, factorial
    
    !-------------------------------------------------
    
    ! Checking that the quantum numbers are in the right ranges
    
    coef_aux = 1
    coef = 0
    
    IF ((ABS(m1) .GT. j1) .OR. (ABS(m2) .GT. j2) .OR. (ABS(m3) .GT. j3)) THEN
       coef_aux = 0
    ENDIF
    IF ((j1+j2-j3) .LT. 0) THEN
       coef_aux = 0
    ENDIF
    IF ((j2+j3-j1) .LT. 0) THEN
       coef_aux = 0
    ENDIF
    IF ((j3+j1-j2) .LT. 0) THEN
       coef_aux = 0
    ENDIF
    
    IF ((m1+m2+m3) .NE. 0) THEN
       coef_aux = 0
    ENDIF
    
    IF (coef_aux .EQ. 1) THEN
       kmin1 = j3-j1-m2
       kmin2 = j3-j2+m1
       kmin = -MIN(kmin1, kmin2)
       IF (kmin .LT. 0) THEN 
          kmin = 0
       ENDIF
       
       kmax1 = j1 + j2 - j3
       kmax2 = j1 - m1
       kmax3 = j2 + m2
       kmax = MIN(kmax1, kmax2, kmax3)
       
       IF (kmin .GT. kmax) THEN 
          coef_aux = 0
       ENDIF
       
       CALL frontl(j1, j2, j3, m1, m2, m3, term1)
       msign = (-1)**(j1-j2-m3)
       
       coef = 0
       DO j = kmin, kmax 
          term2 = factorial(j) * factorial(kmin1+j) * factorial(kmin2+j)
          term2 = term2 * factorial(kmax1-j) * factorial(kmax2-j) * factorial(kmax3-j)
          term = (-1)**j * msign*term1/term2
          coef = coef + term
       ENDDO
       
       ! If the conditions at the beginning of the routine are not satisfied, then the 
       ! returned coefficient should be zero. We multiply by coef_aux, who carries the 
       ! information on whether or not the conditions are satisfied.
       
    ENDIF
    coef = coef * coef_aux
  END SUBROUTINE threej
  
  !---------------------------------------------------------------------------
  
SUBROUTINE frontl (x1, x2, x3, y1, y2, y3, dum)


  IMPLICIT NONE
  
  INTEGER, INTENT(IN)        :: x1, x2, x3, y1, y2, y3
  INTEGER                    :: l1, l2, l3, l4, l5, l6, l7, l8, l9, l10
  REAL                       :: dum
  INTEGER                    :: factorial
  l1 = x1+x2-x3
  l2 = x2+x3-x1
  l3 = x3+x1-x2
  l4 = x1+x2+x3+1
  l5 = x1+y1
  l6 = x1-y1
  l7 = x2+y2
  l8 = x2-y2
  l9 = x3+y3
  l10 = x3-y3

dum = 1
dum = dum *factorial(l1)*factorial(l2)*factorial(l3)*factorial(l5)/factorial(l4)
dum = dum*factorial(l6)*factorial(l7)*factorial(l8)*factorial(l9)*factorial(l10)

dum = sqrt(dum)

END SUBROUTINE frontl


END MODULE ZEEMAN
