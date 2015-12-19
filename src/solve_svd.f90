
SUBROUTINE SOLVE_SVD(U,W,VT,N,B,X)
!
! R. Centeno
! December 2015
! This routine solves a system of equations A X = B following the Lapack SVD decomposition
! of the matrix A. The LAPACK SVD decomposes A in the form A = U W V, where U and V are
! unitary matrices (which for real numbers means they are orthogonal matrices) and W is a
! diagonal matrix of eigenvalues. The inverse of an orthogonal matrix is its transverse, 
! so solving the A X = B system becomes very easy because the inverse of A is 
! straightforward to calculate: inverse(A) = transpose(V) x inverse(W) x transpose(U).
! The Lapack SVD returns VT = transpose(V) rather than V.
!
! B is the right-hand side of the system of equations. X is the vector we want to calculate.
! The SVD algorithm works in general for rectangular matrices, however, this solver is 
! assuming that the original matrix A is square (N x N) so all of the matrices in the 
! decomposition are also N x N.

  USE CONS_PARAM
  IMPLICIT NONE

  REAL(DP), DIMENSION(N,N)           :: U, VT
  REAL(DP), DIMENSION(N)             :: W, B, X, temp       
  INTEGER                            :: N, I, J
  

  
  DO I = 1, N
     temp(i) = 0D0
     ! Multiply transpose(U) x B
     DO J = 1, N
        temp(I) = temp(I) + U(J,I)*B(J)
     ENDDO
     ! Multiply by the inverse(W), which is just diag(1/Wi)
     IF (W(I) .NE. 0D0) THEN
        temp(I) = temp(I)/W(I)
     ENDIF
  ENDDO

  ! And now multiply transpose(V) = VT by the temp variable
  
  
  DO I = 1, N
     x(I) = 0D0
     DO J = 1, N
        x(I) = x(I) + VT(J,I)*temp(J)
     ENDDO
  ENDDO
  
  
END SUBROUTINE SOLVE_SVD
