FUNCTION FACTORIAL (N)

  !This function computes n!, i.e. n*(n-1)*(n-2)*...*1   
  IMPLICIT NONE
  INTEGER FACTORIAL, N, I
  
  FACTORIAL = 1
  IF ( N .GT. 1 ) THEN
     DO I = 2, N
        FACTORIAL = FACTORIAL * I
     END DO
  ENDIF
  
END FUNCTION FACTORIAL
