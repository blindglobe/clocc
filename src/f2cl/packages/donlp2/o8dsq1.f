C***************************************************************
C     LENGHT OF VECTOR (A,B). NUMERICALLY STABLE VERSION WITH
C     OVERFLOW / UNDERFLOW SAVEGUARD
C*****************************************************************
      DOUBLE PRECISION FUNCTION  O8DSQ1(A,B)
      IMPLICIT NONE
      DOUBLE PRECISION A,B,A1,B1
      INCLUDE 'O8CONS.INC'
      SAVE
      A1=ABS(A)
      B1=ABS(B)
      IF ( A1 .GT. B1 ) THEN
        O8DSQ1=A1*SQRT(ONE+(B1/A1)**2)
      ELSE
        IF ( B1 .GT. A1 ) THEN
          O8DSQ1=B1*SQRT(ONE+(A1/B1)**2)
        ELSE
          O8DSQ1=A1*SQRT(TWO)
        ENDIF
      ENDIF
      RETURN
      END
