      DOUBLE PRECISION FUNCTION DD7TPR(P, X, Y)
C
C  ***  RETURN THE INNER PRODUCT OF THE P-VECTORS X AND Y.  ***
C
      INTEGER P
      DOUBLE PRECISION X(P), Y(P)
C
      INTEGER I
      DOUBLE PRECISION DR7MDC
      EXTERNAL DR7MDC
C  ***  ACTIVATE THE *'ED COMMENT LINES BELOW IF UNDERFLOW IS A PROBLEM.
C  ***  DR7MDC(2) RETURNS A MACHINE-DEPENDENT CONSTANT, SQTETA, WHICH
C  ***  IS SLIGHTLY LARGER THAN THE SMALLEST POSITIVE NUMBER THAT
C  ***  CAN BE SQUARED WITHOUT UNDERFLOWING.
C
      DOUBLE PRECISION ONE, ZERO
      PARAMETER (ONE=1.D+0, ZERO=0.D+0)
*     DOUBLE PRECISION SQTETA, T
*      DATA SQTETA/0.D+0/
C
      DD7TPR = ZERO
*      IF (P .LE. 0) GO TO 999
*      IF (SQTETA .EQ. ZERO) SQTETA = DR7MDC(2)
      DO 20 I = 1, P
*         T = DMAX1(DABS(X(I)), DABS(Y(I)))
*         IF (T .GT. ONE) GO TO 10
*         IF (T .LT. SQTETA) GO TO 20
*         T = (X(I)/SQTETA)*Y(I)
*         IF (DABS(T) .LT. SQTETA) GO TO 20
 10      DD7TPR = DD7TPR + X(I)*Y(I)
 20   CONTINUE
C
 999  RETURN
C  ***  LAST LINE OF DD7TPR FOLLOWS  ***
      END
