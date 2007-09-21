C*******************************************************************
C     SCALAR PRODUCT OF TWO VECTORS OR PARTS OF VECTORS
C*******************************************************************
      DOUBLEPRECISION FUNCTION O8SC1(I,J,A,B)
C     MULPILY TWO VECTORS
      IMPLICIT NONE
      INCLUDE 'O8CONS.INC'
      INTEGER I,J,K
      DOUBLE PRECISION A(*),B(*)
      DOUBLE PRECISION S
      SAVE
        IF ( I .GT. J ) THEN
          O8SC1=ZERO
          RETURN
        ELSE
          S=ZERO
          DO   K=I,J
            S=S+A(K)*B(K)
          ENDDO
          O8SC1=S
          RETURN
        ENDIF
      END
