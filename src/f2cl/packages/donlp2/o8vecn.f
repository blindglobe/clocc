C******************************************************************
      DOUBLE PRECISION FUNCTION O8VECN(NL,NM,X)
      IMPLICIT NONE
      INCLUDE 'O8CONS.INC'
C*** EUCLIDEAN NORM OF X , AVOID OVERFLOW
      INTEGER NL,NM,I
      DOUBLE PRECISION X(*)
      DOUBLE PRECISION XM,H
      INTRINSIC MAX,ABS,SQRT
      SAVE
      IF ( NM .LT. NL ) THEN
        O8VECN=ZERO
        RETURN
      ENDIF
      XM=ABS(X(NL))
      DO I=NL+1,NM
        XM=MAX(XM,ABS(X(I)))
      ENDDO
      IF ( XM .EQ. ZERO ) THEN
        O8VECN=ZERO
        RETURN
      ELSE
        H=ZERO
        DO I=NL,NM
          H=H+(X(I)/XM)**2
        ENDDO
        O8VECN=XM*SQRT(H)
        RETURN
      ENDIF
      END
