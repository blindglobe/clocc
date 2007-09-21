C*********************************************************************
C     SOLVE TRIANGULAR SYSTEM R(TRANSPOSE)*X=B, R DEFINED BY
C     HOUSEHOLDER-QR-DECOMPOSITION DECOMP (WITH COLUMN SCALING)
C*********************************************************************
      SUBROUTINE O8SOLT(NLOW,NUP,B,X)
      IMPLICIT NONE
      INTEGER NLOW,NUP
      INCLUDE 'O8PARA.INC'
      INCLUDE 'O8CONS.INC'
      DOUBLE PRECISION B(*),X(*)
      INCLUDE 'O8RDAT.INC'
C********
      INTEGER I,J
      DOUBLE PRECISION SUM
      SAVE
      DO I=NLOW,NUP
C*** B HAS BEEN PERMUTED ALREADY !
        X(I)=B(I)*CSCAL(COLNO(I))
      ENDDO
      DO I=NLOW,NUP
        SUM=ZERO
        DO J=NLOW,I-1
          SUM=SUM+QR(J,I)*X(J)
        ENDDO
        X(I)=(X(I)-SUM)/DIAG(I)
      ENDDO
      RETURN
      END
