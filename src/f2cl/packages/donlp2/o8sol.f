C*********************************************************************
C     SOLVE TRIANGULAR SYSTEM R*X=B, R DEFINED BY HOUSEHOLDER-QR-
C     DECOMPOSITION DECOMP (WITH COLUMN SCALING)
C*********************************************************************
      SUBROUTINE O8SOL(NLOW,NUP,B,X)
      IMPLICIT NONE
      INTEGER NLOW,NUP
      INCLUDE 'O8PARA.INC'
      INCLUDE 'O8CONS.INC'
      INCLUDE 'O8RDAT.INC'
      DOUBLE PRECISION B(*),X(*),XL(NX)
      SAVE
C*******
      DOUBLE PRECISION SUM
      INTEGER I,J
      DO I=NUP,NLOW,-1
        SUM=ZERO
        DO J=I+1,NUP
          SUM=SUM+QR(I,J)*XL(J)
        ENDDO
        XL(I)=(B(I)-SUM)/DIAG(I)
      ENDDO
      DO  I=NLOW,NUP
        X(I)=XL(I)*CSCAL(COLNO(I))
      ENDDO
C*** THERE MUST FOLLOW INTERCHANGE OF X AS GIVEN BY COLNO
C    E.G. XX(COLNO(I))=X(I)
C***
      RETURN
      END
