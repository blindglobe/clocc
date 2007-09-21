C**********************************************************************
      SUBROUTINE O8LEFT(A,B,Y,YL,N)
      IMPLICIT NONE
      INCLUDE 'O8PARA.INC'
      INCLUDE 'O8CONS.INC'
      INTEGER N,I,J
      DOUBLE PRECISION A(NX,*),B(*),Y(*),YL,H
      SAVE
C     O8LEFT ASSUMES THAT THE CHOLESKY-FACTOR OF A
C     A=R(TRANSPOSE)*R IS STORED IN THE UPPER HALF OF A.
C     B IS A RIGHT HAND SIDE. O8LEFT SOLVES
C         R(TRANSPOSE)*Y = B
C     YL=NORM(Y)**2
      YL=ZERO
      DO   I=1,N
        H=B(I)
        DO    J=1,I-1
          H = H - A(J,I)*Y(J)
        ENDDO
        H=H/A(I,I)
        Y(I)=H
        YL = H**2 + YL
      ENDDO
      RETURN
      END
