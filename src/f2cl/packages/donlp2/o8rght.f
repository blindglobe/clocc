C***********************************************************************
      SUBROUTINE O8RGHT(A,B,Y,YL,N)
      IMPLICIT NONE
      INCLUDE 'O8PARA.INC'
      INCLUDE 'O8CONS.INC'
      INTEGER N,I,J
      DOUBLE PRECISION A(NX,*),B(*),Y(*),YL,H
      SAVE
C     O8RGHT ASSUMES THAT THE CHOLESKY-FACTOR OF A
C     A=R(TRANSPOSE)*R IS STORED IN THE UPPER HALF OF A.
C     B IS A RIGHT HAND SIDE. O8RGHT SOLVES
C         R*Y = B
C     YL=NORM(Y)**2
      YL=ZERO
      DO   I=N,1,-1
        H=B(I)
        DO    J=I+1,N
          H = H - A(I,J)*Y(J)
        ENDDO
        H=H/A(I,I)
        Y(I)=H
        YL = H**2 + YL
      ENDDO
      RETURN
      END
