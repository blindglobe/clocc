      DOUBLE PRECISION FUNCTION O8SC2(N,M,J,A,LDA,B)
C     MULTIPLY ROW J OF MATRIX A WITH VECTOR B
      IMPLICIT NONE
      INCLUDE 'O8CONS.INC'
      DOUBLE PRECISION A,B,S
      INTEGER N,M,I,J,LDA
      DIMENSION A(LDA,*),B(*)
      SAVE
      S=ZERO
      DO  I=N,M
        S=S+A(J,I)*B(I)
      ENDDO
      O8SC2=S
      RETURN
      END
