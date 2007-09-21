C**********************************************************************
C********************************************************************
      SUBROUTINE O8EGPH(GPHI)
      INCLUDE 'O8COMM.INC'
      INCLUDE 'O8CONS.INC'
      DOUBLE PRECISION GPHI(NX)
      INTEGER I,J,L
      SAVE
C**** COMPUTE GRADIENT OF LAGRANGIAN
      DO  I=1,N
         GPHI(I)=GRADF(I) * SCF
         DO    J=1,NH
           GPHI(I)=GPHI(I)-U(J)*GRES(I,J)
         ENDDO
         DO    J=NH+1,ALIST(0)
           L=ALIST(J)
           IF( U(L) .GT. ZERO )
C**** INCLUDE CONSTRAINTS, WHOSE MULTIPLIERS ARE OF CORRECT SIGN ONLY
     F     GPHI(I)=GPHI(I)-GRES(I,L)*U(L)
         ENDDO
      ENDDO
      RETURN
      END
