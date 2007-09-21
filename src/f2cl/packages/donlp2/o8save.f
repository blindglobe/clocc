C********************************************************************
      SUBROUTINE O8SAVE
C**** SAVE THE BEST POINT FOUND SO FAR IN ...MIN VARIABLES
      INCLUDE 'O8COMM.INC'
      INTEGER I
      SAVE
        PHIMIN=PHI1
        UPSIM=UPSI1
        PSIMIN=PSI1
        FMIN = FX1
        SIGMIN = SIG
        DO  I=1,N
          XMIN(I)=X1(I)
        ENDDO
        DO  I=1,NRES
          RESMIN(I)=RES1(I)
        ENDDO
      RETURN
      END
