C********************************************************************
      SUBROUTINE O8REST
C**** RESTORE THE BEST POINT FOUND SO FAR TO BE THE CURRENT NEW POINT
      INCLUDE 'O8COMM.INC'
      INTEGER J
      SAVE
        PHI1=PHIMIN
        PSI1=PSIMIN
        UPSI1=UPSIM
        SIG=SIGMIN
        FX1=FMIN
        DO J=1,N
          X1(J)=XMIN(J)
        ENDDO
        DO J=1,NRES
          RES1(J)=RESMIN(J)
        ENDDO
      RETURN
      END
