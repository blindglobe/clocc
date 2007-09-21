C*******************************************************************
      SUBROUTINE O8SCE
C  COMPUTATION OF NEW SCALING FACTORS FOR L1-PENALTY-FUNCTION
      INCLUDE 'O8COMM.INC'
      INCLUDE 'O8CONS.INC'
      INTEGER I
      DOUBLE PRECISION TERM,S1,S2,DIFF0
      LOGICAL WLOW
      SAVE
      WLOW=.FALSE.
      DO I=1,NRES
C***** W1 TENTATIVE NEW WEIGHTS
        TERM=NY*ABS(U(I))+TAU
        IF ( TERM .GT. W(I) ) THEN
          W1(I)=TERM+TAU
        ELSE
          W1(I)=W(I)
          IF ( TERM .LT. W(I)*P5 .AND.
     F         BIND(I) .EQ. 1  ) W1(I)=(TERM+W(I))*P5
        ENDIF
        IF ( W1(I) .LT. W(I)) WLOW=.TRUE.
      ENDDO
C  WLOW EQUALS TRUE IF ONE TENTATIVE WEIGHT AT LEAST HAS BEEN DECREASED
      S1=ZERO
      S2=ZERO
      DO I=1,NRES
        IF ( I .LE. NH ) THEN
          S1=S1+W1(I)*ABS(RESST(I))
          S2=S2+W1(I)*ABS(RES(I))
        ELSE
          S1=S1-MIN(ZERO,RESST(I))*W1(I)
          S2=S2-MIN(ZERO,RES(I))*W1(I)
        ENDIF
      ENDDO
      DIFF0=(FXST-FX)*SCF+(S1-S2)
      IF ( WLOW .AND. DIFF0 .GE. ETA*CLOW
     F    .AND. ITSTEP-LASTDW .GT. MAX(5,MIN(20,N/10)) ) THEN
C***** ACCEPT NEW (DIMINISHED ) WEIGHTS
        IF ( CLOW .GT. ITSTEP/10 ) THEN
          ETA=ONEP3*ETA
          IF ( .NOT. SILENT ) CALL O8INFO(11)
        ENDIF
        LASTCH=ITSTEP
        LASTDW=ITSTEP
        LEVEL=DIFF0/ITERMA
        PSIST=S1
        PSI=S2
        DO I=1,NRES
          W(I)=W1(I)
        ENDDO
        CLOW=CLOW+ONE
      ELSE
C***** INCREASE INDIVIDUAL WEIGHTS IF NECESSARY. LET WEIGTHS UNCHANGED
C***** OTHERWISE
        S1=ZERO
        S2=ZERO
        DO I=1,NRES
          IF ( W1(I) .GT. W(I) ) THEN
            LASTUP=ITSTEP
            LASTCH=ITSTEP
          ENDIF
          W(I)=MAX(W(I),W1(I))
          IF ( I.LE. NH ) THEN
            S1=S1+W(I)*ABS(RESST(I))
            S2=S2+W(I)*ABS(RES(I))
          ELSE
            S1=S1-W(I)*MIN(ZERO,RESST(I))
            S2=S2-W(I)*MIN(ZERO,RES(I))
          ENDIF
        ENDDO
        PSIST=S1
        PSI=S2
      ENDIF
      TERM=ZERO
      IF ( NRES .GE. 1 ) TERM=W(1)
      DO I=2,NRES
        TERM=MAX(TERM,W(I))
      ENDDO
      ACCINF(ITSTEP,20)=TERM
C****** MAXIMUM OF WEIGHTS
      ACCINF(ITSTEP,19)=CLOW
      IF ( .NOT. SILENT ) CALL O8INFO(12)
      RETURN
      END
