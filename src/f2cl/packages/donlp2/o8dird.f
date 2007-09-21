C****************************************************************
C     COMPUTE THE DIRECTIONAL DERIVATIVE OF THE L1-PENALTY-FUNCTION
C****************************************************************
      SUBROUTINE O8DIRD
      INCLUDE 'O8COMM.INC'
      INCLUDE 'O8CONS.INC'
      INTEGER I
      DOUBLE PRECISION TERM,TERM1,O8SC1,O8SC3
      EXTERNAL O8SC1,O8SC3
      SAVE
C***** COMPUTE DIRECTIONAL DERIVATIVE OF ZANGWILL FUNCTION
  100 CONTINUE
      DIRDER=O8SC1(1,N,GRADF,D)*SCF
      DO  I=1,NRES
        TERM=O8SC3(1,N,I,GRES,NX,D)*W(I)
        TERM1=RES(I)
        IF ( I .LE. NH ) THEN
          IF ( TERM1/GRESN(I) .LT. -TP3*EPSMAC ) THEN
            DIRDER=DIRDER-TERM
          ELSE
            IF ( TERM1/GRESN(I) .GT. TP3*EPSMAC ) THEN
              DIRDER=DIRDER+TERM
            ELSE
              DIRDER=DIRDER+ABS(TERM)
            ENDIF
          ENDIF
        ELSE
          IF ( BIND(I) .EQ. 1 ) THEN
            IF ( ABS(TERM1)/GRESN(I) .LE. TP3*EPSMAC ) THEN
              DIRDER=DIRDER-MIN(ZERO,TERM)
            ELSE
              IF ( TERM1/GRESN(I) .LT. -TP3*EPSMAC ) THEN
                IF ( TERM .GT. ZERO ) TERM=MIN(TERM,-RES(I)*W(I))
C*****  ONLY NEGATIVE VALUES OF THE CONSTRAINTS CONTRIBUTE TO THE
C*****  ZANGWILL FUNCTION
                DIRDER=DIRDER-TERM
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDDO
      RETURN
      END
