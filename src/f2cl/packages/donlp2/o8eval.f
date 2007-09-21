C********************************************************************
      SUBROUTINE O8EVAL(SIGACT,SIGRES,REJECT,ERROR)
      INCLUDE 'O8COMM.INC'
      INCLUDE 'O8CONS.INC'
      INCLUDE 'O8FINT.INC'
      DOUBLE PRECISION SIGACT,SIGRES
      LOGICAL REJECT,ERROR
C***  LOCAL
      INTEGER I,J
      DOUBLE PRECISION TERM
      SAVE
C**** EVALUATE THE FUNCTIONS AT THE NEW POINT
      SIG=SIGACT
      DO I =1,N
        X1(I)=X(I)+SIG*(D(I)+SIG*DD(I))
C****** PROJECT WITH RESPECT TO THE BOX-CONSTRAINTS
        IF ( LLOW(I) ) X1(I)=MAX(X1(I),UG(I))
        IF ( LUP(I) ) X1(I)=MIN(X1(I),OG(I))
      ENDDO
      REJECT=.FALSE.
      ERROR=.FALSE.
      SIGRES=SIG
      UPSI1=ZERO
      PSI1=ZERO
      IF ( BLOC ) CALL USER_EVAL(X1,-1)
C***** ONLY FUNCTION VALUES, FROM XTR=X1*XSC
      DO J = 1,NRES
        I=SORT(J)
        IF ( I .LE. NH ) THEN
          CFUERR(I)=.FALSE.
          CALL ESH(I,X1,RES1(I))
          IF ( CFUERR(I) ) THEN
            ERROR=.TRUE.
            RETURN
          ENDIF
          TERM=ABS(RES1(I))
        ELSE
          CFUERR(I)=.FALSE.
          CALL ESG(I-NH,X1,RES1(I))
          IF ( CFUERR(I) ) THEN
            ERROR=.TRUE.
            RETURN
          ENDIF
          TERM=-MIN(ZERO,RES1(I))
          IF ( RES1(I) .LT. -DELMIN .AND. BIND(I) .EQ. 0) THEN
            VIOLIS(0)=VIOLIS(0)+1
            VIOLIS(VIOLIS(0))=I
          ENDIF
        ENDIF
C***** VIOLIS IS THE LIST OF INEQUALITY-CONTRAINTS CURRENTLY
C***** NOT BINDING WHICH HAVE BEEN HIT DURING UNIDIMENSIONAL SEARCH
C***** SIGRES IS THE SMALLEST ZERO OF SECANTS THROUGH CONSTRAINTS
C***** WHICH CHANGE SIGN ALONG [X,X+D]
        UPSI1=UPSI1+TERM
        IF ( UPSI1 .GT. TAU0 .AND. PHASE .NE. -1  ) THEN
          REJECT=.TRUE.
          RETURN
        ENDIF
        PSI1=PSI1+TERM*W(I)
        IF ( RES1(I)*RES(I) .LT. ZERO .AND. SIG .LE. ONE
     F       .AND. ( BIND(I) .EQ. 0 .OR.
     F            (BIND(I) .EQ. 1 .AND. (ABS(RES(I))/GRESN(I)
     F             .GE. TP3*EPSMAC .OR. ABS(RES1(I))/GRESN(I)
     F             .GE. TP3*EPSMAC ) ) )
     F     )
     F      SIGRES=MIN(SIGRES,SIG*RES(I)/(RES(I)-RES1(I)))
      ENDDO
      IF ( PHASE .NE. -1 ) THEN
        FFUERR=.FALSE.
        CALL ESF(X1,FX1)
        IF ( FFUERR ) THEN
          ERROR=.TRUE.
          RETURN
        ENDIF
      ELSE
        FX1=ZERO
      ENDIF
      PHI1=SCF*FX1+PSI1
      RETURN
      END
