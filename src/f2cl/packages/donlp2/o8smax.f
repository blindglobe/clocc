C********************************************************************
      SUBROUTINE O8SMAX
      INCLUDE 'O8COMM.INC'
      INCLUDE 'O8CONS.INC'
      INTEGER I
      LOGICAL EXIS
      SAVE
C****
C***** COMPUTE MAXIMUM STEPSIZE STMAXL SUCH THAT PROJECTION
C***** ON THE BOX OF LOWER AND UPPER BOUNDS CHANGES
C***** FOR SIG IN [0,STMAXL],IF SUCH EXISTS
      EXIS=.TRUE.
      DO I=1,N
        EXIS= EXIS .AND.( ( D(I) .EQ. ZERO )
     F        .OR. ( LUP(I) .AND. D(I) .GT. ZERO )
     F        .OR. ( LLOW(I) .AND. D(I) .LT. ZERO ) )
      ENDDO
      IF ( EXIS ) THEN
        STMAXL=SIGSM
        DO  I=1,N
          IF (LLOW(I) .AND. D(I) .LT. ZERO) THEN
            IF ( -D(I)*SIGLA .GE. X(I)-UG(I) ) THEN
              STMAXL=MAX(STMAXL,(X(I)-UG(I))/(-D(I)))
            ELSE
              STMAXL=SIGLA
            ENDIF
          ENDIF
          IF ( LUP(I) .AND. D(I) .GT. ZERO) THEN
            IF ( D(I)*SIGLA .GE. OG(I)-X(I) ) THEN
              STMAXL=MAX(STMAXL,(OG(I)-X(I))/D(I))
            ELSE
              STMAXL=SIGLA
            ENDIF
          ENDIF
        ENDDO
      ELSE
        STMAXL=SIGLA
      ENDIF
C***** BUT NEVER USE STEPSIZE LARGER THAN SIGLA
      STMAXL=MIN(SIGLA,STMAXL)
      RETURN
      END
