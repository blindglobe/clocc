      SUBROUTINE EVAL_EXTERN(MODE)
      INCLUDE 'O8COMM.INC'
      INCLUDE 'O8FINT.INC'
      INCLUDE 'O8CONS.INC'
      INTEGER MODE,I,J
      DOUBLE PRECISION GR(NX)
      SAVE
C***** THIS IS A MODEL OF EVAL_EXTERN, SIMPLY CALLING THE
C***** STANDARD EVALUATION FUNCTIONS OF THE OPTIMIZATION PACKAGE
        IF ( MODE .EQ. 0 ) THEN
          DO I=1,NRES
            IF ( GUNIT(1,I) .EQ. 1 ) THEN
              IF ( I .LE. NH ) CALL EH(I,XTR,FU(I))
              IF ( I .GT. NH ) CALL EG(I-NH,XTR,FU(I))
            ENDIF
          ENDDO
          RETURN
        ENDIF
        CALL EF(XTR,FU(0))
        IF ( GUNIT(1,0) .NE. 1 .AND. MODE .EQ. 2 ) THEN
          IF ( ANALYT ) THEN
            CALL EGRADF(XTR,GR)
            DO I=1,N
              FUGRAD(I,0)=GR(I)
            ENDDO
          ENDIF
        ENDIF
        DO J=1,NH
          CALL EH(J,XTR,FU(J))
          IF ( GUNIT(1,J) .NE. 1 .AND. MODE .EQ. 2 ) THEN
            IF ( ANALYT ) THEN
              CALL EGRADH(J,XTR,GR)
              DO I=1,N
                FUGRAD(I,J)=GR(I)
              ENDDO
            ENDIF
          ENDIF
        ENDDO
        DO J=1,NG
          CALL EG(J,XTR,FU(NH+J))
          IF ( GUNIT(1,J+NH) .NE. 1 .AND. MODE .EQ. 2 ) THEN
            IF ( ANALYT ) THEN
              CALL EGRADG(J,XTR,GR)
              DO I=1,N
                FUGRAD(I,J+NH)=GR(I)
              ENDDO
            ENDIF
          ENDIF
        ENDDO
        VALID=.TRUE.
        RETURN
      END

