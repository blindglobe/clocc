C     GRADIENT OF OBJECTIVE FUNCTION
      SUBROUTINE ESGRADF(X,GRADF)
      INCLUDE 'O8FUCO.INC'
      INCLUDE 'O8FINT.INC'
      INCLUDE 'O8CONS.INC'
      DOUBLE PRECISION X(*),GRADF(*)
C     USER DECLARATIONS, IF ANY ,FOLLOW
      INTEGER J
      DOUBLE PRECISION D1,D2,D3,SD1,SD2,SD3,FHELP,FHELP1,FHELP2,
     *   FHELP3,FHELP4,FHELP5,FHELP6,XINCR,XHELP,FLOC
      SAVE
      IF ( GUNIT(1,0) .EQ. 1 ) THEN
        DO J=1,N
          GRADF(J)=ZERO
        ENDDO
        GRADF(GUNIT(2,0))=GUNIT(3,0)*XSC(GUNIT(2,0))
        RETURN
      ENDIF
      IF ( BLOC ) THEN
        IF ( VALID ) THEN
          ICGF=ICGF+1
          DO           J=1,N
	    GRADF(J)=XSC(J)*FUGRAD(J,0)
          ENDDO
          RETURN
        ELSE
          STOP 'DONLP2: BLOC CALL WITH FUNCTION INFO INVALID'
        ENDIF
      ELSE
        DO J=1,N
          XTR(J)=XSC(J)*X(J)
        ENDDO
        IF ( ANALYT ) THEN
           CALL EGRADF(XTR,GRADF)
        ELSE
        IF ( DIFFTYPE .EQ. 1 ) THEN
          DELDIF=MIN(TM1*SQRT(EPSFCN),TM2)
          CALL EF(XTR,FLOC)
          DO J=1,N
            XHELP=XTR(J)
            XINCR=MIN(TM2,DELDIF*ABS(XHELP)+DELDIF,TAUBND)
            IF ( XHELP .GE. ZERO ) THEN
              XTR(J)=XHELP+XINCR
            ELSE
              XTR(J)=XHELP-XINCR
            ENDIF
            CALL EF(XTR,FHELP)
            GRADF(J)=(FHELP-FLOC)/(XTR(J)-XHELP)
            XTR(J)=XHELP
          ENDDO
        ELSEIF ( DIFFTYPE .EQ. 2 ) THEN
          DELDIF=MIN(TM1*EPSFCN**(ONE/THREE),TM2)
          DO J=1,N
            XHELP=XTR(J)
            XINCR=MIN(TM2,DELDIF*ABS(XHELP)+DELDIF,TAUBND)
            XTR(J)=XHELP+XINCR
            CALL EF(XTR,FHELP1)
            XTR(J)=XHELP-XINCR
            CALL EF(XTR,FHELP2)
            GRADF(J)=(FHELP1-FHELP2)/(XINCR+XINCR)
            XTR(J)=XHELP
          ENDDO
        ELSE
          DELDIF=MIN(TM1*EPSFCN**(ONE/SEVEN),TM2)
          DO J=1,N
            XHELP=XTR(J)
            XINCR=MIN(TM2,DELDIF*ABS(XHELP)+DELDIF,TAUBND/FOUR)
            XTR(J)=XHELP-XINCR
            CALL EF(XTR,FHELP1)
            XTR(J)=XHELP+XINCR
            CALL EF(XTR,FHELP2)
            XINCR=XINCR+XINCR
            D1=XINCR
            XTR(J)=XHELP-XINCR
            CALL EF(XTR,FHELP3)
            XTR(J)=XHELP+XINCR
            CALL EF(XTR,FHELP4)
            XINCR=XINCR+XINCR
            D2=XINCR
            XTR(J)=XHELP-XINCR
            CALL EF(XTR,FHELP5)
            XTR(J)=XHELP+XINCR
            CALL EF(XTR,FHELP6)
            XTR(J)=XHELP
            D3=XINCR+XINCR
            SD1=(FHELP2-FHELP1)/D1
            SD2=(FHELP4-FHELP3)/D2
            SD3=(FHELP6-FHELP5)/D3
            SD3=SD2-SD3
            SD2=SD1-SD2
            SD3=SD2-SD3
            GRADF(J)=SD1+P4*SD2+SD3/C45
          ENDDO
        ENDIF
        ENDIF
        DO J=1,N
          GRADF(J)=XSC(J)*GRADF(J)
        ENDDO
      ENDIF
      RETURN
      END
