C EVALUATION OF GRADIENT OF I-TH INEQUALITY
C NOT NECESSARY FOR BOUNDS, BUT CONSTANT GRADIENTS MUST BE SET
C HERE E.G. USING DCOPY FROM A DATA-FIELD
      SUBROUTINE ESGRADG(I,X,GRADGI)
      INCLUDE 'O8FUCO.INC'
      INCLUDE 'O8CONS.INC'
      INCLUDE 'O8FINT.INC'
      DOUBLE PRECISION X(*) ,GRADGI(*)
      INTEGER I,J
      DOUBLE PRECISION D1,D2,D3,SD1,SD2,SD3,FHELP,FHELP1,FHELP2,
     *   FHELP3,FHELP4,FHELP5,FHELP6,XINCR,XHELP,GIXLOC
      SAVE
      IF ( GUNIT(1,I+NH) .EQ. 1 ) THEN
        DO  J=1,N
	  GRADGI(J)=ZERO
        ENDDO
        GRADGI(GUNIT(2,I+NH))=GUNIT(3,I+NH)*XSC(GUNIT(2,I+NH))
        RETURN
      ENDIF
      IF ( BLOC ) THEN
        IF ( VALID ) THEN
          IF ( GUNIT(1,I+NH) .NE. 1 ) CGRES(I+NH)=CGRES(I+NH)+1
          DO J=1,N
            GRADGI(J)=XSC(J)*FUGRAD(J,I+NH)
          ENDDO
          RETURN
        ELSE
          STOP 'DONLP2: BLOC CALL WITH FUNCTION INFO INVALID'
        ENDIF
      ELSE
        DO J=1,N
          XTR(J)=X(J)*XSC(J)
        ENDDO
        IF ( ANALYT ) THEN
          CALL EGRADG(I,XTR,GRADGI)
        ELSE
        IF ( DIFFTYPE .EQ. 1 ) THEN
          DELDIF=MIN(TM1*SQRT(EPSFCN),TM2)
          CALL EG(I,XTR,GIXLOC)
          DO J=1,N
            XHELP=XTR(J)
            XINCR=MIN(TM2,DELDIF*ABS(XHELP)+DELDIF,TAUBND)
            IF ( XHELP .GE. ZERO ) THEN
              XTR(J)=XHELP+XINCR
            ELSE
              XTR(J)=XHELP-XINCR
            ENDIF
            CALL EG(I,XTR,FHELP)
            GRADGI(J)=(FHELP-GIXLOC)/(XTR(J)-XHELP)
            XTR(J)=XHELP
          ENDDO
        ELSEIF ( DIFFTYPE .EQ. 2 ) THEN
          DELDIF=MIN(TM1*EPSFCN**(ONE/THREE),TM2)
          DO J=1,N
            XHELP=XTR(J)
            XINCR=MIN(TM2,DELDIF*ABS(XHELP)+DELDIF,TAUBND)
            XTR(J)=XHELP+XINCR
            CALL EG(I,XTR,FHELP1)
            XTR(J)=XHELP-XINCR
            CALL EG(I,XTR,FHELP2)
            GRADGI(J)=(FHELP1-FHELP2)/(XINCR+XINCR)
            XTR(J)=XHELP
          ENDDO
        ELSE
          DELDIF=MIN(TM1*EPSFCN**(ONE/SEVEN),TM2)
          DO J=1,N
            XHELP=XTR(J)
            XINCR=MIN(TM2,DELDIF*ABS(XHELP)+DELDIF,TAUBND/FOUR)
            XTR(J)=XHELP-XINCR
            CALL EG(I,XTR,FHELP1)
            XTR(J)=XHELP+XINCR
            CALL EG(I,XTR,FHELP2)
            XINCR=XINCR+XINCR
            D1=XINCR
            XTR(J)=XHELP-XINCR
            CALL EG(I,XTR,FHELP3)
            XTR(J)=XHELP+XINCR
            CALL EG(I,XTR,FHELP4)
            XINCR=XINCR+XINCR
            D2=XINCR
            XTR(J)=XHELP-XINCR
            CALL EG(I,XTR,FHELP5)
            XTR(J)=XHELP+XINCR
            CALL EG(I,XTR,FHELP6)
            XTR(J)=XHELP
            D3=XINCR+XINCR
            SD1=(FHELP2-FHELP1)/D1
            SD2=(FHELP4-FHELP3)/D2
            SD3=(FHELP6-FHELP5)/D3
            SD3=SD2-SD3
            SD2=SD1-SD2
            SD3=SD2-SD3
            GRADGI(J)=SD1+P4*SD2+SD3/C45
          ENDDO
        ENDIF

        ENDIF
        DO J=1,N
          GRADGI(J)=XSC(J)*GRADGI(J)
        ENDDO
      ENDIF
      RETURN
      END
