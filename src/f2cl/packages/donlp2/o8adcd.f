C***********************************************************************
      SUBROUTINE O8ADCD
C********** ADD CONSTRAINT WHOSE GRADIENT IS GIVEN BY NP
      INCLUDE 'O8COMM.INC'
      INCLUDE 'O8CONS.INC'
      DOUBLE PRECISION NP,RNORM,RLOW,XJ,DDUAL,R,UD,UD1
      INTEGER NDUALM,MDUALM,NDUAL,MI,ME,IQ
      PARAMETER (NDUALM=NX+NRESM,MDUALM=NRESM*2)
      COMMON /O8DUPA/RNORM,RLOW,NDUAL,MI,ME,IQ
      COMMON /O8QPUP/XJ(NDUALM,NDUALM),DDUAL(NDUALM),R(NDUALM,NDUALM),
     F                NP(NDUALM),UD(MDUALM),UD1(MDUALM)
      INTEGER I,J,K
      DOUBLE PRECISION CC,SS,H,S1,C1,T1,T2,O8DSQ1,XNY
      SAVE
      DO J=NDUAL,IQ+2,-1
        CC=DDUAL(J-1)
        SS=DDUAL(J)
        H=O8DSQ1(CC,SS)
        IF(H .EQ. ZERO)           GOTO 20
        DDUAL(J)=ZERO
        S1=SS/H
        C1=CC/H
        IF ( C1 .LT. ZERO ) THEN
          C1=-C1
          S1=-S1
          DDUAL(J-1)=-H
        ELSE
          DDUAL(J-1)=H
        ENDIF
        XNY=S1/(ONE+C1)
        DO K=1,NDUAL
          T1=XJ(K,J-1)
          T2=XJ(K,J)
          XJ(K,J-1)=T1*C1+T2*S1
          XJ(K,J)=XNY*(T1+XJ(K,J-1))-T2
        ENDDO
   20 CONTINUE
      ENDDO
      IQ=IQ+1
      DO I=1,IQ
        R(I,IQ)=DDUAL(I)
      ENDDO
      RNORM=ONE
      RLOW=ONE
C*** IN ORDER TO AVOID A COMPILER ERROR OF HP IN +OP3 MODE
      IF ( IQ .GE. 1 ) THEN
        RNORM=ABS(R(1,1))
        RLOW=ABS(R(1,1))
        I=1
        DO WHILE ( I .LT. IQ )
          I=I+1
          RNORM=MAX(RNORM,ABS(R(I,I)))
          RLOW=MIN(RLOW,ABS(R(I,I)))
        ENDDO
      ENDIF
      RETURN
      END
