C*********************************************************************
      SUBROUTINE O8DLCD(AI,L)
C************** DELETE CONSTRAINT NR. L
      INCLUDE 'O8COMM.INC'
      INCLUDE 'O8CONS.INC'
      DOUBLE PRECISION NP,RNORM,RLOW,XJ,DDUAL,R,UD,UD1
      INTEGER NDUALM,MDUALM,NDUAL,MI,ME,IQ
      PARAMETER (NDUALM=NX+NRESM,MDUALM=NRESM*2)
      COMMON /O8DUPA/RNORM,RLOW,NDUAL,MI,ME,IQ
      COMMON /O8QPUP/XJ(NDUALM,NDUALM),DDUAL(NDUALM),R(NDUALM,NDUALM),
     F                NP(NDUALM),UD(MDUALM),UD1(MDUALM)
      INTEGER AI(MDUALM),QQ,L,I,J,K
      DOUBLE PRECISION T1,T2,CC,SS,H,C1,S1,XNY,O8DSQ1
      SAVE
      DO I=1,IQ
        IF(AI(I) .EQ. L)      THEN
          QQ=I
          GOTO 10
         ENDIF
      ENDDO
   10 CONTINUE
      DO I=QQ,IQ-1
        AI(I)=AI(I+1)
        UD1(I)=UD1(I+1)
        DO J=1,NDUAL
          R(J,I)=R(J,I+1)
        ENDDO
      ENDDO
   20 CONTINUE
      AI(IQ)=AI(IQ+1)
      UD1(IQ)=UD1(IQ+1)
      AI(IQ+1)=0
      UD1(IQ+1)=ZERO
      DO  J=1,IQ
        R(J,IQ)=ZERO
      ENDDO
      IQ=IQ-1
      IF(IQ .EQ. 0)     GOTO 100
      DO J=QQ,IQ
        CC=R(J,J)
        SS=R(J+1,J)
        H=O8DSQ1(CC,SS)
        IF(H .EQ. ZERO)            GOTO 90
        C1=CC/H
        S1=SS/H
        R(J+1,J)=ZERO
        IF ( C1 .LT. ZERO ) THEN
          R(J,J)=-H
          C1=-C1
          S1=-S1
        ELSE
          R(J,J)=H
        ENDIF
        XNY=S1/(ONE+C1)
        DO K=J+1,IQ
          T1=R(J,K)
          T2=R(J+1,K)
          R(J,K)=T1*C1+T2*S1
          R(J+1,K)=XNY*(T1+R(J,K))-T2
        ENDDO
        DO K=1,NDUAL
          T1=XJ(K,J)
          T2=XJ(K,J+1)
          XJ(K,J)=T1*C1+T2*S1
          XJ(K,J+1)=XNY*(XJ(K,J)+T1)-T2
        ENDDO
   90 CONTINUE
      ENDDO
  100 CONTINUE
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
