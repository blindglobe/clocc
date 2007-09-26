C   EXAMPLE MURTY 10.15 (MURTY K.G.: LINEAR COMPLEMENTARITY, LINEAR AND NONLINEAR
C   PROGRAMMING, HELDERMANN, BERLIN 1988 )
      SUBROUTINE SETUP
      RETURN
      END
C   BETTING
      BLOCK DATA
      INCLUDE 'O8BLOC.INC'
      INTEGER I,J
      DOUBLE PRECISION BB,QQ,PP,PI(8),QI(8)
      COMMON/BET/BB,QQ,PP,PI,QI
      DATA NAME/'BETTING8'/
      DATA (X(I),I=1,8)/1.D0,2.D0,3.D0,4.D0,5.D0,6.D0,7.D0,8.D0/
      DATA (PI(I),I=1,8)/3*4.D3,8.D3,3.D3,8.D3,13.D3,5.D3/
      DATA BB,QQ/5.D2,.83D0/
      DATA N/8/ , NH/ 0/ , NG/9/
      DATA DEL0/1.00D0/ ,TAU0/1.D0/ ,TAU/.1D0/
      DATA (GUNIT(1,J),J=0,9)/2*-1,8*1/
      DATA (GUNIT(2,J),J=0,9)/2*0,1,2,3,4,5,6,7,8/
      DATA (GUNIT(3,J),J=0,9)/2*0,8*1/
      END
      SUBROUTINE SETUP0
      INCLUDE 'O8COMM.INC'
      INTEGER I
      DOUBLE PRECISION WW,WI(8)
      DOUBLE PRECISION BB,PP,QQ,PI(8),QI(8)
      COMMON/BET/BB,QQ,PP,PI,QI
      DATA (WI(I),I=1,8)/1.D4,1.5D4,5.D3,3.5D4,5.D3,1.D4,1.8D4,1.2D4/
      SILENT=.FALSE.
      PROU=10
      MEU=20
      NRESET=N
      WW=0.D0
      PP=0.D0
      ANALYT=.TRUE.
      EPSDIF=0.D0
      DO I=1,N
      WW=WW+WI(I)
      PP=PP+PI(I)
      ENDDO
      DO I=1,N
      QI(I)=WI(I)/WW
      ENDDO
      RETURN
      END
      SUBROUTINE EF(X,FX)
      INCLUDE 'O8FUCO.INC'
      DOUBLE PRECISION FX,X(NX)
      INTEGER I,J,L
      DOUBLE PRECISION SX,XIJ,FIJ,TERM
      DOUBLE PRECISION BB,PP,QQ,PI(8),QI(8)
      COMMON/BET/BB,QQ,PP,PI,QI
      ICF=ICF+1
      TERM=PP
      DO I=1,N
      TERM=TERM+X(I)
      ENDDO
      TERM=TERM*QQ
      FX=0.D0
      DO I=1,N
      DO J=1,N
      IF (J.NE.I) THEN
      XIJ=X(I)/(X(I)+PI(I))+X(J)/(X(J)+PI(J))
      SX=0.D0
      DO L=1,N
      IF (L.NE.I.AND.L.NE.J) SX=SX+X(L)
      ENDDO
      FIJ=.5D0*(TERM-X(I)-X(J)-PI(I)-PI(J))*XIJ+BB-SX
      FX=FX-QI(I)*QI(J)/(1.D0-QI(I))*LOG(FIJ)
      ENDIF
      ENDDO
      ENDDO
      RETURN
      END
      SUBROUTINE EGRADF(X,GRADF)
      INCLUDE 'O8FUCO.INC'
      DOUBLE PRECISION X(NX),GRADF(NX)
      INTEGER I,J,K
      DOUBLE PRECISION SX,FIJ
      DOUBLE PRECISION BB,PP,QQ,PI(8),QI(8)
      COMMON/BET/BB,QQ,PP,PI,QI
      DOUBLE PRECISION GRFIJ(NX),GRADT1(NX),GRADT2(NX),GRARG(NX)
      DOUBLE PRECISION T1,T2,ARG
      ICGF=ICGF+1
      SX=0.D0
      DO I=1,N
      SX=SX+X(I)
      GRADF(I)=0.D0
      ENDDO
      DO I=1,N
      DO J=1,N
        IF ( I .NE. J ) THEN
          DO K=1,N
          GRARG(K)=0.D0
            GRADT1(K)=0.D0
            GRADT2(K)=0.D0
          ENDDO
          T2=X(I)/(X(I)+PI(I))+X(J)/(X(J)+PI(J))
          GRADT2(I)=PI(I)/((X(I)+PI(I))**2)
          GRADT2(J)=PI(J)/((X(J)+PI(J))**2)
          T1=(QQ*(PP+SX)-X(I)-X(J)-PI(I)-PI(J))*.5D0
          DO K=1,N
            GRADT1(K)=QQ*.5D0
          ENDDO
          GRADT1(I)=GRADT1(I)-.5D0
          GRADT1(J)=GRADT1(J)-.5D0
          DO K=1,N
          GRFIJ(K)=T2*GRADT1(K)+T1*GRADT2(K)
          ENDDO
          FIJ=T1*T2
          ARG=FIJ+BB-SX+X(I)+X(J)
          DO K=1,N
          GRARG(K)=GRFIJ(K)-1.D0
          ENDDO
        GRARG(I)=GRARG(I)+1.D0
        GRARG(J)=GRARG(J)+1.D0
          DO K=1,N
          GRADF(K)=GRADF(K)-QI(I)*QI(J)/(1.D0-QI(I))/ARG*GRARG(K)
          ENDDO
        ENDIF
      ENDDO
      ENDDO
      RETURN
      END
      SUBROUTINE EH(I,X,HXI)
      INCLUDE 'O8FUCO.INC'
      INTEGER I
      DOUBLE PRECISION X(NX),HXI
      CRES(I)=CRES(I)+1
      RETURN
      END
      SUBROUTINE EGRADH(I,X,GRADHI)
      INCLUDE 'O8FUCO.INC'
      INTEGER I
      DOUBLE PRECISION X(NX),GRADHI(NX)
      CGRES(I)=CGRES(I)+1
      RETURN
      END
      SUBROUTINE EG(I,X,GXI)
      INCLUDE 'O8FUCO.INC'
      INTEGER I,J
      DOUBLE PRECISION X(NX),GXI
      DOUBLE PRECISION BB,PP,QQ,PI(8),QI(8)
      COMMON/BET/BB,QQ,PP,PI,QI
      IF (I.EQ.1) THEN
      GXI=BB
      DO J=1,N
      GXI=GXI-X(J)
      ENDDO
      ELSE
      GXI=X(I-1)
      ENDIF
      END
      SUBROUTINE EGRADG(I,X,GRADGI)
      INCLUDE 'O8FUCO.INC'
      INTEGER I,J
      DOUBLE PRECISION X(NX) ,GRADGI(NX)
      IF (I.EQ.1) THEN
      DO J=1,N
      GRADGI(J)=-1.D0
      ENDDO
      ENDIF
      RETURN
      END
