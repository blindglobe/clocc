      SUBROUTINE SETUP0
      INCLUDE 'O8COMM.INC'
      DOUBLE PRECISION LENGTH,DIST,PI
      DOUBLE PRECISION THETAW(163)
      COMMON/ANTENNA/LENGTH,DIST,PI,THETAW
      INTEGER I,J
      N=7
      NH=0
      NG=333
      DEL0=0.01D0
      TAU0=1.D3
      TAU=0.1D0
      NAME='ANTENNA1'
      LENGTH=3.5D0
      DIST=0.4D0
      PI=4.D0*DATAN(1.D0)
      X(1)=10.D0
      DO I=2,7
        X(I)=DBLE(I-1)*LENGTH/7.D0
      ENDDO
      DO J=1,163
        THETAW(J)=PI*(8.5D0+DBLE(J)*0.5D0)/180.D0
      ENDDO
      ANALYT=.TRUE.
      EPSDIF=1.D-8
      DO I=2,NG
        GUNIT(1,I)=-1
        GUNIT(2,I)=0
        GUNIT(3,I)=0
      ENDDO
      DO J=1,3
        GUNIT(J,0)=1
      ENDDO
      GUNIT(1,1)=1
      GUNIT(2,1)=2
      GUNIT(3,1)=1
      PROU=10
      MEU=20
      RETURN
      END
      SUBROUTINE SETUP
      RETURN
      END
      SUBROUTINE EF(X,FX)
      INCLUDE 'O8FUCO.INC'
      DOUBLE PRECISION FX,X(*)
      ICF=ICF+1
      FX=X(1)
      RETURN
      END
      SUBROUTINE EGRADF(X,GRADF)
      INCLUDE 'O8FUCO.INC'
      DOUBLE PRECISION X(*),GRADF(*)
      INTEGER I
      ICGF=ICGF+1
      GRADF(1)=1.D0
      DO I=2,N
        GRADF(I)=0.D0
      ENDDO
      RETURN
      END
      SUBROUTINE EH(I,X,HXI)
      INCLUDE 'O8FUCO.INC'
      INTEGER I
      DOUBLE PRECISION HXI,X(*)
      RETURN
      END
      SUBROUTINE EGRADH(I,X,GRADHI)
      INCLUDE 'O8FUCO.INC'
      INTEGER I
      DOUBLE PRECISION X(*),GRADHI(*)
      RETURN
      END
      SUBROUTINE EG(I,X,GXI)
      INCLUDE 'O8FUCO.INC'
      DOUBLE PRECISION LENGTH,DIST,PI
      DOUBLE PRECISION THETAW(163)
      COMMON/ANTENNA/LENGTH,DIST,PI,THETAW
      INTEGER I,J,K
      DOUBLE PRECISION GXI,X(*)
      DOUBLE PRECISION CJ,FAC
      IF ( I .GT. 1 ) CRES(I)=CRES(I)+1
      IF ( I .EQ. 1 ) THEN
        GXI=X(2)-DIST
        RETURN
      ELSEIF ( I .GE. 2 .AND. I .LE. 6 ) THEN
        GXI=-X(I)+X(I+1)-DIST
        RETURN
      ELSEIF ( I .EQ. 7 ) THEN
        GXI=-X(7)+LENGTH-DIST
        RETURN
      ELSEIF ( I .GE. 8 .AND. I .LE. 170 ) THEN
        J=I-7
        FAC=2.D0*PI*SIN(THETAW(J))
        CJ=0.5D0+COS(LENGTH*FAC)
        DO K=1,6
          CJ=CJ+COS(FAC*X(K+1))
        ENDDO
        CJ=CJ*2.D0/15.D0
        GXI=X(1)-CJ
        RETURN
      ELSE
        J=I-170
        FAC=2.D0*PI*SIN(THETAW(J))
        CJ=0.5D0+COS(LENGTH*FAC)
        DO K=2,7
          CJ=CJ+COS(FAC*X(K))
        ENDDO
        CJ=CJ*2.D0/15.D0
        GXI=CJ+X(1)
        RETURN
      ENDIF
      END
      SUBROUTINE EGRADG(I,X,GRADGI)
      INCLUDE 'O8FUCO.INC'
      INTEGER I
      DOUBLE PRECISION X(*),GRADGI(*)
      DOUBLE PRECISION LENGTH,DIST,PI
      DOUBLE PRECISION THETAW(163)
      COMMON/ANTENNA/LENGTH,DIST,PI,THETAW
      DOUBLE PRECISION FAC
      INTEGER J,K
      IF ( I .EQ. 1 ) RETURN
      CGRES(I)=CGRES(I)+1
      DO J=1,N
        GRADGI(J)=0.D0
      ENDDO
      IF ( I .GE. 2 .AND. I .LE. 6 ) THEN
        GRADGI(I)=-1.D0
        GRADGI(I+1)=1.D0
        RETURN
      ELSEIF ( I .EQ. 7 ) THEN
        GRADGI(7)=-1.D0
        RETURN
      ELSEIF ( I .GE. 8 .AND. I .LE. 170 ) THEN
        J=I-7
        GRADGI(1)=1.D0
        FAC=2.D0*PI*SIN(THETAW(J))
        DO K=2,7
          GRADGI(K)=(2.D0/15.D0)*FAC*SIN(FAC*X(K))
        ENDDO
        RETURN
      ELSE
        J=I-170
        GRADGI(1)=1.D0
        FAC=2.D0*PI*SIN(THETAW(J))
        DO K=2,7
          GRADGI(K)=(2.D0/15.D0)*FAC*(-SIN(FAC*X(K)))
        ENDDO
        RETURN
      ENDIF
      END
