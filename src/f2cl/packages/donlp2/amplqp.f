      SUBROUTINE SETUP0
      INCLUDE 'O8COMM.INC'
      INTEGER I
      NAME='AMPLEXAM'
      N=5
      NH=2
      NG=1
      DO I=1,5
        X(I)=0.D0
      ENDDO
      DO I=0,3
        GUNIT(1,I)=-1
        GUNIT(2,I)=0
        GUNIT(3,I)=0
      ENDDO
      GCONST(0)=.FALSE.
      DO I=1,3
        GCONST(I)=.TRUE.
      ENDDO
      TAU0=1.D8
      DEL0=1.D0
      TAU=0.1D0
      PROU=10
      MEU=20
C*** SINCE WE USE HIGH ORDER NUMERICAL DIFFERENTIATION :
      ANALYT=.TRUE.
      EPSDIF=0.D0
      COLD=.TRUE.
      SILENT=.FALSE.
      RETURN
      END
      SUBROUTINE SETUP
      INCLUDE 'O8COMM.INC'
      EPSX=1.D-8
      RETURN
      END
      SUBROUTINE EF(X,FX)
      INCLUDE 'O8FUCO.INC'
      DOUBLE PRECISION X(*),FX
      ICF=ICF+1
      FX=
     1   0.5*(0.371136*X(1)*X(1)) + 0.5*(5.14041*X(1)*X(2))
     2   + 0.5*(2.60871*X(1)*
     3   X(3)) + 0.5*(-1.13012*X(1)*X(4))
     4   + 0.5*(-6.05446*X(1)*X(5)) + 0.5*(
     5   5.14041*X(2)*X(1)) + 0.5*(72.1133*X(2)*X(2))
     6   + 0.5*(36.1319*X(2)*X(3))
     7   + 0.5*(-15.6528*X(2)*X(4))
     8   + 0.5*(-83.8572*X(2)*X(5)) + 0.5*(2.60871*
     9   X(3)*X(1)) + 0.5*(36.1319*X(3)*X(2))
     A   + 0.5*(18.4058*X(3)*X(3)) + 0.5*( -7.94363*X(3)*X(4))
     B   + 0.5*(-42.5567*X(3)*X(5)) + 0.5*(-1.13012*X(4)*
     C   X(1)) + 0.5*(-15.6528*X(4)*X(2)) + 0.5*(-7.94363*X(4)*X(3))
     D   + 0.5*( 4.32868*X(4)*X(4)) + 0.5*(23.8574*X(4)*X(5))
     E   + 0.5*(-6.05446*X(5)*X(1))
     F   + 0.5*(-83.8572*X(5)*X(2)) + 0.5*(-42.5567*X(5)*X(3))
     G   + 0.5*(23.8574*  X(5)*X(4)) + 0.5*(132.59*X(5)*X(5))
     H    + 8.61104*X(1) + 14.8321*X(2) +
     I     3.92921*X(3) + 1.43377*X(4) + 5.98614*X(5)
      RETURN
      END
      SUBROUTINE EH(I,X,HXI)
      INCLUDE 'O8FUCO.INC'
      INTEGER I
      DOUBLE PRECISION HXI,X(*)
      CRES(I)=CRES(I)+1
C  S.T. EQ(1):
      IF ( I .EQ. 1 ) THEN
        HXI=
     1   0.302736*X(1) + 0.217135*X(2) + 2.66167*X(3) - 1.54133*X(4)
     2   -  1.12413*X(5) -( -3.50618)
      ELSE
        HXI=
C    S.T. EQ(2):
     1    -2.035*X(1) - 1.23544*X(4) -( -2.39339)
      ENDIF
      RETURN
      END
      SUBROUTINE EG(I,X,GXI)
      INCLUDE 'O8FUCO.INC'
      INTEGER I
      DOUBLE PRECISION X(*),GXI
      CRES(I+NH)=CRES(I+NH)+1
C     S.T. INEQ(1):
      GXI=1.47194*X(1) - 0.312665*X(3)- 1.40314
      RETURN
      END
C*******************************************************************
C     COMPUTES GRADIENT BY HIGH PRECISION NUMERICAL DIFFERENTIATION
C*******************************************************************
C COPYRIGHT = SPELLUCCI
      SUBROUTINE EGRADF(XL,GRADXL)
      INCLUDE 'O8FUCO.INC'
      INCLUDE 'O8CONS.INC'
      DOUBLE PRECISION XL(*),GRADXL(*),FXL(6)
      DOUBLE PRECISION SD1,SD2,SD3,XINCR,ZZ,D1,D2,D3
      INTEGER I
C****************************************************
C     HIGH PRECISION NUMERICAL DIFFERENTIATION
C     BY SIXTH ORDER EXTRAPOLATION
C     USES RICHARDSON-EXTRAPOLATION OF THREE VALUES
C     OF THE SYMMETRIC DIFFERENCE QUOTIENT WITH
C     RATHER LARGE DISCRETIZATION STEPSIZE
C     THE OPTIMAL STEPSIZE DEPENDS ON THE SEVENTH
C     PARTIAL DERIVATIVES OF THE FUNCTION . SINCE THESE
C     ARE INACCESSIBLE, WE REPLACE THEM BY ONE IN THE
C     ESTIMATE OF DELDIF
C****************************************************
      DELDIF=EXP(LOG(EPSMAC)/SEVEN)*TWOM2
      ICGF=ICGF+1
      DO   I=1,N
        ZZ=XL(I)
        XINCR=DELDIF*(ABS(XL(I))+ONE)
        XL(I)=ZZ-XINCR
        CALL EF(XL,FXL(1))
        XL(I)=ZZ+XINCR
        CALL EF(XL,FXL(2))
        XINCR=XINCR+XINCR
        D1=XINCR
        XL(I)=ZZ-XINCR
        CALL EF(XL,FXL(3))
        XL(I)=ZZ+XINCR
        CALL EF(XL,FXL(4))
        XINCR=XINCR+XINCR
        D2=XINCR
        XL(I)=ZZ-XINCR
        CALL EF(XL,FXL(5))
        XL(I)=ZZ+XINCR
        CALL EF(XL,FXL(6))
        XL(I)=ZZ
        D3=XINCR+XINCR
        SD1=(FXL(2)-FXL(1))/D1
        SD2=(FXL(4)-FXL(3))/D2
        SD3=(FXL(6)-FXL(5))/D3
        SD3=SD2-SD3
        SD2=SD1-SD2
        SD3=SD2-SD3
        GRADXL(I)=SD1+P4*SD2+SD3/C45
      ENDDO
      RETURN
      END
C*******************************************************************
      SUBROUTINE EGRADG(J,XL,GRADXL)
      INCLUDE 'O8FUCO.INC'
      INCLUDE 'O8CONS.INC'
      DOUBLE PRECISION XL(*),GRADXL(*),FXL(6)
      DOUBLE PRECISION SD1,SD2,SD3,XINCR,ZZ,D1,D2,D3
      INTEGER I,J
C****************************************************
C     HIGH PRECISION NUMERICAL DIFFERENTIATION
C     BY SIXTH ORDER EXTRAPOLATION
C     USES RICHARDSON-EXTRAPOLATION OF THREE VALUES
C     OF THE SYMMETRIC DIFFERENCE QUOTIENT WITH
C     RATHER LARGE DISCRETIZATION STEPSIZE
C     THE OPTIMAL STEPSIZE DEPENDS ON THE SEVENTH
C     PARTIAL DERIVATIVES OF THE FUNCTION . SINCE THESE
C     ARE INACCESSIBLE, WE REPLACE THEM BY ONE IN THE
C     ESTIMATE OF DELDIF
C****************************************************
      DELDIF=EXP(LOG(EPSMAC)/SEVEN)*TWOM2
      CGRES(J+NH)=CGRES(J+NH)+1
      DO   I=1,N
        ZZ=XL(I)
        XINCR=DELDIF*(ABS(XL(I))+ONE)
        XL(I)=ZZ-XINCR
        CALL EG(J,XL,FXL(1))
        XL(I)=ZZ+XINCR
        CALL EG(J,XL,FXL(2))
        XINCR=XINCR+XINCR
        D1=XINCR
        XL(I)=ZZ-XINCR
        CALL EG(J,XL,FXL(3))
        XL(I)=ZZ+XINCR
        CALL EG(J,XL,FXL(4))
        XINCR=XINCR+XINCR
        D2=XINCR
        XL(I)=ZZ-XINCR
        CALL EG(J,XL,FXL(5))
        XL(I)=ZZ+XINCR
        CALL EG(J,XL,FXL(6))
        XL(I)=ZZ
        D3=XINCR+XINCR
        SD1=(FXL(2)-FXL(1))/D1
        SD2=(FXL(4)-FXL(3))/D2
        SD3=(FXL(6)-FXL(5))/D3
        SD3=SD2-SD3
        SD2=SD1-SD2
        SD3=SD2-SD3
        GRADXL(I)=SD1+P4*SD2+SD3/C45
      ENDDO
      RETURN
      END
C*******************************************************************
      SUBROUTINE EGRADH(J,XL,GRADXL)
      INCLUDE 'O8FUCO.INC'
      INCLUDE 'O8CONS.INC'
      DOUBLE PRECISION XL(*),GRADXL(*),FXL(6)
      DOUBLE PRECISION SD1,SD2,SD3,XINCR,ZZ,D1,D2,D3
      INTEGER I,J
C****************************************************
C     HIGH PRECISION NUMERICAL DIFFERENTIATION
C     BY SIXTH ORDER EXTRAPOLATION
C     USES RICHARDSON-EXTRAPOLATION OF THREE VALUES
C     OF THE SYMMETRIC DIFFERENCE QUOTIENT WITH
C     RATHER LARGE DISCRETIZATION STEPSIZE
C     THE OPTIMAL STEPSIZE DEPENDS ON THE SEVENTH
C     PARTIAL DERIVATIVES OF THE FUNCTION . SINCE THESE
C     ARE INACCESSIBLE, WE REPLACE THEM BY ONE IN THE
C     ESTIMATE OF DELDIF
C****************************************************
      DELDIF=EXP(LOG(EPSMAC)/SEVEN)*TWOM2
      CGRES(J)=CGRES(J)+1
      DO   I=1,N
        ZZ=XL(I)
        XINCR=DELDIF*(ABS(XL(I))+ONE)
        XL(I)=ZZ-XINCR
        CALL EH(J,XL,FXL(1))
        XL(I)=ZZ+XINCR
        CALL EH(J,XL,FXL(2))
        XINCR=XINCR+XINCR
        D1=XINCR
        XL(I)=ZZ-XINCR
        CALL EH(J,XL,FXL(3))
        XL(I)=ZZ+XINCR
        CALL EH(J,XL,FXL(4))
        XINCR=XINCR+XINCR
        D2=XINCR
        XL(I)=ZZ-XINCR
        CALL EH(J,XL,FXL(5))
        XL(I)=ZZ+XINCR
        CALL EH(J,XL,FXL(6))
        XL(I)=ZZ
        D3=XINCR+XINCR
        SD1=(FXL(2)-FXL(1))/D1
        SD2=(FXL(4)-FXL(3))/D2
        SD3=(FXL(6)-FXL(5))/D3
        SD3=SD2-SD3
        SD2=SD1-SD2
        SD3=SD2-SD3
        GRADXL(I)=SD1+P4*SD2+SD3/C45
      ENDDO
      RETURN
      END


