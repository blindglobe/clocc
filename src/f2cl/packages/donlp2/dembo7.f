      SUBROUTINE SETUP0
      INCLUDE 'O8COMM.INC'
      ANALYT=.TRUE.
      EPSDIF=0.D0
      PROU=10
      MEU=20
      NRESET=N
      SILENT=.FALSE.
      RETURN
      END
      SUBROUTINE SETUP
      RETURN
      END
C   DEMBO 7 EXAMPLE
      BLOCK DATA
      INCLUDE 'O8BLOC.INC'
      INTEGER I,J
      DATA NAME/'DEMBO7'/
      DATA (X(I),I=1,16)/
     1   .8D0,.83D0,.85D0,.87D0,.9D0,.1D0,.12D0,.19D0,0.25D0,
     2   .29D0,512.D-2,13.1D-2,71.8D-2,640.D-2,650.D-2,5.7D-2/
      DATA N/  16/ , NH/0/ , NG/51/
      DATA DEL0/0.005D0/ ,TAU0/.009D0/ ,TAU/.1D0/
      DATA (GUNIT(I,0),I=1,3)/-1,0,0/,(GUNIT(1,I),I=1,32)/32*1/,
     F     (GUNIT(2,I),I=1,32)/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,
     F     16,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16/,(GUNIT(3,I),I=
     F     1,32)/16*1,16*-1/,(GUNIT(1,I),I=33,51)/19*-1/,
     F     ((GUNIT(I,J),I=2,3),J=33,51)/38*0/
      END
      SUBROUTINE EF(X,FX)
      INCLUDE 'O8FUCO.INC'
      INTEGER I
      DOUBLE PRECISION FX,X(*),C(10)
      SAVE C
      DATA C/4*1.262626D0,1.262626D0,4*-1.231060D0,-1.231060D0/
      ICF=ICF+1
      FX=0.D0
      DO      100      I=1,5
      FX=FX+C(I)*X(I+11)+C(I+5)*X(I)*X(I+11)
 100  CONTINUE
      RETURN
      END
      SUBROUTINE EGRADF(X,GRADF)
      INCLUDE 'O8FUCO.INC'
      DOUBLE PRECISION X(*),GRADF(*),C(10)
      INTEGER J
      SAVE C
      DATA C/4*1.262626D0,1.262626D0,4*-1.231060D0,-1.231060D0/
      ICGF=ICGF+1
      DO      10       J=1,NX
      GRADF(J)=0.D0
   10 CONTINUE
      DO     100      J=1,5
      GRADF(J)=C(J+5)*X(J+11)
  100 CONTINUE
      DO      200      J=12,16
      GRADF(J)=C(J-11)+C(J-6)*X(J-11)
  200 CONTINUE
      RETURN
      END
      SUBROUTINE EH(I,X,HXI)
      INCLUDE 'O8FUCO.INC'
      INTEGER I
      DOUBLE PRECISION HXI,X(NX)
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
      INTEGER I,I1,II,K
      DOUBLE PRECISION GXI,GX,F1,F2
      DOUBLE PRECISION X(*),UG(16),OG(16),C(51),D(16)
      DOUBLE PRECISION X12,X13,X14,
     *     X15,X16
      SAVE D,C,UG,OG
      DATA D/16*1.D0/
      DATA UG/4*0.1D0,.9D0,1.D-4,4*.1D0,1.D-2,1.D-8,1.D-2,2*5.D0,1.D-8/
      DATA OG/4*.9D0,1.D0,0.1D0,4*.9D0,1.D1,2*5.D0,2*1.D1,5.D0/
      DATA C/0.03475D0,0.975D0,-0.975D-2,0.03475D0,0.975D0,-0.975D-2,
     1       0.03475D0,0.975D0,-0.975D-2,0.03475D0,0.975D0,-0.975D-2,
     2       0.03475D0,0.975D0,-0.975D-2,
     3       2*1.D0,-1.D0,1.D0,2*0.2D-2,2*-0.2D-2,1.D0,
     4       2*0.2D-2,1.D0,2*-0.2D-2,
     5       2*1.D0,500.D0,-500.D0,-1.D0,
     6       2*1.D0,500.D0,-1.D0,-500.D0,
     7       0.9D0,0.2D-2,-0.2D-2,0.2D-2,-0.2D-2,7*1.D0/
      IF ( I .GE. 33 )    GOTO 20
      IF ( I .GE. 17 )      GOTO 10
      GXI=(X(I)-UG(I))*D(I)
      RETURN
  10  CONTINUE
      GXI=(-X(I-16)+OG(I-16))*D(I-16)
      RETURN
  20  CONTINUE
      CRES(I+NH)=CRES(I+NH)+1
      I1=I-32
      GOTO(100,100,100,100,100,600,700,800,900,1000,
     1     1100,1200,1300,1400,1400,1400,1400,1400,1400) I1
  100 CONTINUE
      II=(I1-1)*3
      GX=C(II+1)*X(I1)/X(I1+5)+C(II+2)*X(I1)+C(II+3)*X(I1)**2/X(I1+5)
      GOTO 2000
  600 CONTINUE
      F1=X(6)/X(7)
      F2=X(12)/X(11)/X(7)
      GX=C(16)*F1 + C(17)*X(1)*F2 + C(18)*X(6)*F2
      GOTO 2000
  700 CONTINUE
      X12=X(12)*1.D2
      X13=X(13)*1.D2
      GX=C(19)*X(7)/X(8)+C(20)*X(7)*X12/X(8) + C(21)*X(2)*X13/X(8)
     1    + C(22)*X13 + C(23)*X(1)*X12/X(8)
      GOTO 2000
  800 CONTINUE
      X13=X(13)*1.D2
      X14=X(14)*1.D2
      GX=C(24)*X(8)+C(25)*X(8)*X13+C(26)*X(3)*X14+
     1   C(27)*X(9)+C(28)*X(2)*X13+C(29)*X(9)*X14
      GOTO 2000
  900 CONTINUE
      X14=X(14)*1.D2
      X15=X(15)*1.D2
      GX=C(30)*X(9)/X(3)+C(31)*X(4)*X15/X(3)/X14+C(32)*X(10)/X(3)
     1  /X14+C(33)*X(9)/X(3)/X14+C(34)*X(8)*X15/X(3)/X14
      GOTO 2000
 1000 CONTINUE
      X15=X(15)*1.D2
      X16=X(16)*1.D2
      GX=C(35)*X(5)/X(4)*X16/X15+C(36)*X(10)/X(4)+C(37)/X15
     1  +C(38)*X16/X15+C(39)*X(10)/X15/X(4)
      GOTO 2000
 1100 CONTINUE
      X16=X(16)*1.D2
      GX=C(40)/X(4)+C(41)*X16+C(42)*X(5)/X(4)*X16
      GOTO 2000
 1200 CONTINUE
      GX=(C(43)*X(11)+C(44)*X(12))*1.D2
      GOTO 2000
 1300 CONTINUE
      GX=C(45)*X(12)/X(11)
      GOTO 2000
 1400 CONTINUE
      K=IABS(I1-19)
      IF(K .EQ. 1)      K=10
      IF(K .EQ. 0)      K= 9
      GX=C(I1+32)*X(K-1)/X(K)
 2000 CONTINUE
      GXI=1.D0-GX
      RETURN
      END
      SUBROUTINE EGRADG(I,X,GRADGI)
      INCLUDE 'O8FUCO.INC'
      DOUBLE PRECISION X(NX),GRADGI(NX),C(51),D(16)
      DOUBLE PRECISION X3,X4,X8,X12,X13,X14,
     *   X15,X16,X142,X152
      INTEGER I,K,I1,II
      SAVE C,D
      DATA D/16*1.D0/
      DATA C/0.03475D0,0.975D0,-0.975D-2,0.03475D0,0.975D0,-0.975D-2,
     1       0.03475D0,0.975D0,-0.975D-2,0.03475D0,0.975D0,-0.975D-2,
     2       0.03475D0,0.975D0,-0.975D-2,
     3       2*1.D0,-1.D0,1.D0,2*0.2D-2,2*-0.2D-2,1.D0,
     4       2*0.2D-2,1.D0,2*-0.2D-2,
     5       2*1.D0,500.D0,-500.D0,-1.D0,
     6       2*1.D0,500.D0,-1.D0,-500.D0,
     7       0.9D0,0.2D-2,-0.2D-2,0.2D-2,-0.2D-2,7*1.D0/
      IF(I .GE. 33 )      GOTO 20
      RETURN
  20  CONTINUE
      DO      5      K=1,NX
      GRADGI(K)=0.D0
  5   CONTINUE
      CGRES(I+NH)=CGRES(I+NH)+1
      I1=I-32
      GOTO(100,100,100,100,100,600,700,800,900,1000,
     1     1100,1200,1300,1400,1400,1400,1400,1400,1400),I1
  100 CONTINUE
      II=(I1-1)*3
      GRADGI(I1) =C(II+1)/X(I1+5) + C(II+2)+2.D0*C(II+3)*X(I1)/X(I1+5)
      GRADGI(I1+5)=(-C(II+1)*X(I1)-C(II+3)*X(I1)**2)/X(I1+5)**2
      GOTO 2000
  600 CONTINUE
      GRADGI(1)=C(17)*X(12)/X(7)/X(11)
      GRADGI(6)=C(16)/X(7)+C(18)*X(12)/X(7)/X(11)
      GRADGI(7)=-C(16)*X(6)/X(7)**2-C(17)*X(1)/X(7)**2/X(11)*X(12)-
     1          C(18)*X(6)*X(12)/X(7)**2/X(11)
      GRADGI(11)=-C(17)*X(1)/X(7)/X(11)**2*X(12)-C(18)*X(6)/X(7)
     1           /X(11)**2*X(12)
      GRADGI(12)=C(17)*X(1)/X(7)/X(11)+C(18)*X(6)/X(7)/X(11)
      GOTO 2000
  700 CONTINUE
      X12=X(12)*1.D2
      X13=X(13)*1.D2
      GRADGI(1)= C(23)*X12/X(8)
      GRADGI(2)= C(21)*X13/X(8)
      GRADGI(7)=C(19)/X(8) + C(20)*X12/X(8)
      X8=X(8)**2
      GRADGI(8)=-C(19)*X(7)/X8-C(20)*X(7)*X12/X8-C(21)*X(2)*X13/X8
     1          - C(23)*X(1)*X12/X8
      GRADGI(12)=( C(20)*X(7)/X(8) + C(23)*X(1)/X(8))*1.D2
      GRADGI(13)= (C(21)*X(2)/X(8)+C(22))*1.D2
      GOTO 2000
  800 CONTINUE
      X13=X(13)*1.D2
      X14=X(14)*1.D2
      GRADGI(2)=C(28)*X13
      GRADGI(3)=C(26)*X14
      GRADGI(8)=C(24)+   C(25)*X13
      GRADGI(9)=C(27)+   C(29)*X14
      GRADGI(13)=(C(25)*X(8)   +   C(28)*X(2))*1.D2
      GRADGI(14)=(C(26)*X(3)   +   C(29)*X(9))*1.D2
      GOTO 2000
  900 CONTINUE
      X14=X(14)*1.D2
      X15=X(15)*1.D2
      X3=X(3)**2
      GRADGI(3)=-C(30)*X(9)/X3-C(31)*X(4)*X15/X3/X14-C(32)*X(10)/X3
     1  /X14-C(33)*X(9)/X3/X14-C(34)*X(8)*X15/X3/X14
      GRADGI(4)=C(31)*X15/X(3)/X14
      GRADGI(8)=C(34)*X15/X(3)/X14
      GRADGI(9)=C(30)/X(3)+C(33)  /X(3)/X14
      GRADGI(10)=C(32)/X(3)/X14
      X142=X14**2
      GRADGI(14)=(-C(31)*X(4)*X15/X(3)/X142-C(32)*X(10)/X(3)
     1  /X142-C(33)*X(9)/X(3)/X142-C(34)*X(8)*X15/X(3)/X142)*1.D2
      GRADGI(15)=(C(31)*X(4)/X(3)/X14+C(34)*X(8)/X(3)/X14)*1.D2
      GOTO 2000
 1000 CONTINUE
      X15=X(15)*1.D2
      X4=X(4)**2
      X16=X(16)*1.D2
      GRADGI(4)=-C(35)*X(5)/X4*X16/X15-C(36)*X(10)/X4
     1          -C(39)*X(10)/X15/X4
      GRADGI(5)=C(35)/X(4)*X16/X15
      GRADGI(10)=C(36)/X(4)+C(39)/X15/X(4)
      X152=X15**2
      GRADGI(15)=(-C(35)*X(5)/X(4)*X16/X152-C(37)/X152
     1           -C(38)*X16/X152-C(39)*X(10)/X152/X(4))*1.D2
      GRADGI(16)=(C(35)*X(5)/X(4)/X15+C(38)/X15)*1.D2
      GOTO 2000
 1100 CONTINUE
      X4=X(4)**2
      X16=X(16)*1.D2
      GRADGI(4)=-C(40)/X4-C(42)*X(5)/X4*X16
      GRADGI(5)= C(42)/X(4)*X16
      GRADGI(16)=( C(41) + C(42)/X(4)*X(5))*1.D2
      GOTO 2000
 1200 CONTINUE
      GRADGI(11)=C(43)*1.D2
      GRADGI(12)=C(44)*1.D2
      GOTO 2000
 1300 CONTINUE
      GRADGI(11)=-C(45)*X(12)/X(11)**2
      GRADGI(12)= C(45)/X(11)
      GOTO 2000
 1400 CONTINUE
      K=IABS(I1-19)
      IF(K .EQ. 1)      K=10
      IF(K .EQ. 0)      K= 9
      GRADGI(K)=-C(I1+32)*X(K-1)/X(K)**2
      GRADGI(K-1)= C(I1+32)/X(K)
 2000 CONTINUE
      DO      2100      K=1,NX
      GRADGI(K)=-GRADGI(K)
 2100 CONTINUE
      RETURN
      END


