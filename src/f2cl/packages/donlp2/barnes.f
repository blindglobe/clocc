      SUBROUTINE SETUP
      RETURN
      END
C
C   HS85
      SUBROUTINE EF(X,FX)
      INCLUDE 'O8FUCO.INC'
      DOUBLE PRECISION X(NX),FX,C(17),Y(17)
      ICF=ICF+1
      CALL CY(X,C,Y)
      FX=-5.843D-7*Y(17)+1.17D-4*Y(14)+2.358D-5*Y(13)+1.502D-6*Y(16)+
     1   3.21D-2*Y(12)+4.324D-3*Y(5)+1.D-4*C(15)/C(16)+37.48D0*Y(2)
     2    /C(12) +.1365D0
      RETURN
      END
      SUBROUTINE EGRADF(X,GRADF)
      INCLUDE 'O8FUCO.INC'
      INTEGER J
      DOUBLE PRECISION X(NX),GRADF(NX),C(17),Y(17),GC(5,17),GY(5,17)
      ICGF=ICGF+1
      CALL GCY(X,C,Y,GC,GY)
      DO      100      J=1,5
      GRADF(J)=-5.843D-7*GY(J,17)+1.17D-4*GY(J,14)+2.358D-5*GY(J,13)
     1         +1.502D-6*GY(J,16)+3.21D-2*GY(J,12)+4.3240D-3*GY(J,5)
     2         +1.D-4  /C(16)*GC(J,15)- 1.D-4 *C(15)/C(16)**2*GC(J,16)
     3         +37.48D0/C(12)*GY(J, 2)-37.48D0*Y( 2)/C(12)**2*GC(J,12)
 100  CONTINUE
      RETURN
      END
      SUBROUTINE EH(I,X,HXI)
      INCLUDE 'O8FUCO.INC'
      INTEGER I
      DOUBLE PRECISION HXI,X(NX)
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
      INTEGER I,K
      DOUBLE PRECISION X(NX),GXI,C(17),Y(17),UG(5),OG(5),A(17),B(17)
      SAVE UG,OG,A,B
      DATA UG/704.4148D0,68.6D0,0.D0,193.D0,25.D0/
      DATA OG/906.3855D0,288.88D0,134.75D0,287.0966D0,84.1988D0/
      DATA A /213.1D0,17.505D0,11.275D0,214.228D0,7.458D0,.961D0,
     1        1.612D0,.146D0,107.99D0,922.693D0,926.832D0,18.766D0,1072.
     2        163D0,8961.448D0,.063D0,71084.33D0,2802713.D0/
      DATA B /405.23D0,1053.6667D0,35.03D0,665.585D0,584.463D0,
     1        265.916D0,7.046D0,.222D0,273.366D0,1286.105D0,1444.046D0,
     2        537.141D0,3247.039D0,26844.086D0,.386D0,1.4D5,1.2146108D7/
      IF ( GUNIT(1,I+NH) .EQ. -1 )CRES(I+NH)=CRES(I+NH)+1
      IF(I .GT. 38)      GOTO 600
      IF(I .GE.  2)      GOTO 100
      GXI=1.5D0*X(2)-X(3)
      RETURN
 100  CONTINUE
      CALL CY(X,C,Y)
      IF(I .GT. 4)      GOTO 500
      K=I-1
      GOTO (200,300,400),K
  200 CONTINUE
      GXI=Y(4)-.28D0/.72D0*Y(5)
      RETURN
  300 CONTINUE
      GXI=21.D0-3.496D3*Y(2)/C(12)
      RETURN
  400 CONTINUE
      GXI=6.2212D4/C(17)-Y(1)-110.6D0
      RETURN
  500 CONTINUE
      IF(I .LE. 21)      GXI=Y(I-4)-A(I-4)
      IF(I .GT. 21)      GXI=B(I-21)-Y(I-21)
      RETURN
  600 CONTINUE
      IF(I .LE. 43)      GXI=X(I-38)-UG(I-38)
      IF(I .GT. 43)      GXI=OG(I-43)-X(I-43)
      RETURN
      END
      SUBROUTINE EGRADG(I,X,GRADGI)
      INCLUDE 'O8FUCO.INC'
      INTEGER I,K,J
      DOUBLE PRECISION X(NX),GRADGI(NX),C(17),Y(17),GC(5,17),GY(5,17)
      IF ( GUNIT(1,I+NH) .EQ. 1 )RETURN
      CGRES(I+NH)=CGRES(I+NH)+1
      IF( I .GT. 38)      GOTO 600
      IF( I .GE.  2)      GOTO 100
      GRADGI(1)=0.D0
      GRADGI(2)=1.5D0
      GRADGI(3)=-1.D0
      GRADGI(4)= 0.D0
      GRADGI(5)= 0.D0
      RETURN
 100  CONTINUE
       CALL GCY(X,C,Y,GC,GY)
      IF(I .GT. 4)      GOTO 500
      K=I-1
      GOTO (200,300,400),K
 200  CONTINUE
      DO     250      J=1,5
      GRADGI(J)=GY(J,4)-.28D0/.72D0*GY(J,5)
  250 CONTINUE
      RETURN
  300 CONTINUE
      DO     350      J=1,5
      GRADGI(J)=-3.496D3*(GY(J,2)-Y(2)/C(12)*GC(J,12))/C(12)
  350 CONTINUE
      RETURN
  400 CONTINUE
      DO      450      J=1,5
      GRADGI(J)=-GY(J,1)-6.2212D4/C(17)**2*GC(J,17)
  450 CONTINUE
      RETURN
  500 CONTINUE
      IF( I .GT. 21)      GOTO 560
      DO      520      J=1,5
      GRADGI(J)= GY(J,I-4)
  520 CONTINUE
      RETURN
  560 CONTINUE
      DO      580      J=1,5
      GRADGI(J)=-GY(J,I-21)
  580 CONTINUE
      RETURN
  600 CONTINUE
      DO      650      J=1,5
      GRADGI(J)=0.D0
  650 CONTINUE
      IF( I .LE. 43)      GRADGI(I-38)=1.D0
      IF( I .GT. 43)      GRADGI(I-43)=-1.D0
      RETURN
      END
      SUBROUTINE CY(X,C,Y)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'O8PARA.INC'
      DOUBLE PRECISION X(NX),C(17),Y(17)
      Y(1)=X(2)+X(3)+41.6D0
      C(1)=.024D0*X(4)-4.62D0
      Y(2)=12.5D0/C(1)+12.D0
      C(2)=(3.535D-4*X(1)+.5311D0+8.705D-2*Y(2))*X(1)
      C(3)=(5.2D-2+2.377D-3*Y(2))*X(1)+78.D0
      Y(3)=C(2)/C(3)
      Y(4)=19.D0*Y(3)
      C(4)=(4.782D-2+.1956D0*(X(1)-Y(3))/X(2))*(X(1)-Y(3))
     1     +.6376D0*Y(4)+1.594D0*Y(3)
       C(5)=1.D2*X(2)
      C(6)=X(1)-Y(3)-Y(4)
      C(7)=.95D0-C(4)/C(5)
      Y(5)=C(6)*C(7)
      Y(6)=X(1)-Y(5)-Y(4)-Y(3)
      C(8)=(Y(5)+Y(4))*.995D0
      Y(7)=C(8)/Y(1)
      Y(8)=C(8)/3798.D0
      C(9)=Y(7)-.0663D0*Y(7)/Y(8)-.3153D0
      Y(9)=96.82D0/C(9)+.321D0*Y(1)
      Y(10)=1.29D0*Y(5)+1.258D0*Y(4)+2.29D0*Y(3)+1.71D0*Y(6)
      Y(11)=1.71D0*X(1)-.452D0*Y(4)+.58D0*Y(3)
      C(10)=12.3D0/752.3D0
      C(11)=1.74125D0*Y(2)*X(1)
      C(12)= .995D0*Y(10)+1998.D0
      Y(12)=C(10)*X(1)+C(11)/C(12)
      Y(13)=C(12)-1.75D0*Y(2)
      Y(14)=3623.D0+64.4D0*X(2)+58.4D0*X(3)+146312.D0/(Y(9)+X(5))
      C(13)=.995D0*Y(10)+60.8D0*X(2)+48.D0*X(4)-.1121D0*Y(14)-5095.D0
      Y(15)=Y(13)/C(13)
      Y(16)=1.48D5-3.31D5*Y(15)+4.D1*Y(13)-6.1D1*Y(15)*Y(13)
      C(14)=2324D0*Y(10)-2.874D7*Y(2)
      Y(17)=1.413D7-1328.D0*Y(10)-531.D0*Y(11)+C(14)/C(12)
      C(15)=Y(13)*(1.D0/Y(15)-1.D0/.52D0)
      C(16)=1.104D0-.72D0*Y(15)
      C(17)=Y(9)+X(5)
      RETURN
      END
      SUBROUTINE GCY(X,C,Y,GC,GY)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'O8PARA.INC'
      INTEGER J
      DOUBLE PRECISION X(NX),C(17),Y(17),GC(5,17),GY(5,17)
      CALL CY(X,C,Y)
      DO      100      J=1,5
      GY(J,1)=0.D0
      GC(J,1)=0.D0
      GY(J,2)=0.D0
      GC(J,5)=0.D0
      GC(J,10)=0.D0
  100 CONTINUE
      GY(2,1)=1.D0
      GY(3,1)=1.D0
      GC(4,1)=0.024D0
      GY(4,2)=-.3D0/C(1)**2
      GC(2,5)=1.D2
      DO      200      J=1,5
      GC(J,2)=.08705D0*X(1)*GY(J,2)
      GC(J,3)=2.377D-3*X(1)*GY(J,2)
  200 CONTINUE
      GC(1,2)=GC(1,2)+8.705D-2*Y(2)+.5311D0+7.07D-4*X(1)
      GC(1,3)=GC(1,3)+5.2D-2+2.377D-3*Y(2)
      DO      300      J=1,5
      GY(J,3)=GC(J,2)/C(3)-C(2)/C(3)**2*GC(J,3)
      GY(J,4)=19.D0*GY(J,3)
      GC(J,4)=-(4.782D-2+.3912D0*(X(1)-Y(3))/X(2))*GY(J,3)+
     1          .6376D0*GY(J,4)+1.594D0*GY(J,3)
      GC(J,6)=-GY(J,3)-GY(J,4)
  300 CONTINUE
      GC(1,6)=GC(1,6)+1.D0
      GC(1,4)=GC(1,4)+4.782D-2+.3912D0*(X(1)-Y(3))/X(2)
      GC(2,4)=GC(2,4)-.1956D0*((X(1)-Y(3))/X(2))**2
      DO      400      J=1,5
      GC(J,7)=-GC(J,4)/C(5)+C(4)/C(5)**2*GC(J,5)
      GY(J,5)= C(7)*GC(J,6)+C(6)*GC(J,7)
      GY(J,6)=-(GY(J,5)+GY(J,4)+GY(J,3))
      GC(J,8)=.995D0*(GY(J,5)+GY(J,4))
      GY(J,7)=GC(J,8)/Y(1)-C(8)/Y(1)**2*GY(J,1)
      GY(J,8)=GC(J,8)/3798.D0
      GC(J,9)=GY(J,7)*(1.D0-6.63D-2/Y(8))+6.63D-2*Y(7)/Y(8)**2*GY(J,8)
      GY(J,9)=.321D0*GY(J,1)-96.82D0/C(9)**2*GC(J,9)
      GY(J,11)=-.452D0*GY(J,4)+.58D0*GY(J,3)
      GC(J,11)=1.74125D0*X(1)*GY(J,2)
  400 CONTINUE
      GY(1,6)=GY(1,6)+1.D0
      GY(1,11)=GY(1,11)+1.71D0
      GC(1,11)=GC(1,11)+1.74125D0*Y(2)
      DO      500      J=1,5
      GY(J,10)=1.29D0*GY(J,5)+1.258D0*GY(J,4)+2.29D0*GY(J,3)+1.71D0
     1         *GY(J,6)
       GC(J,12)=.995D0*GY(J,10)
       GY(J,12)=GC(J,11)/C(12)-C(11)/C(12)**2*GC(J,12)
      GY(J,13)=GC(J,12)-1.75D0*GY(J,2)
      GC(J,17)=GY(J,9)
      GY(J,14)=-1.46312D5/(Y(9)+X(5))**2*GY(J,9)
  500 CONTINUE
      GY(1,12)=GY(1,12)+C(10)
      GY(2,14)=GY(2,14)+64.4D0
      GY(3,14)=GY(3,14)+58.4D0
      GY(5,14)=GY(5,14)-1.46312D5/(Y(9)+X(5))**2
      DO 510 J=1,5
      GC(J,13)=.995D0*GY(J,10)-.1121D0*GY(J,14)
  510 CONTINUE
      GC(2,13)=GC(2,13)+60.8D0
      GC(4,13)=GC(4,13)+48.0D0
      GC(5,17)=GC(5,17)+ 1.0D0
      DO      600      J=1,5
      GY(J,15)=GY(J,13)/C(13)-Y(13)/C(13)**2*GC(J,13)
      GY(J,16)=-3.31D5*GY(J,15)+4.D1*GY(J,13)-6.1D1*(GY(J,15)
     1       *Y(13)+Y(15)*GY(J,13))
      GC(J,14)=2.324D3*GY(J,10)-2.874D7*GY(J,2)
      GY(J,17)=-1.328D3*GY(J,10)-5.31D2*GY(J,11)+GC(J,14)/C(12)
     1         -C(14)/C(12)**2*GC(J,12)
      GC(J,15)=GY(J,13)*(1.D0/Y(15)-1.D0/.52D0)-Y(13)/Y(15)**2*GY(J,15)
      GC(J,16)=-.72D0*GY(J,15)
  600 CONTINUE
      RETURN
      END
      BLOCK DATA
      INCLUDE 'O8BLOC.INC'
      INTEGER I,J
      DATA NAME/'HS85ORIG'/
      DATA (X(I),I=1,5)/9.D2,8.D1,1.15D2,2.67D2,2.7D1/
      DATA N/ 5/ , NH/0/ , NG/48/
      DATA DEL0/1.D0/ ,TAU0/1.D0/ ,TAU/.1D0/
      DATA ((GUNIT(I,J),I=1,3),J=0,48)
     F     /-1,0,0,-1,0,0,-1,0,0,-1,0,0,
     F     -1,0,0,-1,0,0,-1,0,0,-1,0,0,-1,0,0,
     F     -1,0,0,-1,0,0,-1,0,0,-1,0,0,-1,0,0,
     F     -1,0,0,-1,0,0,-1,0,0,-1,0,0,-1,0,0,
     F     -1,0,0,-1,0,0,-1,0,0,-1,0,0,-1,0,0,
     F     -1,0,0,-1,0,0,-1,0,0,-1,0,0,-1,0,0,
     F     -1,0,0,-1,0,0,-1,0,0,-1,0,0,-1,0,0,
     F     -1,0,0,-1,0,0,-1,0,0,-1,0,0,-1,0,0,
     F      1,1,1, 1,2,1, 1,3,1, 1,4,1, 1,5,1,
     F     1,1,-1,1,2,-1,1,3,-1,1,4,-1,1,5,-1/
      END
      SUBROUTINE SETUP0
      INCLUDE 'O8COMM.INC'
C   NAME IS IDENT OF THE EXAMPLE/USER AND CAN BE SET AT USERS WILL
C   THE FIRST CHARACTER MUST BE ALPHABETIC.  40 CHARACTERS MAXIMUM
C   X IS INITIAL GUESS AND ALSO HOLDS THE CURRENT SOLUTION
C   PROBLEM DIMENSION N=DIM(X), NH=DIM(H), NG=DIM(G)
      SILENT=.FALSE.
      ANALYT=.TRUE.
      EPSDIF=0.D0
      PROU=10
      MEU=20
C   DEL0 AND TAU0: SEE BELOW
      RETURN
      END


