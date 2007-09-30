      SUBROUTINE SETUP0
      INCLUDE 'O8COMM.INC'
      PROU=10
      MEU=20
      COLD=.TRUE.
      SILENT=.FALSE.
      ANALYT=.TRUE.

      RETURN
      END
      SUBROUTINE SETUP
      INCLUDE 'O8COMM.INC'
      TE1=.TRUE.
      RETURN
      END
C   HS109
      BLOCK DATA
      INCLUDE 'O8BLOC.INC'
      INTEGER I,J
      DATA NAME /'HS109'/
      DATA X/NX*0.0D0/
      DATA N/9/ , NH/6/ , NG/20/
      DATA DEL0/1.0D-3/ ,TAU0/1.D2/ ,TAU/.1D0/
      DATA ((GUNIT(I,J),I=1,3),J=7,10)/-1,0,0,-1,0,0,-1,0,0,-1,0,0/,
     F     ((GUNIT(I,J),I=1,3),J=11,19)/1,1,1,1,2,1,1,3,1,1,4,1,1,5,1,
     F                                 1,6,1,1,7,1,1,8,1,1,9,1/,
     F     ((GUNIT(I,J),I=1,3),J=20,26)/1,3,-1,1,4,-1,1,5,-1,1,6,-1,
     F                                  1,7,-1,1,8,-1,1,9,-1/,
     F     ((GUNIT(I,J),I=1,3),J=0,6)/-1,0,0,-1,0,0,-1,0,0,-1,0,0,
     F     -1,0,0,-1,0,0,-1,0,0/
      END
      SUBROUTINE EF(X,FX)
      INCLUDE 'O8FUCO.INC'
      DOUBLE PRECISION X(NX),FX
      ICF=ICF+1
      FX=X(1)*(3.D0+1.D-6*X(1)**2)+X(2)*(2.D0+.522074D-6*X(2)**2)
      RETURN
      END
      SUBROUTINE EGRADF(X,GRADF)
      INCLUDE 'O8FUCO.INC'
      DOUBLE PRECISION X(NX),GRADF(NX)
      INTEGER I
      ICGF=ICGF+1
      GRADF(1)=3.D0+3.D-6*X(1)**2
      GRADF(2)= 2. D0+1.566222D-6*X(2)**2
      DO      100      I=3,9
      GRADF(I)= 0. D0
 100  CONTINUE
      RETURN
      END
      SUBROUTINE EH(I,X,HXI)
      INCLUDE 'O8FUCO.INC'
      INTEGER I
      DOUBLE PRECISION X(NX),A,B,C,HXI
      SAVE A,B,C
      DATA A/50.176D0/
      CRES(I)=CRES(I)+1
      B=DSIN(.25D0)
      C=DCOS(.25D0)
      GOTO (100,200,300,400,500,600),I
  100 CONTINUE
      HXI=X(5)*(X(6)*DSIN(-X(3)-.25D0)+X(7)*DSIN(-X(4)-.25D0)+
     1    2.D0*B*X(5))+A*(400.D0-X(1))
      RETURN
  200 CONTINUE
      HXI=X(6)*(X(5)*DSIN( X(3)-.25D0)+X(7)*DSIN( X(3)-X(4)-.25D0)+
     1    2.D0*B*X(6))+A*(400.D0-X(2))
      RETURN
  300 CONTINUE
      HXI=X(7)*(X(5)*DSIN( X(4)-.25D0)+X(6)*DSIN( X(4)-X(3)-.25D0)+
     1    2.D0*B*X(7))+A*881.779D0
      RETURN
  400 CONTINUE
      HXI=A*(X(8)-200.D0)+ X(5)*((A*.7533D-3-2.D0*C)*X(5)+X(6)*
     1    DCOS(-X(3)-.25D0)+X(7)*DCOS(-X(4)-.25D0))
      RETURN
  500 CONTINUE
      HXI=A*(X(9)-200.D0)+ X(6)*((A*.7533D-3-2.D0*C)*X(6)+X(5)*
     1    DCOS( X(3)-.25D0)+X(7)*DCOS(X(3)-X(4)-.25D0))
      RETURN
  600 CONTINUE
      HXI=22.938D0*A+X(7)*((A*.7533D-3-2.D0*C)*X(7)+X(5)*DCOS(
     1    X(4)-.25D0)+X(6)*DCOS(X(4)-X(3)-.25D0))
      RETURN
      END
      SUBROUTINE EGRADH(I,X,GRADHI)
      INCLUDE 'O8FUCO.INC'
      INTEGER I,J
      DOUBLE PRECISION A,Q,SM,S3,S4,C34,S34,C4,C43,S43,C,C3,B
      DOUBLE PRECISION X(NX),GRADHI(NX)
      SAVE A,Q,SM
      DATA A/50.176D0/, Q/.25D0/ , SM/.7533D-3/
      CGRES(I)=CGRES(I)+1
      B=DSIN(Q)
      C=DCOS(Q)
      DO      50      J=1,9
      GRADHI(J)=0.D0
   50 CONTINUE
      GOTO (100,200,300,400,500,600),I
  100 CONTINUE
      GRADHI(1)=-A
      S3=DSIN(-X(3)-Q)
      S4=DSIN(-X(4)-Q)
      GRADHI(3)=-X(5)*X(6)*DCOS(-X(3)-Q)
      GRADHI(4)=-X(5)*X(7)*DCOS(-X(4)-Q)
      GRADHI(5)=4.D0*B*X(5)+X(6)*S3+X(7)*S4
      GRADHI(6)=X(5)*S3
      GRADHI(7)=X(5)*S4
      RETURN
  200 CONTINUE
      GRADHI(2)=-A
      C34=DCOS(X(3)-X(4)-Q)
      S3=DSIN( X(3)-Q)
      S34=DSIN(X(3)-X(4)-Q)
      GRADHI(3)= X(6)*(X(5)*DCOS( X(3)-Q)+X(7)*C34)
      GRADHI(4)=-X(6)*X(7)*C34
      GRADHI(5)=X(6)*S3
      GRADHI(6)=4.D0*B*X(6)+X(5)*S3+X(7)*S34
      GRADHI(7)=X(6)*S34
      RETURN
  300 CONTINUE
      C43=DCOS(X(4)-X(3)-Q)
      C4=DCOS(X(4)-Q)
      S43=DSIN(X(4)-X(3)-Q)
      S4 =DSIN(X(4)-Q)
      GRADHI(3)=-X(6)*X(7)*C43
      GRADHI(4)= X(7)*(X(5)*C4+X(6)*C43)
      GRADHI(5)=X(7)*S4
      GRADHI(6)=X(7)*S43
      GRADHI(7)=4.D0*B*X(7)+X(5)*S4+X(6)*S43
      RETURN
  400 CONTINUE
      C3 =DCOS(-X(3)-Q)
      C4 =DCOS(-X(4)-Q)
      GRADHI(3)= X(5)*X(6)*DSIN(-X(3)-Q)
      GRADHI(4)= X(5)*X(7)*DSIN(-X(4)-Q)
      GRADHI(5)=X(6)*C3+X(7)*C4+X(5)*(2.D0*A*SM-4.D0*C)
      GRADHI(6)=X(5)*C3
      GRADHI(7)=X(5)*C4
      GRADHI(8)= A
      RETURN
  500 CONTINUE
      S34=DSIN(X(3)-X(4)-Q)*X(6)*X(7)
      C3 =DCOS( X(3)-Q)
      C34=DCOS(X(3)-X(4)-Q)
      GRADHI(3)=-X(5)*X(6)*DSIN( X(3)-Q)-S34
      GRADHI(4)=S34
      GRADHI(5)= X(6)*C3
      GRADHI(6)=X(5)*C3+X(7)*C34+X(6)*(2.D0*A*SM-4.D0*C)
      GRADHI(7)= X(6)*C34
      GRADHI(9)= A
      RETURN
  600 CONTINUE
      C4=DCOS(X(4)-Q)
      C43=DCOS(X(4)-X(3)-Q)
      S43=DSIN(X(4)-X(3)-Q)*X(6)*X(7)
      GRADHI(3)= S43
      GRADHI(4)=-X(5)*X(7)*DSIN( X(4)-Q)-S43
      GRADHI(5)=    X(7)*C4
      GRADHI(6)=    X(7)*C43
      GRADHI(7)=X(5)*C4+X(6)*C43+X(7)*(2.D0*A*SM-4.D0*C)
      RETURN
      END
      SUBROUTINE EG(I,X,GXI)
      INCLUDE 'O8FUCO.INC'
      INTEGER I
      DOUBLE PRECISION X(NX),UG(9),OG(7),GXI
      SAVE UG,OG
      DATA UG/2*0.D0,2*-0.55D0,3*196.D0,2*-400.D0/
      DATA OG/2*.55D0,3*252.D0,2*800.D0/
      IF ( GUNIT(1,I+NH) .EQ. -1 ) CRES(I+NH)=CRES(I+NH)+1
      IF(I .GT. 13)      GOTO 60
      IF(I .GT.  4)      GOTO 50
      GOTO(10,20,30,40),I
 10   CONTINUE
      GXI=2.25D6-X(1)**2-X(8)**2
      RETURN
 20   CONTINUE
      GXI=2.25D6-X(2)**2-X(9)**2
      RETURN
 30   CONTINUE
      GXI=X(4)-X(3)+.55D0
      RETURN
 40   CONTINUE
      GXI=X(3)-X(4)+.55D0
      RETURN
 50   CONTINUE
      GXI=X(I-4)-UG(I-4)
      RETURN
 60   CONTINUE
      GXI=OG(I-13)-X(I-11)
      RETURN
      END
      SUBROUTINE EGRADG(I,X,GRADGI)
      INCLUDE 'O8FUCO.INC'
      INTEGER I,J
      DOUBLE PRECISION X(NX) ,GRADGI(NX)
      CGRES(I+NH)=CGRES(I+NH)+1
      DO     10      J=1,9
      GRADGI(J)=0.D0
  10  CONTINUE
        IF(I .GT. 13)      GOTO 70
        IF(I .GT.  4)      GOTO 60
      GOTO(20,30,40,50),I
   20 CONTINUE
      GRADGI(1)=-2.D0*X(1)
      GRADGI(8)=-2.D0*X(8)
      RETURN
   30 CONTINUE
      GRADGI(2)=-2.D0*X(2)
      GRADGI(9)=-2.D0*X(9)
      RETURN
   40 CONTINUE
      GRADGI(3)=-1.D0
      GRADGI(4)= 1.D0
      RETURN
   50 CONTINUE
      GRADGI(3)= 1.D0
      GRADGI(4)=-1.D0
      RETURN
   60 CONTINUE
      GRADGI(I-4)= 1.D0
      RETURN
   70 CONTINUE
      GRADGI(I-11)=-1.D0
      RETURN
      END
