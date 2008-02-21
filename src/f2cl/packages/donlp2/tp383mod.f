      SUBROUTINE SETUP0
      INCLUDE 'O8COMM.INC'
      INTEGER I
c# s383.mod	OLR2-AN-14-29
c# Original AMPL coding by Elena Bobrovnikova (summer 1996 at Bell Labs).
c
c# Ref.: K. Schittkowski, More Test Examples for Nonlinear Programming Codes.
c# Lecture Notes in Economics and Mathematical Systems, v. 282,
c# Springer-Verlag, New York, 1987, p. 202.
c
c# Number of variables:  14
c# Number of constraints:  29
c# Objective separable convex
c# Linear constraints
c
Cset I := 1 .. 14;
C
Cparam a{I};
Cparam c{I};
C
Cvar x{I} >= 0 := 0.01;
      NAME='TP383MOD'
      N=14
      NH=1
      NG=28
      TAU=0.1D0
      TAU0=1.D0
      DEL0=1.D0
      DO I=1,14
        X(I)=0.01D0
      ENDDO
      SILENT=.FALSE.
      COLD=.TRUE.
      ANALYT=.TRUE.
      EPSDIF=1.D-8
      PROU=10
      MEU=20
      DO I=0,1
        GUNIT(1,I)=-1
        GUNIT(2,I)=0
        GUNIT(3,I)=0
      ENDDO
      GCONST(0)=.FALSE.
      DO I=1,29
        GCONST(I)=.TRUE.
      ENDDO
      DO I=2,29
        GUNIT(1,I)=1
        IF ( I-1 .LE. 14 ) THEN
          GUNIT(1,I)=1
          GUNIT(3,I)=1
          GUNIT(2,I)=I-1
        ELSE
          GUNIT(1,I)=1
          GUNIT(2,I)=I-15
          GUNIT(3,I)=-1
        ENDIF
      ENDDO
      RETURN
      END
      SUBROUTINE SETUP
      RETURN
      END
      SUBROUTINE EF(X,FX)
      INCLUDE 'O8FUCO.INC'
      DOUBLE PRECISION X(*),FX
      INTEGER I
      DOUBLE PRECISION A(14),SUM
      SAVE A
      DATA (A(I),A(I+7),I=1,7)/
cparam a :=
c      1    12842.275       8    1267
c      2      634.25        9     760.05
c      3      634.25       10     633.25
c      4      634.125      11    1266.25
c      5     1268          12     632.875
c      6      633.875      13     394.46
c      7      633.75       14     940.838   ;
     1     12842.275D0,    1267.D0,
     2     634.25D0,        760.05D0,
     3      634.25D0,       633.25D0,
     4      634.125D0,      1266.25D0,
     5     1268.D0,           632.875D0,
     7     633.875D0,        394.46D0,
     8      633.75D0,        940.838D0 /
cminimize Obj:
c         sum {i in I} a[i] / x[i];
      ICF=ICF+1
      SUM=0.D0
      DO I=1,14
        SUM=SUM+A(I)/X(I)
      ENDDO
      FX=SUM
      RETURN
      END
      SUBROUTINE EGRADF(X,GRADF)
      INCLUDE 'O8FUCO.INC'
      DOUBLE PRECISION X(*),GRADF(*)
      INTEGER I
      DOUBLE PRECISION A(14)
      SAVE A
      DATA (A(I),A(I+7),I=1,7)/
cparam a :=
c      1    12842.275       8    1267
c      2      634.25        9     760.05
c      3      634.25       10     633.25
c      4      634.125      11    1266.25
c      5     1268          12     632.875
c      6      633.875      13     394.46
c      7      633.75       14     940.838   ;
     1     12842.275D0,    1267.D0,
     2     634.25D0,        760.05D0,
     3      634.25D0,       633.25D0,
     4      634.125D0,      1266.25D0,
     5     1268.D0,           632.875D0,
     7     633.875D0,        394.46D0,
     8      633.75D0,        940.838D0 /
      ICGF=ICGF+1
      DO I=1,14
        GRADF(I)=-A(I)/X(I)**2
      ENDDO
      RETURN
      END
      SUBROUTINE EH(I,X,HXI)
      INCLUDE 'O8FUCO.INC'
      INTEGER I
      DOUBLE PRECISION X(*),HXI
      INTEGER J
      DOUBLE PRECISION C(14),SUM
      SAVE C
      DATA (C(J),C(J+7),J=1,7)/
C
Cparam c :=
C      1     5.47934        8     3.90896
C      2      .83234        9     2.74284
C      3      .94749       10     2.60541
C      4     1.11082       11     5.96184
C      5     2.64824       12     3.29522
C      6     1.55868       13     1.83517
C      7     1.73215       14     2.81372   ;
     1      5.47934D0,             3.90896D0,
     2      .83234D0,              2.74284D0,
     3       .94749D0,             2.60541D0,
     4       1.11082D0,            5.96184D0,
     5       2.64824D0,            3.29522D0,
     6       1.55868D0,            1.83517D0,
     7       1.73215D0,            2.81372D0/
      CRES(1)=CRES(1)+1
C  s.t. G1:
C     sum {i in I} c[i] * x[i] == 1;
      SUM=0.D0
      DO J=1,14
        SUM=SUM+C(J)*X(J)
      ENDDO
      HXI=SUM-1.D0
      RETURN
      END
      SUBROUTINE EGRADH(I,X,GRADHI)
      INCLUDE 'O8FUCO.INC'
      INTEGER I,J
      DOUBLE PRECISION X(*),GRADHI(*)
      DOUBLE PRECISION C(14)
      SAVE C
      DATA (C(J),C(J+7),J=1,7)/
C
Cparam c :=
C      1     5.47934        8     3.90896
C      2      .83234        9     2.74284
C      3      .94749       10     2.60541
C      4     1.11082       11     5.96184
C      5     2.64824       12     3.29522
C      6     1.55868       13     1.83517
C      7     1.73215       14     2.81372   ;
     1      5.47934D0,             3.90896D0,
     2      .83234D0,              2.74284D0,
     3       .94749D0,             2.60541D0,
     4       1.11082D0,            5.96184D0,
     5       2.64824D0,            3.29522D0,
     6       1.55868D0,            1.83517D0,
     7       1.73215D0,            2.81372D0/
       DO J=1,14
         GRADHI(J)=C(J)
       ENDDO
       RETURN
       END
       SUBROUTINE EG(I,X,GXI)
       INCLUDE 'O8FUCO.INC'
       INTEGER I
       DOUBLE PRECISION X(*),GXI
       DOUBLE PRECISION UG(14),OG(14)
C!!!! THE ORIGINAL STATEMENT OF PROBLEM 383 IS ERRONEOUS
C     THE LOWER BOUND ZERO USED THERE MAKES NO SENSE HERE, CLEARLY
C     HENCE WE INTRODUCE THE TINY LOWER BOUND 1.D-4, WHICH IS NOT BINDING
C     AT THE SOLUTION
      SAVE UG,OG
      DATA UG/14*1.D-4/
      DATA OG/5*0.04D0,9*0.03D0/
Cs.t. B1 {i in {1..5}}:
C     x[i] <= 0.04;
Cs.t. B2 {i in {6..14}}:
C     x[i] <= 0.03;
C EVALUATION OF BOUNDS NOT COUNTED
      IF ( I .GT. 14 ) THEN
        GXI=OG(I-14)-X(I-14)
      ELSE
        GXI=X(I)-UG(I)
      ENDIF
      RETURN
      END
      SUBROUTINE EGRADG(I,X,GRADGI)
      INCLUDE 'O8FUCO.INC'
      INTEGER I
      DOUBLE PRECISION X(*),GRADGI(*)
      RETURN
      END


