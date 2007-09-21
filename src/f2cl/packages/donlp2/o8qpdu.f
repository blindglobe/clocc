C***************************************************************
C********************************************************************
C     SOLUTION OF EXTENDED QUADRATIC PROGRAM
C     SCF*GRADF(X)*D+(1/2)*D*A*D+SUMME( TAUQP*D(I) +( MY/2)*D(I)**2)
C     MINIMAL SUBJECT TO
C     D(I) .GE. 0, I=1,...,NR
C     (GRES(.,J)*D+RES(J))+VZ*D(J)=0, J=1,...,NH VZ=-SIGN(RES(J))
C     (GRES(.,ALIST(J)))*D+RES(ALIST(J)))+D(J) .GE. 0, J=NH+1,....,NR
C     THE WEIGHT TAUQP IS ADAPTED DURING SOLUTION
C     THE QUASI-NEWTON-MATRIX A IS TAKEN FROM O8COMM.INC
C     A IS REGULARIZED IF NOT SUFFICIENTLY WELL CONDITIONED
C     THE RESULTING D(1+NR),...,D(N+NR) IS A DIRECTION OF DESCENT FOR
C     THE ZANGWILL FUNCTION OF THE CORRESPONDING NONLINEAR
C     OPTIMIZATION PROBLEM
C     F(X)=MIN, RES(J)=0, J=1,..NH, RES(J) .GE. 0 , J=NH+1,NRES
C     AT THE CURRRENT POINT X IF THE WEIGHT TAUQP IS CHOSEN
C     APPROPRIATELY
C     THE QUADRATIC PROGRAMMING PROBLEM IS SOLVED USING THE METHOD
C     OF GOLDFARB AND IDNANI
C     VARIABLES ARE STORED IN XD (SOLUTION) AND UD (MULTIPLIERS)
C     IN THE FOLLOWING ORDER XD=( D(J), J=1,NR; D=DIRECT. OF DESC.)
C     UD = ( MULTIPLIERS FOR D(I) .GE. 0 , I=1,..,NR ;
C     MULTIPLIERS FOR FOR EQUALITY CONSTRAINTS ,
C     MULTIPLIERS FOR THE GENERAL INEQUALITY CONSTRAINTS )
C**********************************************************************
      SUBROUTINE O8QPDU
      INCLUDE 'O8COMM.INC'
      INCLUDE 'O8CONS.INC'
      INCLUDE 'O8QPDU.INC'
      INTEGER NDUALM,MDUALM,NDUAL,MI,ME,IQ
      PARAMETER (NDUALM=NX+NRESM,MDUALM=NRESM*2)
      DOUBLE PRECISION NP,RNORM,RLOW,XJ,DDUAL,R,UD
      DOUBLE PRECISION G0(NDUALM),CI0(MDUALM),CII(MDUALM),
     F       CEI(NDUALM),
     F       XD(NDUALM),S(MDUALM),Z(NDUALM),VR(MDUALM)
      DOUBLE PRECISION XDOLD(NDUALM),UDOLD(MDUALM),UD1
      INTEGER AI(MDUALM),IAI(MDUALM),IAEXCL(MDUALM),AIOLD(MDUALM)
      DOUBLE PRECISION Y(NDUALM),MULT(NRESM)
      COMMON /O8DUPA/RNORM,RLOW,NDUAL,MI,ME,IQ
      COMMON /O8QPUP/XJ(NDUALM,NDUALM),DDUAL(NDUALM),R(NDUALM,NDUALM),
     F                NP(NDUALM),UD(MDUALM),UD1(MDUALM)
      DOUBLE PRECISION INFE1,S1,S2,TINY,
     F    MY,ZZ,SS,SU,T,T1,T2,F,PSID,C1,C2,CDIAG,O8SC1,TERM,
     F       SU1,SU2,CONDR,INFINY,TERM1,O8VECN,
     F       DIFF0
      INTEGER I,J,K,IP,L,QPDEL(0:MDUALM),INCR
      LOGICAL WLOW
      INTEGER IPTR,IQTR,AITR(MDUALM)
      DOUBLE PRECISION SSTR,RIITR
      COMMON/O8QPTR/SSTR,RIITR,IPTR,IQTR,AITR
      SAVE
      NDUAL=N+NR
C*** NUMBER OF EQUALITY CONSTRAINTS IN QP-PROBLEM
      ME=NH
C*** QP INEQUALITY CONSTRAINTS = ACTIVE CONSTRAINTS -
C*** EQUALITY-CONSTRAINTS + SLACK'S
      MI=2*NR-NH
      INFINY=EPSMAC/TOLMAC
      IF ( ANALYT ) THEN
        TINY=2*NR*EPSMAC*TP3
      ELSE
        TINY=2*NR*MAX(EPSDIF,EPSMAC*TP3)
      ENDIF
      QPTERM=0
      DO I=1,NR
C*** CHECK GRADIENTS OF ACTIVE CONSTRAINTS AGAINST ZERO
        DO J=1,N
          Y(J)=GRES(J,ALIST(I))
        ENDDO
        MULT(I)=ONE
        IF ( O8VECN(1,N,Y) .EQ. ZERO ) THEN
          MULT(I)=ZERO
          IF ( .NOT. SILENT ) CALL O8MSG(8)
        ENDIF
      ENDDO
C*** RESTART POINT IN CASE OF INCREASE OF TAUQP
   10 CONTINUE
C*******  INITIALIZE MATRICES J AND R
      DO I=1,NDUAL
        DDUAL(I)=ZERO
        DO J=1,NDUAL
          R(J,I)=ZERO
          XJ(J,I)=ZERO
        ENDDO
      ENDDO
      RNORM=ONE
      RLOW=ONE
      TERM1=ZERO
      DO I=1,NRES
        U(I)=ZERO
        IF ( W(I) .GT. TERM1 ) TERM1=W(I)
      ENDDO
      ACCINF(ITSTEP,19)=CLOW
      ACCINF(ITSTEP,20)=TERM1
      ACCINF(ITSTEP,31)=TAUQP
      DO I=1,ME+MI
        UD(I)=ZERO
      ENDDO
      C1=ABS(A(1,1))
      DO I=1,N
        C1=MAX(C1,ABS(A(I,I)))
      ENDDO
      C1=C1*TP1
C**** WE REQUIRE MUCH MORE REGULARITY OF A IN THE SINGULAR CASE
      DO I=1,N
        IF ( ABS(A(I,I)) .LT. SQRT(RHO1)*C1 ) A(I,I)=SQRT(RHO1)*C1
      ENDDO
C**** INVERT THE CHOLESKY-FACTOR AND STORE IN XJ ( IDNANIS J-MATRIX)
      CALL O8RINV(NX,N,A,NDUALM,NDUAL,XJ)
      C1=ABS(A(1,1))
      INCR=NR
      C2=ABS(XJ(1+INCR,1+INCR))
      DO I=1,N
        C1=MAX(C1,ABS(A(I,I)))
        C2=MAX(C2,ABS(XJ(I+INCR,I+INCR)))
      ENDDO
      MY=ZERO
      DO I=1,N
        DO J=I,N
           MY=MY+A(I,J)**2
        ENDDO
      ENDDO
      MY=MY/N
      CDIAG=ONE/SQRT(MY)
      DO  I=1,INCR
        XJ(I,I)=CDIAG
      ENDDO
      DO I=1,NDUAL
        IF ( I .GT. INCR )  G0(I)=GRADF(I-INCR)*SCF
        IF ( I .LE. INCR )  G0(I)=TAUQP
      ENDDO
C*** COMPUTE UNCONSTRAINED SOLUTION
C*** THE CHOLESKY-FACTOR OF A IS STORED IN THE UPPER TRIANGLE
      DO I=1,N
        SU=ZERO
        DO J=1,I-1
          SU=SU+A(J,I)*Y(J+INCR)
        ENDDO
        Y(I+INCR)=(G0(I+INCR)-SU)/A(I,I)
      ENDDO
      DO I=N,1,-1
        SU=ZERO
        DO J=I+1,N
          SU=SU+A(I,J)*Y(J+INCR)
        ENDDO
        Y(I+INCR)=(Y(I+INCR)-SU)/A(I,I)
      ENDDO
      DO I=1,INCR
C****** INITIALLY ASSUME THE SLACKS BEING ZERO
        Y(I)=ZERO
      ENDDO
      DO I=1,NDUAL
        XD(I)=-Y(I)
      ENDDO
C****** UNCONSTRAINED MINIMIZER OF THE QP: SLACKS COME FIRST
      F=P5*O8SC1(1,NDUAL,G0,XD)
C****** DEFINE THE INITIAL WORKING SET: ALL SLACKS ARE AT THEIR
C****** LOWER BOUNDS
      IQ=NR
      DO I=1,IQ
        AI(I)=I
        R(I,I)=ONE
        UD(I)=TAUQP
      ENDDO
      RNORM=ONE
      RLOW=ONE
C****** INTRODUCTION OF EQUALITY CONSTRAINTS
      DO I=1,ME
        DO  J =1,IQ
          UD1(J)=UD(J)
        ENDDO
   20   CONTINUE
        UD1(IQ+1)=ZERO
C***** AN EQUALITY CONSTRAINT IS INDICATED BY THE NEGATIVE INDEX
        AI(IQ+1)=-NR-I
        DO  J=1,N
C*** ME=NH AND ALIST(I)=I FOR I=1,..,NH
          CEI(J+INCR)=GRES(J,I)
        ENDDO
        DO J=1,INCR
          CEI(J)=ZERO
        ENDDO
        CEI(I)=ONE
        IF ( RES(I) .GT. ZERO ) CEI(I)=-ONE
        DO  J=1,NDUAL
          NP(J)=CEI(J)
        ENDDO
        CALL O8ZUP(Z)
        IF ( IQ .NE. 0 ) CALL O8RUP(VR)
C*******|Z|=0?
COLD        ZZ=O8SC1(1,NDUAL,Z,Z)
        ZZ=O8VECN(1,NDUAL,Z)
        TERM=O8SC1(1,NDUAL,Z,NP)
COLD        IF  (ZZ .NE. ZERO .AND. TERM .GT. ZERO )         THEN
        IF  (ZZ .GE. TINY*RNORM .AND. TERM .GT. ZERO )         THEN
          T2=(-O8SC1(1,NDUAL,NP,XD)-RES(I))/TERM
        ELSEIF ( (-O8SC1(1,NDUAL,NP,XD)-RES(I)) .GE. ZERO ) THEN
          T2=INFINY
        ELSE
          T2=-INFINY
        ENDIF
C***** IN ADDITION OF AN EQUALITY CONSTRAINT, T2 MAY BE
C***** POSITIVE OR NEGATIVE
        IF ( IQ .NE. 0 ) CALL O8RUP(VR)
        L=0
        IF ( T2 .GT. ZERO ) THEN
          T1=INFINY
          DO K=1,IQ
            IF(VR(K) .GT. ZERO .AND. AI(K) .GT. 0 )     THEN
              IF(UD1(K)/VR(K) .LT. T1)      THEN
                T1=UD1(K)/VR(K)
              ENDIF
            ENDIF
          ENDDO
          T=MIN(T1,T2)
        ELSE
          T1=INFINY
          DO K=1,IQ
            IF(VR(K) .LT. ZERO .AND. AI(K) .GT. 0 )     THEN
              IF(UD1(K)/ABS(VR(K)) .LT. T1)      THEN
                T1=UD1(K)/ABS(VR(K))
              ENDIF
            ENDIF
          ENDDO
          T1=-T1
          T=MAX(T1,T2)
C**** T NOW NEGATIVE
        ENDIF
C*** ADD CONSTRAINT , OTHERWISE WE MUST FIRST DELETE SOME
C*** INEQUALITY CONSTRAINT WITH NEGATIVE MULTIPLIER
C*** FIRST DELETE THEN ADD!
        IF ( ABS(T) .GE. INFINY ) GOTO 2000
        IF(ABS(T2) .GE. INFINY )       THEN
C***** PURELY DUAL STEP
          DO      K=1,IQ
            UD1(K)=UD1(K)+T*(-VR(K))
            IF ( UD1(K) .LT. ZERO .AND. AI(K) .GT. 0 ) UD1(K)=ZERO
          ENDDO
          UD1(IQ+1)=UD1(IQ+1)+T
          QPDEL(0)=0
          DO J=1,IQ
            IF ( UD1(J) .LE. TINY .AND. AI(J) .GT. 0 ) THEN
              QPDEL(0)=QPDEL(0)+1
              QPDEL(QPDEL(0))=AI(J)
            ENDIF
          ENDDO
          DO K=1,QPDEL(0)
            L=QPDEL(K)
            IAI(L)=L
            CALL O8DLCD(AI,L)
          ENDDO
          GOTO 20
        ENDIF
        DO K=1,NDUAL
          XD(K)=XD(K)+T*Z(K)
        ENDDO
        DO K=1,IQ
          UD1(K)=UD1(K)+T*(-VR(K))
          IF ( UD1(K) .LT. ZERO .AND. AI(K) .GT. 0 ) UD1(K)=ZERO
        ENDDO
        UD1(IQ+1)=UD1(IQ+1)+T
        F=F+T*O8SC1(1,NDUAL,Z,NP)*(P5*T+UD1(IQ+1))
        IF ( ABS(T2-T1) .LE. TINY ) THEN
          QPDEL(0)=0
          DO J=1,IQ
            IF ( UD1(J) .LE. TINY .AND. AI(J) .GT. 0 ) THEN
              QPDEL(0)=QPDEL(0)+1
              QPDEL(QPDEL(0))=AI(J)
            ENDIF
          ENDDO
          DO K=1,QPDEL(0)
            L=QPDEL(K)
            IAI(L)=L
            CALL O8DLCD(AI,L)
          ENDDO
          AI(IQ+1)=-I-NR
          CALL O8ADCD
        ELSEIF ( T .EQ. T2 ) THEN
          AI(IQ+1)=-I-NR
          CALL O8ADCD
        ELSE
          QPDEL(0)=0
          DO J=1,IQ
            IF ( UD1(J) .LE. TINY .AND. AI(J) .GT. 0 ) THEN
              QPDEL(0)=QPDEL(0)+1
              QPDEL(QPDEL(0))=AI(J)
            ENDIF
          ENDDO
          DO K=1,QPDEL(0)
            L=QPDEL(K)
            IAI(L)=L
            CALL O8DLCD(AI,L)
          ENDDO
          GOTO 20
        ENDIF
        DO J=1,IQ
          UD(J)=UD1(J)
        ENDDO
      ENDDO
C****** SET IAI=K\AI
      DO I=1,MI
        IAI(I)=I
      ENDDO
C****** STEP 1
   50 CONTINUE
C***  AI = QP - WORKING SET , IAI(I)=0 IF I IN AI
      DO I=1,IQ
        IP=AI(I)
        IF ( IP .GT. 0 ) IAI(IP)=0
      ENDDO
C****** S(XD)=CI(TRANS)*XD+CI0 >= 0 ?
      PSID=ZERO
C*** PSID : THE MEASURE OF INFEASIBILITY
      DO I=1,MI
C*** IAEXCL: IF = 0, EXCLUDE FROM ADDITION IN THIS CYCLE
        IAEXCL(I)=1
        SU=ZERO
C*** NUMBERS OF INEQUALITY CONSTRAINTS:
C*** I=1,...,NR CORRESPONDS TO THE CONSTRAINTS V>=0, U_A>=0
C*** I=NR+1,....,MI TO THE REGULARIZED GENERAL INEQUALITIES
        IF ( I .GT. NR ) THEN
          K=ALIST(I+NH-INCR)
          DO J=1,N
            CII(J+INCR)=GRES(J,K)
          ENDDO
          DO J=1,INCR
            CII(J)=ZERO
          ENDDO
          CII(NH+I-INCR)=ONE
          CI0(I)=RES(K)
        ELSE
          DO J=1,NDUAL
            CII(J)=ZERO
          ENDDO
          CI0(I)=ZERO
          CII(I)=ONE
        ENDIF
        SU=O8SC1(1,NDUAL,CII,XD)+CI0(I)
        S(I)=SU
        PSID=PSID+MIN(ZERO,SU)
      ENDDO
      DO I=1,IQ
        UDOLD(I)=UD(I)
        AIOLD(I)=AI(I)
      ENDDO
      DO  I=1,NDUAL
        XDOLD(I)=XD(I)
      ENDDO
   60 CONTINUE
      SS=ZERO
      IP=0
C*** INTRODUCE MOST VIOLATED INEQUALITY CONSTRAINT
      DO I=1,MI
        IF(S(I) .LT. SS .AND. IAI(I) .NE. 0 .AND. IAEXCL(I) .NE.0)THEN
          SS=S(I)
          IP=I
        ENDIF
      ENDDO
      IF ( IQ .GT. 1 ) THEN
        CONDR=RNORM/RLOW
      ELSE
        CONDR=ONE
      ENDIF
C***
C***
      IF (ABS(PSID) .LE. TINY*(C1*C2+CONDR) .OR. IP .EQ. 0) THEN
C****   SUCCESSFUL TERMINATION OF QP-SOLVER FOR CURRENT TAUQP
        IF ( ABS(PSID) .GT. TINY*(C1*C2+CONDR) .AND. .NOT. SILENT )
     F    CALL O8MSG(10)
        QPTERM=1
        ACCINF(ITSTEP,30)=ONE
        ACCINF(ITSTEP,13)=CONDR
        ACCINF(ITSTEP,14)=C1*C2
        DO I=1,N
          D(I)=XD(I+INCR)
        ENDDO
CNEW : DNORM ADDED
        DNORM=O8VECN(1,N,D)
        INFEAS=ZERO
        DO I=1,INCR
          INFEAS=INFEAS+ABS(XD(I))
        ENDDO
C******* L1-NORM OF SLACK VARIABLES
        ACCINF(ITSTEP,31)=TAUQP
        ACCINF(ITSTEP,32)=INFEAS
        WLOW=.FALSE.
        SU1=ZERO
        SU2=ZERO
        DO I=1,IQ
          IF ( AI(I) .LT. 0 ) THEN
            U(-(AI(I)+NR))=UD(I)
          ELSE
           IF ( AI(I) .GT. NR ) U(ALIST(AI(I)+NH-NR))=UD(I)
          ENDIF
        ENDDO
        TERM1=ZERO
        DO J=1,N
          NP(J)=GRADF(J)*SCF
        ENDDO
        DO I=1,NRES
          DO J=1,N
            NP(J)=NP(J)-GRES(J,I)*U(I)
          ENDDO
        ENDDO
        B2N=O8VECN(1,N,NP)
        IF ( SCF .NE. ZERO ) B2N=B2N/SCF
C*** CORRECTION IN THE ORIGINAL VARIABLES
        INFE1=ZERO
        DO I=1,NR
          INFE1=INFE1+ABS(XD(I))*MULT(I)
        ENDDO
COLD        IF ( UPSI .LE. DELMIN*NRES .AND. UPSI0 .LE. DELMIN*NRES
        IF ( UPSI .LE. DELMIN*NRES
     F   .AND.  B2N .LE. (GFN+ONE)*EPSX*TP2 .AND. PHASE .GE. 0
     F   .AND. INFEAS .LE. DELMIN*NRES
     F      )  THEN
C****** SINCE MULTIPLIERS MAY BE INCORRECT FOR INFEAS .NE. ZERO
C****** BE CAREFUL. IF MULTIPLIERS ARE IMPORTANT CHECK IN SOLCHK
C****** AND POSSIBLY COMPUTE THEM SEPARATELY GIVEN X AND THE ACTIVE
C****** SET E.G. BY USING NNLS FROM NETLIB
C****** WE CONSIDER THE PROBLEM AS SUCCESSFULLY SOLVED WITH REDUCED
C****** REQUIREMENTS
          DO I=1,N
            D(I)=ZERO
          ENDDO
          DNORM=ZERO
          QPTERM=ONE
          DIRDER=ZERO
          OPTITE=THREE
          RETURN
        ENDIF
C*** THERE MAY BE AN ADDITIONAL INCREASE OF TAUQP NECESSARY AGAIN
C***
        IF ( INFE1 .GT. (ONE-DELTA1/TAUQP)*UPSI .AND.
     F       ( O8VECN(1,N,D) .LE. MIN(INFE1,INFE1**2)*TP1
     F        .OR. UPSI .GT. TAU0*P5 ) ) THEN
C*** FURTHER INCREASE TAUQP !
          DO I=1,NRES
            U(I)=ZERO
            SLACK(I)=ZERO
          ENDDO
          IF ( .NOT. SILENT ) CALL O8MSG(17)
          IF ( TAUQP*TAUFAC .GT. TAUMAX ) THEN
            IF ( .NOT. SILENT ) CALL O8MSG(5)
            QPTERM=-1
            ACCINF(ITSTEP,30)=QPTERM
            ACCINF(ITSTEP,31)=TAUQP
            ACCINF(ITSTEP,32)=INFEAS
            DO I=1,N
              D(I)=ZERO
            ENDDO
            DNORM=ZERO
            RETURN
          ELSE
            TAUQP=TAUQP*TAUFAC
            GOTO 10
          ENDIF
        ENDIF
C*** COMPUTE NEW WEIGHTS FOR THE PENALTY-FUNCTION
  500   CONTINUE
        DO I=1,NRES
          SLACK(I)=ZERO
        ENDDO
        DO I=1,NR
          SLACK(ALIST(I))=XD(I)
        ENDDO
        WLOW=.FALSE.
        DO I=1,NRES
          W1(I)=W(I)
          IF ( I .LE. NH ) THEN
            IF ( ABS(SLACK(I)) .GT. ABS(RES(I))+TINY ) THEN
              W1(I)=ABS(U(I))
            ELSE
              W1(I)=NY*ABS(U(I))+TAU
            ENDIF
          ELSE
            IF ( BIND(I)  .EQ. 0 ) THEN
              W1(I)=MAX(W(I)*P8,TAU)
            ELSE
              IF ( RES(I) .GE. ZERO .AND. SLACK(I) .LE. TINY)
     *            W1(I)=MAX(NY*ABS(U(I))+TAU,(ABS(U(I))+W1(I))*P5)
              IF ( RES(I) .GE. ZERO .AND. SLACK(I) .GT. TINY )
     *            W1(I)=ABS(U(I))
              IF (RES(I) .LT. ZERO .AND. SLACK(I) .LE. -RES(I)+TINY)
     *            W1(I)=MAX(NY*ABS(U(I))+TAU,(ABS(U(I))+W1(I))*P5)
              IF (RES(I) .LT. ZERO .AND. SLACK(I) .GT. -RES(I)+TINY)
     *            W1(I)=ABS(U(I))

            ENDIF
          ENDIF
          IF ( W1(I) .LT. W(I) ) WLOW=.TRUE.
        ENDDO
        IF ( WLOW  ) THEN
          S1=ZERO
          S2=ZERO
          DO I=1,NRES
            IF ( I .LE. NH ) THEN
              S1=S1+W1(I)*ABS(RESST(I))
              S2=S2+W1(I)*ABS(RES(I))
            ELSE
              S1=S1-MIN(ZERO,RESST(I))*W1(I)
              S2=S2-MIN(ZERO,RES(I))*W1(I)
            ENDIF
          ENDDO
          DIFF0=(FXST-FX)*SCF+(S1-S2)
          IF (  DIFF0 .GE. ETA*CLOW
     F    .AND. ITSTEP-LASTDW .GE. MAX(5,MIN(N/10,20))  ) THEN
C***** ACCEPT NEW (DIMINISHED ) WEIGHTS
            LASTDW=ITSTEP
            LASTCH=ITSTEP
            LEVEL=DIFF0/ITERMA
            PSIST=S1
            PSI=S2
            DO I=1,NRES
              IF (W1(I) .NE. W(I)) LASTCH=ITSTEP
              W(I)=W1(I)
            ENDDO
            CLOW=CLOW+ONE
            IF ( CLOW .GT. ITSTEP/10 ) THEN
C*** ADDITIONAL INCREASE OF ETA
              ETA=ETA*ONEP3
              IF ( .NOT. SILENT )  CALL O8INFO(11)
            ENDIF
            IF ( .NOT. SILENT ) CALL O8INFO(12)
            GOTO 1000
          ENDIF
        ENDIF
C***  WE CANNOT ACCEPT NEW WEIGHTS
C***  RESET WEIGHTS
        DO I=1,NRES
          W1(I)=W(I)
          IF ( I .LE. NH ) THEN
            IF ( SLACK(I) .GT. ABS(RES(I)) )
     *        W1(I)=ABS(U(I))
            IF ( SLACK(I) .LE. ABS(RES(I)) ) THEN
              IF ( W(I) .LE. ABS(U(I)) .AND. ABS(U(I)) .LE.
     *             W(I)+TAU ) THEN
                   W1(I)=W(I)+TWO*TAU
              ELSE
                   W1(I)=MAX(W(I),NY*ABS(U(I))+TAU)
              ENDIF
            ENDIF
          ELSE
            IF ( SLACK(I) .GT. -MIN(-TINY,RES(I))
     *        .AND. BIND(I) .EQ. 1 ) THEN
              W1(I)=ABS(U(I))
            ELSEIF ( BIND(I) .EQ. 1 .AND. SLACK(I) .LE.
     *           -MIN(-TINY,RES(I)) .AND. U(I) .LE. W(I)+TAU
     *           .AND. W(I) .GE. U(I) )
     *        THEN
              W1(I)=W(I)+TWO*TAU
            ELSEIF ( BIND(I) .EQ. 1 ) THEN
              W1(I)=MAX(W(I),NY*ABS(U(I))+TAU)
            ENDIF
          ENDIF
        ENDDO
        TERM1=ZERO
        DO I=1,NRES
          IF ( W1(I) .GT. W(I) .OR. W1(I) .LT. W(I) )
     *       LASTCH=ITSTEP
          IF ( W1(I) .GT. W(I) ) LASTUP=ITSTEP
          IF ( W1(I) .LT. W(I) ) LASTDW=ITSTEP
          W(I)=W1(I)
          TERM1=MAX(TERM1,W(I))
        ENDDO
        S1=ZERO
        S2=ZERO
        DO I=1,NRES
          IF ( I.LE. NH ) THEN
            S1=S1+W(I)*ABS(RESST(I))
            S2=S2+W(I)*ABS(RES(I))
          ELSE
            S1=S1-W(I)*MIN(ZERO,RESST(I))
            S2=S2-W(I)*MIN(ZERO,RES(I))
          ENDIF
        ENDDO
        PSIST=S1
        PSI=S2
        IF ( .NOT. SILENT ) CALL O8INFO(12)
        ACCINF(ITSTEP,20)=TERM1
        ACCINF(ITSTEP,19)=CLOW
        GOTO 1000
C***
      ENDIF
      IF ( IP .GT. NR ) THEN
        K=ALIST(IP+NH-NR)
        DO J=1,N
          CII(J+INCR)=GRES(J,K)
        ENDDO
        DO J=1,INCR
          CII(J)=ZERO
        ENDDO
        CII(NH+IP-NR)=ONE
        CI0(IP)=RES(K)
      ELSE
        DO J=1,NDUAL
          CII(J)=ZERO
        ENDDO
        CI0(IP)=ZERO
        CII(IP)=ONE
      ENDIF
      DO I=1,NDUAL
        NP(I)=CII(I)
      ENDDO
      DO  I =1,IQ
        UD1(I)=UD(I)
      ENDDO
      UD1(IQ+1)=ZERO
      AI(IQ+1)=IP
  100 CONTINUE
C********   STEP 2A
      CALL O8ZUP(Z)
      IF(IQ .NE. 0)     CALL O8RUP(VR)
      L=0
      T1=INFINY
      DO K=1,IQ
        IF(AI(K) .GT. 0 .AND. VR(K) .GT. ZERO)     THEN
          IF(UD1(K)/VR(K) .LT. T1)      THEN
            T1=UD1(K)/VR(K)
          ENDIF
        ENDIF
      ENDDO
C*******|Z|=0?
COLD      ZZ=O8SC1(1,NDUAL,Z,Z)
      ZZ=O8VECN(1,NDUAL,Z)
      TERM=O8SC1(1,NDUAL,Z,NP)
COLD      IF  (ZZ .NE. ZERO .AND. TERM .GT. ZERO )         THEN
      IF ( ZZ .GE. TINY*RNORM .AND. TERM .GT. ZERO ) THEN
        T2=-S(IP)/TERM
      ELSE
        T2=INFINY
      ENDIF
      T=MIN(T1,T2)
      IF(T .GE. INFINY ) GOTO 2000
C**********
      IF(T2 .GE. INFINY )       THEN
        DO      K=1,IQ
          UD1(K)=UD1(K)+T*(-VR(K))
          IF ( UD1(K) .LT. ZERO .AND. AI(K) .GT. 0 ) UD1(K)=ZERO
        ENDDO
        UD1(IQ+1)=UD1(IQ+1)+T
        QPDEL(0)=0
        DO I=1,IQ
          IF ( UD1(I) .LE. TINY .AND. AI(I) .GT. 0 ) THEN
            QPDEL(0)=QPDEL(0)+1
            QPDEL(QPDEL(0))=AI(I)
          ENDIF
        ENDDO
        DO K=1,QPDEL(0)
          L=QPDEL(K)
          IAI(L)=L
          CALL O8DLCD(AI,L)
        ENDDO
        GOTO 100
      ENDIF
      DO K=1,NDUAL
        XD(K)=XD(K)+T*Z(K)
      ENDDO
      DO K=1,IQ
        UD1(K)=UD1(K)+T*(-VR(K))
        IF ( UD1(K) .LT. ZERO .AND. AI(K) .GT. 0 ) UD1(K)=ZERO
      ENDDO
      UD1(IQ+1)=UD1(IQ+1)+T
      F=F+T*O8SC1(1,NDUAL,Z,NP)*(P5*T+UD1(IQ+1))
      IF (T2 .LE.T1-TINY  )      THEN
C**** DDUAL IS COMPUTED BY O8ZUP
        IF ( O8VECN(IQ+1,NDUAL,DDUAL) .LT. EPSMAC*RNORM )
     F    THEN
C**** DEGENERACY: ADDING THIS CONSTRAINT GIVES A SINGULAR WORKING SET
C**** THEORETICALLY IMPOSSIBLE, BUT DUE TO ROUNDOFF THIS MAY OCCUR
C**** MARK THIS CONSTRAINT AND TRY TO ADD ANOTHER ONE
          IPTR=IP
          IQTR=IQ
          DO I=1,IQ
            AITR(I)=AI(I)
          ENDDO
          SSTR=SS
          RIITR=O8VECN(IQ+1,NDUAL,DDUAL)
          IF ( .NOT. SILENT ) CALL O8MSG(19)
          IAEXCL(IP)=0
          DO  I=1,MI
            IAI(I)=I
          ENDDO
          DO  I=1,IQ
            AI(I)=AIOLD(I)
            IF (AI(I) .GT. 0 ) IAI(AI(I))=0
            UD1(I)=UDOLD(I)
          ENDDO
          DO  I=1,NDUAL
            XD(I)=XDOLD(I)
          ENDDO
          GOTO 60
        ENDIF
C*** ADD CONSTRAINT, L-PAIR
        CALL O8ADCD
        IAI(IP)=0
        DO I=1,IQ
          UD(I)=UD1(I)
        ENDDO
        GOTO 50
      ENDIF
      SU=ZERO
      IF ( IP .GT. NR ) THEN
C*** A GENERAL LINEAR INEQUALITY CONSTRAINT
        K=ALIST(IP+NH-NR)
        DO J=1,N
          CII(J+INCR)=GRES(J,K)
        ENDDO
        DO J=1,INCR
          CII(J)=ZERO
        ENDDO
        CII(NH+IP-NR)=ONE
        CI0(IP)=RES(K)
        S(IP)=O8SC1(1,NDUAL,CII,XD)+CI0(IP)
      ELSE
C*** A SLACK CONSTRAINT
        S(IP)=XD(IP)
      ENDIF
C*** NOW T=T1
      QPDEL(0)=0
      DO I=1,IQ
        IF ( UD1(I) .LE. TINY .AND. AI(I) .GT. 0 ) THEN
          QPDEL(0)=QPDEL(0)+1
          QPDEL(QPDEL(0))=AI(I)
        ENDIF
      ENDDO
      DO K=1,QPDEL(0)
        L=QPDEL(K)
        IAI(L)=L
        CALL O8DLCD(AI,L)
      ENDDO
      IF ( T2 .LE. T1+TINY ) THEN
        IF ( O8VECN(IQ+1,NDUAL,DDUAL) .LT. EPSMAC*RNORM )
     F    THEN
C**** DEGENERACY
          IPTR=IP
          IQTR=IQ
          DO I=1,IQ
            AITR(I)=AI(I)
          ENDDO
          SSTR=SS
          RIITR=O8VECN(IQ+1,NDUAL,DDUAL)
           IF ( .NOT. SILENT ) CALL O8MSG(19)
          IAEXCL(IP)=0
          DO  I=1,MI
            IAI(I)=I
          ENDDO
          DO  I=1,IQ
            AI(I)=AIOLD(I)
            IF ( AI(I) .GT. 0 ) IAI(AI(I))=0
            UD1(I)=UDOLD(I)
          ENDDO
          DO  I=1,NDUAL
            XD(I)=XDOLD(I)
          ENDDO
          GOTO 60
        ENDIF
C*** ADD CONSTRAINT, L-PAIR
        CALL O8ADCD
        IAI(IP)=0
        DO I=1,IQ
          UD(I)=UD1(I)
        ENDDO
        GOTO 50
      ELSE
        GOTO 100
      ENDIF
C**** THIS IS THE EXIT POINT OF O8QPDU
C**** WE EITHER MAY HAVE SUCCESSFUL OR UNSUCCESSFUL TERMINATION HERE
C**** THE LATTER WITH QPTERM=-2 OR -3, IN WHICH CASE IT MAY NEVERTHELESS
C**** BE POSSIBLE TO USE THE COMPUTED D. -2 OR -3 EXIT IS THEORETICALLY
C**** IMPOSSIBLE BUT MAY OCCUR DUE TO ROUNDOFF EFFECTS.
C**** WE CHECK THE DIRECTIONAL DERIVATIVE OF THE PENALTY-FUNCTION NOW
 1000 CONTINUE
C*** CUT AND RESCALE D IF APPROPRIATE
      CALL O8CUTD
C*** COMPUTE THE DIRECTIONAL DERIVATIVE DIRDER
      CALL O8DIRD
      IF ( DIRDER .GE. ZERO  .OR.
     F    ( -DIRDER .LE. EPSMAC*TP2*(SCF*ABS(FX)+PSI+ONE) .AND.
     F      INFEAS .GT. MAX(UPSI,NRES*DELMIN) ) ) THEN
        IF ( .NOT. SILENT ) CALL O8MSG(18)
        IF ( TAUQP .LE. TAUMAX/TAUFAC ) THEN
          TAUQP=TAUQP*TAUFAC
          GOTO 10
        ELSE
          IF ( .NOT. SILENT ) CALL O8MSG(5)
          QPTERM=-1
          ACCINF(ITSTEP,30)=QPTERM
          ACCINF(ITSTEP,31)=TAUQP
          ACCINF(ITSTEP,32)=INFEAS
          DO I=1,N
            D(I)=ZERO
          ENDDO
          DNORM=ZERO
          DIRDER=ZERO
        ENDIF
      ENDIF
      RETURN
 2000 CONTINUE
C*** QP INFEASIBLE ( IN THIS APPLICATION IMPOSSIBLE , THEORETICALLY)
        IF ( .NOT. SILENT ) CALL O8MSG(20)
        QPTERM=-2
        ACCINF(ITSTEP,30)=-TWO
        ACCINF(ITSTEP,13)=CONDR
        ACCINF(ITSTEP,14)=C1*C2
        DO I=1,N
          D(I)=XD(I+INCR)
        ENDDO
        DNORM=O8VECN(1,N,D)
        SU1=ZERO
        DO I=1,INCR
          SU1=SU1+ABS(XD(I))
        ENDDO
C******* L1-NORM OF SLACK VARIABLES
        ACCINF(ITSTEP,32)=SU1
        INFEAS=SU1
        WLOW=.FALSE.
        SU1=ZERO
        SU2=ZERO
        DO I=1,IQ
          IF ( AI(I) .LT. 0 ) THEN
            U(-(AI(I)+NR))=UD(I)
          ELSE
            IF ( AI(I) .GT. NR ) U(ALIST(AI(I)-NR+NH))=UD(I)
          ENDIF
        ENDDO
        TERM1=ZERO
        DO J=1,N
          NP(J)=GRADF(J)*SCF
        ENDDO
        DO I=1,NRES
          DO J=1,N
            NP(J)=NP(J)-GRES(J,I)*U(I)
          ENDDO
          W1(I)=W(I)
          IF ( I .LE. NH ) THEN
            IF ( SLACK(I) .GT. ABS(RES(I)) )
     *        W1(I)=ABS(U(I))
            IF ( SLACK(I) .LE. ABS(RES(I)) ) THEN
              IF ( W(I) .LE. ABS(U(I)) .AND. ABS(U(I)) .LE.
     *             W(I)+TAU ) THEN
                W1(I)=W(I)+TWO*TAU
              ELSE
                W1(I)=MAX(W(I),NY*ABS(U(I))+TAU)
              ENDIF
            ENDIF
            SU1=SU1+ABS(RES(I))*W1(I)
            SU2=SU2+ABS(RESST(I))*W1(I)
          ELSE
            IF ( SLACK(I) .GT. -MIN(-TINY,RES(I))
     *        .AND. BIND(I) .EQ. 1 ) THEN
              W1(I)=ABS(U(I))
            ELSEIF ( BIND(I) .EQ. 1 .AND. SLACK(I) .LE.
     *           -MIN(-TINY,RES(I)) .AND. U(I) .LE. W(I)+TAU
     *           .AND. W(I) .GE. U(I) )
     *        THEN
              W1(I)=W(I)+TWO*TAU
            ELSEIF ( BIND(I) .EQ. 1 ) THEN
              W1(I)=MAX(W(I),NY*ABS(U(I))+TAU)
            ENDIF
            SU1=SU1-W1(I)*MIN(ZERO,RES(I))
            SU2=SU2-W1(I)*MIN(ZERO,RESST(I))
          ENDIF
          IF ( W(I) .NE. W1(I) ) LASTCH=ITSTEP
          W(I)=W1(I)
          TERM1=MAX(TERM1,W(I))
        ENDDO
        PSIST=SU2
        PSI=SU1
        B2N=SQRT(O8SC1(1,N,NP,NP))
        IF ( SCF .NE. ZERO ) THEN
            B2N=B2N/SCF
        ELSE
            B2N = -ONE
        ENDIF
        IF ( WLOW ) THEN
           CLOW=CLOW+ONE
           LASTCH=ITSTEP
           LASTDW=ITSTEP
        ENDIF
        IF ( .NOT. SILENT ) CALL O8INFO(12)
        ACCINF(ITSTEP,19)=CLOW
        ACCINF(ITSTEP,20)=TERM1
        ACCINF(ITSTEP,31)=TAUQP
        GOTO 1000
      END
