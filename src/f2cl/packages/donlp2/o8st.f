C************************************************************
      SUBROUTINE O8ST
C************************************************************
C    INITIALIZATION PROGRAM , STANDARD PARAMETER SETTINGS DONE HERE
C************************************************************
      INCLUDE 'O8COMM.INC'
      INCLUDE 'O8CONS.INC'
      INCLUDE 'O8FINT.INC'
      INTEGER I,J,K,IZ
      DOUBLE PRECISION TOL1 ,XNULL(NX),BD0,
     *                 INFINY,GXI,HXI,TERM
      CHARACTER*8 FIL,XXX,NAME1
      SAVE
      DATA XXX/'XXXXXXXX'/
      EPSMAC = TWO**(-20)
100   CONTINUE
      EPSMAC=EPSMAC/TWO
      TERM=ONE+EPSMAC
      IF ( TERM .NE. ONE ) GOTO 100
      EPSMAC=EPSMAC+EPSMAC
      TOLMAC=EPSMAC
200   CONTINUE
      TOL1=TOLMAC
      TOLMAC=TOLMAC/TWOP4
      IF ( TOLMAC .NE. ZERO ) GOTO 200
      TOLMAC=TOL1
C******** EPSMAC MACHINE PRECISION, TOLMAC SMALLEST MACHINE NUMBER
C******** LARGER THAN ZERO (APPROXIMATELY , BASE 16 FOR EXPONENT
C******** THEREFORE DIVISION BY 16 ASSUMED)
C
C***** WARNING
C      ON SOME MACHINES THE COMPUTATION OF TOLMAC MAY RESULT IN AN
C      ERROR
C      BECAUSE UNDERFLOW IS NOT ACCEPTED AS ZERO AS IS ASSUMED HERE
C
C*****
      IF ( N .GT. NX ) STOP 'DONLP2: N TOO LARGE/RECOMPILE'
      IF ( NH+NG .GT. NRESM )STOP 'DONLP2:NH OR NG TOO LARGE/RECOMPILE'
      IF ( TAU0 .EQ. ZERO ) TAU0=ONE
      IF ( DEL0 .EQ. ZERO ) DEL0=TAU0*P5
C      IF ( DEL0 .GT. TAU0 ) DEL0=TAU0*P5
      IF ( NRESET .GT. N )  NRESET=N
      IF ( NRESET .LE. 4  ) NRESET=4
C****** STANDARD INITIALIZATION
      LASTCH=0
      LASTDW=0
      LASTUP=0
      LEVEL=ONE
      TAU=TM1
      ITERMA=MAXIT
      EPSX=TM5
      SIGSM=SQRT(EPSMAC)
      SMALLD=TM1
C FORMERLY TM2. SMALLD HAS MUCH INFLUENCE ON THE MARATOS-EFFECT
      SMALLW=EXP(TWO*LOG(EPSMAC)/THREE)
      RHO=TM6
      RHO1=TM10
      DEL01=DEL0/TP1
      DELMIN=MIN(DEL01,MAX(TM6*DEL0,SMALLW))
      IF ( .NOT. ANALYT ) DELMIN=MIN(DEL01,MAX(EPSDIF,DELMIN))
      C1D=TM2
      SCFMAX=TP4
      TAUFAC=TP1
      TAUMAX=SCFMAX**2
      UPDMY0=TM1
C     TAKE DEL0 AND TAU0 FROM BLOCK DATA OR SETUP0 IN FUNCTION
C     DEFINITION
C     MAY BE MODIFIED BY SUBSEQUENT CALL OF SETUP
        J=1
        DO WHILE ( INDEX(NAME(J:40),' ') .EQ. 1 )
          J=J+1
        ENDDO
        IF ( J .GT. 40 ) THEN
          FIL=XXX
        ELSE
          K=INDEX(NAME(J:40),' ')
          K=K+J-1
          IF ( K .GT. J+7 ) K=J+7
          IF ( K .LT. J ) K=J
          IF ( K .GT. 40 ) K=40
          NAME1=NAME(J:K)
          DO I=1,K-J+1
            IZ=ICHAR(NAME1(I:I))
            IF ( IZ .LT. 48 .OR. ( IZ .GT. 57 .AND. IZ .LT. 65 )
     F       .OR. ( IZ .GT. 90 .AND. IZ .LT. 97 ) .OR. IZ .GT. 122 )
     F      NAME1(I:I)='X'
          ENDDO
          IF ( K .LT. J+7 ) THEN
            FIL=NAME1(1:K-J+1)//XXX(1:J+7-K)
          ELSE
            FIL=NAME1(1:8)
          ENDIF
        ENDIF
      IF ( .NOT. SILENT ) OPEN(MEU,FILE=FIL//'.MES',STATUS='UNKNOWN')
      IF ( .NOT. SILENT ) OPEN(PROU,FILE=FIL//'.PRO',STATUS='UNKNOWN')
      INFINY=EPSMAC/TOLMAC
      FX=ZERO
      B2N=ZERO
      B2N0=ZERO
      NRES=NG+NH
      IF ( COLD ) THEN
        DO I=1,NX
          DO J=1,NX
            A(J,I)=ZERO
          ENDDO
          A(I,I)=ONE
          DIAG0(I)=ONE
        ENDDO
      ENDIF
      DO I=1,NRESM
        DIAG(I)=ZERO
        DO J=1,NX
          QR(J,I)=ZERO
          GRES(J,I)=ZERO
        ENDDO
      ENDDO
      DO I=1,NX
        XNULL(I)=ZERO
        UG(I)=-INFINY
        OG(I)=INFINY
        LLOW(I)=.FALSE.
        LUP(I)=.FALSE.
      ENDDO
      DO I=1,NH
        DELFAC(I)=ONE
      ENDDO
      IF ( BLOC ) CALL USER_EVAL(XNULL,0)
      DO I=NH+1,NRES
        DELFAC(I)=ONE
C***** SCAN FOR REAL LOWER OR UPPER BOUNDS
        IF ( GUNIT(1,I) .EQ. 1 ) THEN
          CALL ESG(I-NH,XNULL,GXI)
          IF ( GUNIT(3,I) .GT. 0 ) THEN
            LLOW(GUNIT(2,I))=.TRUE.
            UG(GUNIT(2,I))=-GXI/GUNIT(3,I)
          ELSE
            OG(GUNIT(2,I))=-GXI/GUNIT(3,I)
            LUP(GUNIT(2,I))=.TRUE.
          ENDIF
        ENDIF
      ENDDO
      DO I=NH+1,NRES
C**** MODIFY DEL0, SUCH THAT LOWER AND UPPER BOUND NEVER BECOME
C     BINDING SIMULTANEOUSLY
        IF ( GUNIT(1,I) .EQ. 1 ) THEN
          J=GUNIT(2,I)
          IF ( OG(J) .LT. INFINY .AND. UG(J) .GT. -INFINY ) THEN
            DEL0=MIN(DEL0,(OG(J)-UG(J))*TM1*ABS(GUNIT(3,I)))
          ENDIF
        ENDIF
      ENDDO
      DO I=NH+1,NRES
C**** DELFAC CORRESPONDS TO AN INDIRECT PRIMAL SCALING
        IF ( GUNIT(1,I) .EQ. 1 ) THEN
          J=GUNIT(2,I)
          IF ( GUNIT(3,I) .GT. 0 ) THEN
            DELFAC(I)=MAX(DELFAC(I),ABS(UG(J))*TM1)
            IF ( OG(J) .LT. INFINY )
     F        DELFAC(I)=MIN(DELFAC(I),(OG(J)-UG(J))/(TP1*DEL0))
          ELSE
            DELFAC(I)=MAX(DELFAC(I),ABS(OG(J))*TM1)
            IF ( UG(J) .GT. -INFINY )
     F        DELFAC(I)=MIN(DELFAC(I),(OG(J)-UG(J))/(TP1*DEL0))
          ENDIF
        ENDIF
      ENDDO
      BD0=INFINY
      DO I=1,N
        IF ( UG(I) .GT. ZERO ) BD0=MIN(BD0,OG(I))
        IF ( OG(I) .LT. ZERO ) BD0=MIN(BD0,-UG(I))
      ENDDO
C**************** CHANGE X IF NECESSARY , SUCH THAT BOUNDS NOT VIOLATED
      CORR=.FALSE.
C** EVALUATE GRADIENTS OF SUPERSIMPLE FUNCTIONS ONLY ONCE
C** A FUNCTION IS SAID TO BE SUPERSIMPLE IFF IT IS OF THE FORM A*X(J)+B
      IF ( GUNIT(1,0) .EQ. 1 ) THEN
        GCONST(0)=.TRUE.
        VAL(0)=.TRUE.
        DO I=1,N
          GRADF(I)=ZERO
        ENDDO
        GRADF(GUNIT(2,0))=GUNIT(3,0)*XSC(GUNIT(2,0))
        GFN=ABS(GUNIT(3,0))
      ELSE
        VAL(0)=.FALSE.
        DO I=1,N
          GRADF(I)=ZERO
        ENDDO
      ENDIF
      DO I=1,NH
        IF ( GUNIT(1,I) .EQ. 1 ) THEN
C*** A FIXED VARIABLE. CORRECTED IF NECESSARY
          VAL(I)=.TRUE.
          GCONST(I)=.TRUE.
          GRES(GUNIT(2,I),I)=GUNIT(3,I)*XSC(GUNIT(2,I))
          GRESN(I)=ABS(GUNIT(3,I))*XSC(GUNIT(2,I))
          IF ( GRESN(I) .EQ. ZERO ) THEN
            IF ( .NOT. SILENT )
     F         WRITE(MEU,*) GUNIT(2,I), ' FIXED VARIABLE/ZERO GRADIENT'
            CLOSE(MEU)
            CLOSE(PROU)
            STOP
          ENDIF
          CALL ESH(I,XNULL,HXI)
          TERM=-HXI/GUNIT(3,I)
          IF ( TERM .NE. X(GUNIT(2,I)) ) CORR=.TRUE.
          X(GUNIT(2,I))=TERM
        ENDIF
      ENDDO
      IF ( BLOC ) CALL USER_EVAL(X,0)
      DO I=NH+1,NRES
        IF ( GUNIT(1,I) .EQ. 1 ) THEN
          IF ( GUNIT(3,I) .EQ. 0 ) THEN
            IF ( .NOT. SILENT ) WRITE(MEU,*)
     F        GUNIT(2,I),' BOUNDED VARIABLE, ZERO GRADIENT'
            CLOSE(MEU)
            CLOSE(PROU)
            STOP
          ENDIF
          CALL ESG(I-NH,X,GXI)
          GXI=TWO*DELMIN-GXI
          IF ( GXI .GT. ZERO ) THEN
            CORR=.TRUE.
            X(GUNIT(2,I))=X(GUNIT(2,I))+GXI/GUNIT(3,I)
          ENDIF
          GRES(GUNIT(2,I),I)=GUNIT(3,I)*XSC(GUNIT(2,I))
          GRESN(I)=ABS(GUNIT(3,I))*XSC(GUNIT(2,I))
          VAL(I)=.TRUE.
          GCONST(I)=.TRUE.
        ENDIF
      ENDDO
C***
      IF ( CORR .AND. .NOT. SILENT ) CALL O8MSG(13)
C***
C     REMEMBER TO REEVALUATE THE FUNCTIONS IF CORR=.TRUE.
C     AND BLOC=.TRUE.
      IF ( BLOC ) CALL USER_EVAL(X,1)
C***
      DO I = 1,NRES
        BIND(I)=0
        BIND0(I)=0
        U(I)=ZERO
        U0(I)=ZERO
        CRES(I) = 0
        CGRES(I) = 0
C INITIAL WEIGHTS OF THE PENALTY-TERM
        IF ( COLD ) W(I)=ONE
        SORT(I)=I
      ENDDO
      CLOW=ONE
      NY=TWO

C********* SCF = WEIGHT FACTOR FOR OBJECTIVE FUNCTION
C********* SCF0 = DAMPING FACTOR FOR TANGENTIAL DIRECTION
      SCF=ONE
      SCF0=ONE
      SIGLA=TWOP11
      BETA=FOUR
C FORMERLY TWO
      ALPHA=TM1
C DELTA =TM2 FORMERLY
      DELTA1=P9
      DELTA=TM3
      THETA=P9
C THETA=0.99 FORMERLY
      ICF=0
      ICGF=0
      IF ( .NOT. SILENT ) THEN
        WRITE(PROU,*) 'DONLP2, V3, 05/29/98, COPYRIGHT P. SPELLUCCI '
        CALL O8TIDA (MEU)
        WRITE(PROU,*) NAME
        WRITE(MEU,*)  'DONLP2, V3, 05/29/98, COPYRIGHT P. SPELLUCCI '
        CALL O8TIDA (PROU)
        WRITE(MEU,*) NAME
      ENDIF
      RETURN
      END
