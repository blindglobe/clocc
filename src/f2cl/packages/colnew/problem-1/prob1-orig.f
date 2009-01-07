C                                                                       MAN   10
C----------------------------------------------------------------       MAN   20
C                                                                       MAN   30
C  PROBLEM 1 - SEE COMPANION PAPER                                      MAN   40
C                                                                       MAN   50
      REAL FSPACE(2000), ZETA(4), TOL(2), Z(4), U(4), ERR(4)            MAN   60
      INTEGER ISPACE(200), M(1), IPAR(11), LTOL(2)                      MAN   70
      EXTERNAL FSUB, DFSUB, GSUB, DGSUB                                 MAN   80
C                                                                       MAN   90
      WRITE (6,99999)                                                   MAN  100
C                                                                       MAN  110
C     ONE DIFFERENTIAL EQUATION OF ORDER 4.                             MAN  120
      M(1) = 4                                                          MAN  130
C     GIVE LOCATION OF BOUNDARY CONDITIONS                              MAN  140
      ZETA(1) = 1.                                                      MAN  150
      ZETA(2) = 1.                                                      MAN  160
      ZETA(3) = 2.                                                      MAN  170
      ZETA(4) = 2.                                                      MAN  180
C     SET UP PARAMETER ARRAY.                                           MAN  190
C     USE DEFAULT VALUES FOR ALL PARAMETERS EXCEPT FOR INITIAL          MAN  200
C     MESH SIZE, NO. OF TOLERANCES AND SIZES OF WORK ARRAYS             MAN  210
      DO 10 I=1,11                                                      MAN  220
        IPAR(I) = 0                                                     MAN  230
   10 CONTINUE                                                          MAN  240
      IPAR(3) = 1                                                       MAN  250
      IPAR(4) = 2                                                       MAN  260
      IPAR(5) = 2000                                                    MAN  270
      IPAR(6) = 200                                                     MAN  280
C     TWO ERROR TOLERANCES (ON U AND ITS SECOND DERIVATIVE)             MAN  290
      LTOL(1) = 1                                                       MAN  300
      LTOL(2) = 3                                                       MAN  310
      TOL(1) = 1.E-7                                                    MAN  320
      TOL(2) = 1.E-7                                                    MAN  330
C                                                                       MAN  340
      CALL COLSYS(1, M, 1., 2., ZETA, IPAR, LTOL, TOL, DUMMY, ISPACE,   MAN  350
     * FSPACE, IFLAG, FSUB, DFSUB, GSUB, DGSUB, DUMMY)                  MAN  360
C                                                                       MAN  370
      IF (IFLAG.NE.1) STOP                                              MAN  380
C     CALCULATE THE ERROR AT 101 POINTS USING THE KNOWN                 MAN  390
C     EXACT SOLUTION                                                    MAN  400
      X = 1.                                                            MAN  410
      DO 20 I=1,4                                                       MAN  420
        ERR(I) = 0.                                                     MAN  430
   20 CONTINUE                                                          MAN  440
      DO 40 J=1,101                                                     MAN  450
        CALL APPSLN(X, Z, FSPACE, ISPACE)                               MAN  460
        CALL EXACT(X, U)                                                MAN  470
        DO 30 I=1,4                                                     MAN  480
          ERR(I) = AMAX1(ERR(I),ABS(U(I)-Z(I)))                         MAN  490
   30   CONTINUE                                                        MAN  500
        X = X + .01                                                     MAN  510
   40 CONTINUE                                                          MAN  520
      WRITE (6,99998) (ERR(I),I=1,4)                                    MAN  530
      STOP                                                              MAN  540
99999 FORMAT (1H1, 35H EXAMPLE OF A SIMPLE PROBLEM SETUP./10H  UNIFORML,MAN  550
     * 36HY LOADED BEAM OF VARIABLE STIFFNESS,/21H  SIMPLY SUPPORTED AT,MAN  560
     * 11H BOTH ENDS./)                                                 MAN  570
99998 FORMAT (/27H ERROR TOLERANCES SATISFIED//22H THE EXACT ERRORS ARE,MAN  580
     * /7X, 4E12.4)                                                     MAN  590
      END                                                               MAN  600
C................................................................       FSU   10
      SUBROUTINE FSUB(X, Z, F)                                          FSU   20
      REAL Z(4), F(1)
      F(1) = (1.-6.*X**2*Z(4)-6.*X*Z(3))/X**3
      RETURN
      END
C................................................................       DFS   10
      SUBROUTINE DFSUB(X, Z, DF)                                        DFS   20
      REAL Z(4), DF(1,4)
      DF(1,1) = 0.
      DF(1,2) = 0.
      DF(1,3) = -6./X**2
      DF(1,4) = -6./X
      RETURN
      END
C................................................................       GSU   10
      SUBROUTINE GSUB(I, Z, G)                                          GSU   20
      REAL Z(4)
      GO TO (10, 20, 10, 20), I
   10 G = Z(1) - 0.
      RETURN
   20 G = Z(3) - 0.
      RETURN
      END
C................................................................       DGS   10
      SUBROUTINE DGSUB(I, Z, DG)                                        DGS   20
      REAL Z(4), DG(4)
      DO 10 J=1,4
        DG(J) = 0.
   10 CONTINUE
      GO TO (20, 30, 20, 30), I
   20 DG(1) = 1.
      RETURN
   30 DG(3) = 1.
      RETURN
      END
C................................................................       EXA   10
      SUBROUTINE EXACT(X, U)                                            EXA   20
      REAL U(4)
C     EXACT SOLUTION
      U(1) = .25*(10.*ALOG(2.)-3.)*(1.-X) + .5*(1./X+(3.+X)*ALOG(X)-X)
      U(2) = -.25*(10.*ALOG(2.)-3.) + .5*(-1./X/X+ALOG(X)+(3.+X)/X-1.)
      U(3) = .5*(2./X**3+1./X-3./X/X)
      U(4) = .5*(-6./X**4-1./X/X+6./X**3)
      RETURN
      END
