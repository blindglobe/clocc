C     Test file for common block translations - adapted from mi10mach.for
C     Includes example of declared COMMON variables (CONV and RESTRT)
C
C
      SUBROUTINE MINOS2( Z,NWCORE,NCALLS,INFORM )
      IMPLICIT           REAL*8(A-H,O-Z)
      DOUBLE PRECISION   Z(NWCORE)
      COMMON    /M2MAPZ/ MAXW,LOC(60)
      COMMON    /M3LOC / LASCAL,LBL   ,
     *                   LHRTYP,LHS(2)
      LOGICAL            CONV,RESTRT
      COMMON    /M7CG1 / CGBETA,ITNCG,RESTRT
      COMMON    /M7CONV/ ETASH,NFAIL,CONV(4)
      COMMON    A,B(5,5)/ALBL/C,D//E,F/
      COMMON    /ALBL/G,H//I,J
      END
