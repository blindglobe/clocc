C*****************************************************************
C     SUBPROGRAM FOR STRUCTURED OUTPUT OF A SUBMATRIX A(MA,NA)
C     EXTERNALLY DECLARED A(ME,NE) ON CHANNEL LOGNUM IN FIX OR
C     FLOAT FORMAT
C     WITH HEADING "HEAD".
C     USES A FIXED FORMAT STRING WITH 70 PRINT COLUMNS
C*****************************************************************
      SUBROUTINE O8MDRU(A,ME,NE,MA,NA,HEAD,LOGNUM,FIX)
      IMPLICIT NONE
      INTEGER ME,NE,MA,NA,LOGNUM,I,J,JO,JU
      DOUBLE PRECISION A(ME,NE)
      LOGICAL FIX
      CHARACTER*40 HEAD
      INTEGER ANZ
      SAVE
      WRITE(LOGNUM,FMT='(/A40)')  HEAD
      ANZ=4
      JO=0
      DO WHILE ( JO .LT. NA )
        JU=JO+1
        JO=MIN(JU+ANZ-1,NA)
        WRITE(LOGNUM,FMT='(/''ROW/COLUMN'',4(6X,I3,6X))')
     *   (J,J=JU,JO)
        DO I=1,MA
          IF ( FIX ) THEN
            WRITE(LOGNUM,FMT='(3X,I4,3X,4(G14.7,1X))')
     *         I,(A(I,J),J=JU,JO)
          ELSE
            WRITE(LOGNUM,FMT='(3X,I4,3X,4(D14.7,1X))')
     *         I,(A(I,J),J=JU,JO)
          ENDIF
        ENDDO
      ENDDO
      RETURN
      END
