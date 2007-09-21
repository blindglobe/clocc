C***********************************************************************
C     QR-DECOMPOSITION OF MATRIX OF GRADIENTS OF BINDING CONSTRAINTS
C     THIS SET MAY BE EXPANDED USING MULTIPLE CALLS TO O8DEC.
C     NO EXIT ON SINGULAR R-FACTOR HERE. INFORMATION ON
C     THE DECOMPOSTION IS STORED IN BETAQ AND IN AND BELOW THE
C     DIAGONAL OF QR. R-FACTOR IS STORED IN DIAG (DIAGONAL ) AND
C     ABOVE THE DIAGONAL OF QR. CSCAL IS THE COLUMN SCALING OF THE
C     ORIGINAL MATRIX. COLUMN PIVOTING IS DONE HERE  AND IS STORED
C     IN COLNO
C***********************************************************************
      SUBROUTINE O8DEC(NLOW,NRL)
      INCLUDE 'O8COMM.INC'
      INCLUDE 'O8CONS.INC'
      INTEGER NLOW,NRL,N1,N2
      INTEGER I,J,K,L,I1,I2,IPIV
      DOUBLE PRECISION SUM,TERM,DALPHA,DBETA,QRII,DL,O8SC3
      DOUBLE PRECISION QRI(NX),QRI0(NX),O8VECN,CURLE
      SAVE
      EXTERNAL O8VECN,O8SC3
      IF ( NLOW .GT. NRL ) RETURN
      IF ( NLOW .EQ. 1 ) RANK=0
      DL=ONE/(N+N+N)
      DO  I=NLOW,NRL
        DIAG(I)=ZERO
        BETAQ(I)=ZERO
        COLNO(I)=I
        DO J=1,N
          QRI(J)=GRES(J,ALIST(I))
        ENDDO
        CALL O8LEFT(A,QRI,QRI0,SUM,N)
        IF ( SUM .EQ. ZERO ) THEN
          CSCAL(I)=ONE
          COLLE(I)=ZERO
          DO J=1,N
            QR(J,I)=ZERO
          ENDDO
        ELSE
          DO J=1,N
            QRI(J)=QRI0(PERM(J))
          ENDDO
          TERM=ONE/SQRT(MAX(SUM,RHO**2))
          CSCAL(I)=TERM
          IF ( NLOW .GT. 1 ) THEN
            CALL O8HT(1,0,1,RANK,N,QR,BETAQ,QRI,QRI0)
            DO J=1,N
              QRI(J)=QRI0(J)
            ENDDO
          ENDIF
          DO J=1,N
            QR(J,I)=QRI(J)*TERM
          ENDDO
C*** COLLE : LENGTH OF REMAINING COLUMN  SQUARED
          COLLE(I)=(O8VECN(RANK+1,N,QRI)*TERM)**2
        ENDIF
      ENDDO
      IF ( NLOW .GT. 1 .AND. RANK .LT. NLOW-1 ) THEN
C   SHIFT ZERO BLOCK TO THE RIGHT
         I1=NLOW-1-RANK
         I2=NRL-NLOW+1
         DO I=1,MIN(I1,I2)
           IPIV=RANK+I
           K=NRL-I+1
           TERM=BETAQ(K)
           BETAQ(K)=BETAQ(IPIV)
           BETAQ(IPIV)=TERM
           J=COLNO(K)
           COLNO(K)=COLNO(IPIV)
           COLNO(IPIV)=J
           TERM=COLLE(K)
           COLLE(K)=COLLE(IPIV)
           COLLE(IPIV)=TERM
           DO J=1,N
             TERM=QR(J,K)
             QR(J,K)=QR(J,IPIV)
             QR(J,IPIV)=TERM
           ENDDO
         ENDDO
      ENDIF
      IF ( NLOW .GT. 1 ) THEN
        N1=RANK+1
        N2=N1+NRL-NLOW
      ELSE
        N1=NLOW
        N2=NRL
      ENDIF
      DO  I=N1,N2
C  SEARCH FOR PIVOT COLUMN
        IPIV=I
        CURLE=COLLE(I)
        DO J=I+1,N2
          IF ( COLLE(J) .GT. CURLE ) THEN
            CURLE=COLLE(J)
          ENDIF
        ENDDO
        DO J=N2,I,-1
          IF ( COLLE(J) .GE. CURLE/THREE ) IPIV=J
        ENDDO
C  INTERCHANGE COLUMNS EXPLICITLY, IF NECESSARY
C  MAKE INTERCHANGES CONTINUOUS WITH RESPECT TO X
        IF ( IPIV .NE. I ) THEN
          J=COLNO(I)
          COLNO(I)=COLNO(IPIV)
          COLNO(IPIV)=J
          TERM=COLLE(I)
          COLLE(I)=COLLE(IPIV)
          COLLE(IPIV)=TERM
          DO K=1,N
            TERM=QR(K,I)
            QR(K,I)=QR(K,IPIV)
            QR(K,IPIV)=TERM
          ENDDO
        ENDIF
C
        SUM=ZERO
        DO   J=I,N
          TERM=QR(J,I)
          QRI(J)=TERM
          SUM=TERM*TERM+SUM
        ENDDO
        IF(SUM .LE. RHO**2 ) THEN
C   SET TINY VALUES TO ZERO
          DO J=I,N2
            COLLE(J)=ZERO
            DO K=I,N
              QR(K,J)=ZERO
            ENDDO
          ENDDO
          RANK=I-1
          RETURN
        ENDIF
        QRII=QRI(I)
        DALPHA=-SQRT(SUM)
        IF ( ABS(QRII) .LE. -DALPHA*DL ) THEN
          TERM=ZERO
          DO J=I+1,N
            IF ( ABS(QRI(J)) .GT. TERM ) THEN
              TERM=ABS(QRI(J))
              L=J
            ENDIF
          ENDDO
          K=PERM1(I)
          PERM1(I)=PERM1(L)
          PERM1(L)=K
        ENDIF
        IF(QRII .LT. ZERO)      DALPHA=-DALPHA
        DBETA =ONE/(SUM-QRII*DALPHA)
        DIAG(I)=DALPHA
        BETAQ(I)=DBETA
        QRI(I)=QRII-DALPHA
        QR(I,I)=QRI(I)
        RANK=I
        I1=I+1
        DO  J=I1,N2
          SUM=DBETA*O8SC3(I,N,J,QR,NX,QRI)
          DO   K=I,N
            QR(K,J)=QR(K,J)-SUM*QRI(K)
          ENDDO
          COLLE(J)=COLLE(J)-QR(I,J)**2
        ENDDO
      ENDDO
      RETURN
      END
