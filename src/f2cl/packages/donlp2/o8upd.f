C*******************************************************************
      SUBROUTINE O8UPD(R,Z,Y,N,FAIL)
      IMPLICIT NONE
      INCLUDE 'O8PARA.INC'
      INCLUDE 'O8CONS.INC'
      INTEGER N,I,J,I1
      LOGICAL FAIL
      DOUBLE PRECISION R(NX,*),Z(*),Y(*)
      DOUBLE PRECISION SDIAG(NX),RN1(NX),W(NX)
      DOUBLE PRECISION YL,ZL,WL,WN1,AI,BI,H,O8DSQ1
      INTRINSIC ABS,SQRT
      SAVE
      EXTERNAL O8DSQ1
C     O8UPD  COMPUTES  THE UPPER TRIANGULAR CHOLESKY-FACTOR
C     R1 OF
C               R(TRANSPOSE)*R+Z*Z(TRANSPOSE)-Y*Y(TRANSPOSE)
C     AND RESTORES IT IN R. THE STRICT LOWER TRIANGLE OF R RE-
C     MAINS UNCHANGED.
C     FAIL=TRUE IF THE DECOMPOSITION DOES'NT EXIST, STOP ON DIMENSION
C     ERROR, FAIL=FALSE ON SUCCESS.
      IF ( N .GT. NX ) THEN
        STOP 'O8UPD WRONG CALL'
      ENDIF
      FAIL=.FALSE.
C     SAVE SUBDIAGONAL
      DO    I=1,N-1
      SDIAG(I)=R(I+1,I)
      R(I+1,I)=ZERO
      ENDDO
C     STEP ONE: INCLUDE Z*Z(TRANSPOSE)
      ZL=ZERO
      DO   I=1,N
        ZL = ZL + Z(I)**2
      ENDDO
      IF ( ZL .NE. ZERO ) THEN
C     SOLVE W(TRANSPOSE)*R=Z(TRANSPOSE)
      CALL O8LEFT(R,Z,W,WL,N)
      WL=SQRT(WL+ONE)
C     U(2)*U(3)*...*U(N)*W = ( NORM(W),0,..,0)(TRANSPOSE)
C     U(I) ROTATIONS
      DO   I=N,2,-1
      IF ( W(I) .NE. ZERO ) THEN
        I1=I-1
        AI=W(I1)
        BI=W(I)
        W(I1)=O8DSQ1(AI,BI)
        AI=AI/W(I1)
        BI=-BI/W(I1)
        R(I,I1)=BI*R(I1,I1)
        R(I1,I1)=AI*R(I1,I1)
        DO   J=I,N
          H = AI*R(I1,J) - BI*R(I,J)
          R(I,J) = BI*R(I1,J) + AI*R(I,J)
          R(I1,J) = H
        ENDDO
      ENDIF
      ENDDO
C     R=D*R, D=DIAG(WL,1,...,1), R NOW HESSENBERG
      DO    I=1,N
      R(1,I)=R(1,I)*WL
      ENDDO
C     R=U(N-1)*...*U(1)*R NOW UPPER TRIANGULAR,
C     U(I)  GIVENS-ROTATIONS
      DO    I=1,N-1
        I1=I+1
        AI=R(I,I)
        BI=-R(I1,I)
        H=O8DSQ1(AI,BI)
        IF ( H .NE. ZERO ) THEN
          AI=AI/H
          BI=BI/H
          R(I,I)=H
          R(I1,I)=ZERO
          DO   J=I+1,N
            H = AI*R(I,J) - BI*R(I1,J)
            R(I1,J) = BI*R(I,J) + AI*R(I1,J)
            R(I,J) = H
          ENDDO
        ENDIF
      ENDDO
      ENDIF
C     STEP TWO :  INCLUDE -Y*Y(TRANSPOSE)
      YL=ZERO
      DO   I=1,N
        YL = YL + Y(I)**2
      ENDDO
      IF ( YL .NE. ZERO ) THEN
        CALL O8LEFT(R,Y,W,WL,N)
        IF ( WL .GE. ONE ) THEN
          FAIL=.TRUE.
        ELSE
          WL=SQRT(ABS(ONE-WL))
          WN1=WL
C******************************************************
C      ( R(NEW) ,0 )                (    R  , W )
C      (-----------) = U(1)*...U(N)*(-----------)
C      (Y(TRANSP),1)                ((0,..,0),WL)
C******************************************************
          DO    I=N,1,-1
            AI=WN1
            BI=W(I)
            WN1=O8DSQ1(AI,BI)
            IF ( WN1 .NE. ZERO ) THEN
              AI=AI/WN1
              BI=BI/WN1
              RN1(I)=BI*R(I,I)
              R(I,I)=AI*R(I,I)
              DO    J=I+1,N
                H = AI*R(I,J) - BI*RN1(J)
                RN1(J) = BI*R(I,J) + AI*RN1(J)
                R(I,J) = H
              ENDDO
            ENDIF
          ENDDO
        ENDIF
      ENDIF
C     RESTORE SUBDIAGONAL
      DO   I=1,N-1
        R(I+1,I)=SDIAG(I)
      ENDDO
      RETURN
      END
