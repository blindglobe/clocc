        SUBROUTINE UPQRQF(N,ETA,S,F0,F1,QT,R,W,T)
C
C SUBROUTINE  UPQRQF  PERFORMS A BROYDEN UPDATE ON THE  Q R
C FACTORIZATION OF A MATRIX  A, (AN APPROXIMATION TO J(X0)),
C RESULTING IN THE FACTORIZATION  Q+ R+ OF
C
C       A+  =  A  +  (Y - A*S) (ST)/(ST * S),
C
C (AN APPROXIMATION TO J(X1))
C WHERE S = X1 - X0, ST = S TRANSPOSE,  Y = F(X1) - F(X0).
C
C THE ENTRY POINT  R1UPQF  PERFORMS THE RANK ONE UPDATE ON THE QR
C FACTORIZATION OF
C
C       A+ =  A + Q*(T*ST).
C
C
C ON INPUT:
C
C N  IS THE DIMENSION OF X AND F(X).
C
C ETA  IS A NOISE PARAMETER.  IF (Y-A*S)(I) .LE. ETA*(|F1(I)|+|F0(I)|)
C    FOR 1 .LE. I .LE. N, THEN NO UPDATE IS PERFORMED.
C
C S(1:N) = X1 - X0   (OR S FOR THE ENTRY POINT R1UPQF).
C
C F0(1:N) = F(X0).
C
C F1(1:N) = F(X1).
C
C QT(1:N,1:N)  CONTAINS THE OLD Q TRANSPOSE, WHERE  A = Q*R .
C
C R(1:N*(N+1)/2)  CONTAINS THE OLD R, STORED BY ROWS.
C
C W(1:N), T(1:N)  ARE WORK ARRAYS ( T  CONTAINS THE VECTOR T FOR THE
C    ENTRY POINT  R1UPQF ).
C
C
C ON OUTPUT:
C
C N  AND  ETA  ARE UNCHANGED.
C
C QT  CONTAINS Q+ TRANSPOSE.
C
C R   CONTAINS R+, STORED BY ROWS.
C
C S, F0, F1, W, AND T  HAVE ALL BEEN CHANGED.
C
C
C CALLS  DAXPY, DDOT, AND DNRM2.
C
C ***** DECLARATIONS *****
C
C     FUNCTION DECLARATIONS
C
        DOUBLE PRECISION DDOT, DNRM2
C
C     LOCAL VARIABLES
C
        DOUBLE PRECISION C, DEN, ONE, SS, WW, YY
        INTEGER I, INDEXR, INDXR2, J, K
        LOGICAL SKIPUP
C
C     SCALAR ARGUMENTS
C
        DOUBLE PRECISION ETA
        INTEGER N
C
C     ARRAY DECLARATIONS
C
        DOUBLE PRECISION  S(N), F0(N), F1(N), QT(N,N), R(N*(N+1)/2),
     $    W(N), T(N), TT(2)
C
C ***** END OF DECLARATIONS *****
C
C ***** FIRST EXECUTABLE STATEMENT *****
C
        ONE = 1.0
        SKIPUP = .TRUE.
C
C ***** DEFINE T AND S SUCH THAT *****
C
C           A+ = Q*(R + T*ST).
C
C T = R*S.
C
        INDEXR = 1
        DO 10 I=1,N
          T(I) = DDOT(N-I+1,R(INDEXR),1,S(I),1)
          INDEXR = INDEXR + N - I + 1
  10    CONTINUE
C
C W = Y - Q*T  = Y - A*S.
C
        DO 20 I=1,N
          W(I) = F1(I) - F0(I) - DDOT(N,QT(1,I),1,T,1)
C
C         IF W(I) IS NOT SMALL, THEN UPDATE MUST BE PERFORMED,
C         OTHERWISE SET W(I) TO 0.
C
          IF (ABS(W(I)) .GT. ETA*(ABS(F1(I)) + ABS(F0(I)))) THEN
            SKIPUP = .FALSE.
          ELSE
            W(I) = 0.0
          END IF
  20    CONTINUE
C
C  IF NO UPDATE IS NECESSARY, THEN RETURN.
C
        IF (SKIPUP) RETURN
C
C T = QT*W = QT*Y - R*S.
C
        DO 30 I=1,N
          T(I) = DDOT(N,QT(I,1),N,W,1)
  30    CONTINUE
C
C S = S/(ST*S).
C
        DEN = 1.0/DDOT(N,S,1,S,1)
        CALL DSCAL(N,DEN,S,1)
C
C ***** END OF COMPUTATION OF  T & S      *****
C       AT THIS POINT,  A+ = Q*(R + T*ST).
C
        ENTRY R1UPQF(N,S,T,QT,R,W)
C
C ***** COMPUTE THE QR FACTORIZATION Q- R- OF (R + T*S).  THEN,  *****
C       Q+ = Q*Q-,  AND  R+ = R-.
C
C FIND THE LARGEST  K  SUCH THAT  T(K) .NE. 0.
C
        K = N
  50    IF (T(K) .NE. 0.0 .OR. K .LE. 1) GOTO 60
          K=K-1
          GOTO 50
  60    CONTINUE
C
C COMPUTE THE INDEX OF R(K-1,K-1).
C
        INDEXR = (N + N - K + 3)*(K - 2) / 2 + 1
C
C ***** TRANSFORM R+T*ST INTO AN UPPER HESSENBERG MATRIX *****
C
C DETERMINE JACOBI ROTATIONS WHICH WILL ZERO OUT ROWS
C N, N-1,...,2  OF THE MATRIX  T*ST,  AND APPLY THESE
C ROTATIONS TO  R.  (THIS IS EQUIVALENT TO APPLYING THE
C SAME ROTATIONS TO  R+T*ST, EXCEPT FOR THE FIRST ROW.
C THUS, AFTER AN ADJUSTMENT FOR THE FIRST ROW, THE
C RESULT IS AN UPPER HESSENBERG MATRIX.  THE
C SUBDIAGONAL ELEMENTS OF WHICH WILL BE STORED IN  W.
C
C NOTE:  ROWS N,N-1,...,K+1 ARE ALREADY ALL ZERO.
C
        DO 90 I=K-1,1,-1
C
C         DETERMINE THE JACOBI ROTATION WHICH WILL ZERO OUT
C         ROW  I+1  OF THE  T*ST  MATRIX.
C
          IF (T(I) .EQ. 0.0) THEN
            C = 0.0
C         SS = SIGN(-T(I+1))= -T(I+1)/|T(I+1)|
            SS = -SIGN(ONE,T(I+1))
          ELSE
            DEN = DNRM2(2,T(I),1)
            C = T(I) / DEN
            SS = -T(I+1)/DEN
          END IF
C
C         PREMULTIPLY  R  BY THE JACOBI ROTATION.
C
          YY = R(INDEXR)
          WW = 0.0
          R(INDEXR) = C*YY - SS*WW
          W(I+1) = SS*YY + C*WW
          INDEXR = INDEXR + 1
          INDXR2 = INDEXR + N - I
          DO 70 J= I+1,N
C           YY = R(I,J)
C           WW = R(I+1,J)
              YY = R(INDEXR)
              WW = R(INDXR2)
C           R(I,J) = C*YY - SS*WW
C           R(I+1,J) = SS*YY + C*WW
              R(INDEXR) = C*YY - SS*WW
              R(INDXR2) = SS*YY + C*WW
            INDEXR = INDEXR + 1
            INDXR2 = INDXR2 + 1
  70      CONTINUE
C
C         PREMULTIPLY  QT  BY THE JACOBI ROTATION.
C
          DO 80 J=1,N
            YY = QT(I,J)
            WW = QT(I+1,J)
            QT(I,J) = C*YY - SS*WW
            QT(I+1,J) = SS*YY + C*WW
  80      CONTINUE
C
C         UPDATE  T(I)  SO THAT  T(I)*ST(J)  IS THE  (I,J)TH  COMPONENT
C         OF  T*ST, PREMULTIPLIED BY ALL OF THE JACOBI ROTATIONS SO
C         FAR.
C
          IF (T(I) .EQ. 0.0) THEN
            T(I) = ABS(T(I+1))
          ELSE
            T(I) = DNRM2(2,T(I),1)
          END IF
C
C         LET INDEXR = THE INDEX OF R(I-1,I-1).
C
          INDEXR = INDEXR - 2*(N - I) - 3
C
  90    CONTINUE
C
C     UPDATE THE FIRST ROW OF  R  SO THAT  R  HOLDS  (R+T*ST)
C     PREMULTIPLIED BY ALL OF THE ABOVE JACOBI ROTATIONS.
C
        CALL DAXPY(N,T(1),S,1,R,1)
C
C ***** END OF TRANSFORMATION TO UPPER HESSENBERG *****
C
C
C ***** TRANSFORM UPPER HESSENBERG MATRIX INTO UPPER *****
C       TRIANGULAR MATRIX.
C
C       INDEXR = INDEX OF R(1,1).
C
          INDEXR = 1
          DO 120 I=1,K-1
C
C           DETERMINE APPROPRIATE JACOBI ROTATION TO ZERO OUT
C           R(I+1,I).
C
            IF (R(INDEXR) .EQ. 0.0) THEN
              C = 0.0
              SS = -SIGN(ONE,W(I+1))
            ELSE
              TT(1) = R(INDEXR)
              TT(2) = W(I+1)
              DEN = DNRM2(2,TT,1)
              C = R(INDEXR) / DEN
              SS = -W(I+1)/DEN
            END IF
C
C           PREMULTIPLY  R  BY JACOBI ROTATION.
C
            YY = R(INDEXR)
            WW = W(I+1)
            R(INDEXR) = C*YY - SS*WW
            W(I+1) = 0.0
            INDEXR = INDEXR + 1
            INDXR2 = INDEXR + N - I
            DO 100 J= I+1,N
C             YY = R(I,J)
C             WW = R(I+1,J)
                YY = R(INDEXR)
                WW = R(INDXR2)
C             R(I,J) = C*YY -SS*WW
C             R(I+1,J) = SS*YY + C*WW
                R(INDEXR) = C*YY - SS*WW
                R(INDXR2) = SS*YY + C*WW
              INDEXR = INDEXR + 1
              INDXR2 = INDXR2 + 1
  100       CONTINUE
C
C           PREMULTIPLY  QT  BY JACOBI ROTATION.
C
            DO 110 J=1,N
              YY = QT(I,J)
              WW = QT(I+1,J)
              QT(I,J) = C*YY - SS*WW
              QT(I+1,J) = SS*YY + C*WW
  110       CONTINUE
  120     CONTINUE
C
C ***** END OF TRANSFORMATION TO UPPER TRIANGULAR *****
C
C
C ***** END OF UPDATE *****
C
C
        RETURN
C
C ***** END OF SUBROUTINE UPQRQF *****
        END
