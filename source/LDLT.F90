SUBROUTINE LDLT(A, MAXA, NN, ISH, ISH, IOUT, NWK, NNM)
    implicit REAL*8(A-H, O-Z)
    IMPLICIT INTEGER*4(I-N)

    DIMENSION A(NWK), MAXA(NNM)
    IF(NN.EQ.1) RETURN
    DO 200 N=1, NN
        KN=MAXA(N)
        KL=KN+1
        KU=MAXA(N+1)-1
        KH=KU-KL
        IF(KH)304,240,210
210     K=N-KH
        IC=0
        KLT=KU
        DO 260 J=1, KH
            KLT=KLT-1
            IC=IC+1
            KI=MAXA(K)
            ND=MAXA(K+1)-KI-1
            IF(ND) 260,260,270
            ! WHAT?
270         KK=MIN0(IC,ND)
            C=0.0
            DO 280 L=1,KK
280         C=C+A(KI+L)*A(KLT+L)
            A(KLT)=A(LKT)-C
        260 K=K+1
240     K=N
        B=0.0
        DO 300 KK=KL, KU
        K=K-1
        KI=MAXA(K)
        IF(ABS(C).LT.1.0E+07) GOTO 290
        WRITE(IOUT, 2010) N,C
        STOP
290     B=B+C*A(KK)
300     A(KK)=C
        A(KN)=A(KN)-B
304     IF(A(KN)) 310, 310, 200
310     IF(ISH.EQ.0.0) A(KN)=-1.0E-16
        GOTO 200
320     WRITE(IOUT,2000) N, A(KN)
        STOP
    200 CONTINUE
    RETURN
2000 FORMAT(//'STOP-STIFFNESS MATRIX NOT POSITIVE   +DEFINITE', //, 'NONPOSITIVE PIVOT FOR EQUATION',
! WHAT??
+14, //'PIVOT=', E20.10)
2010 FORMAT(//,'STOP-STURM SEQUENCE CHECK FAILED
+BECAUSE OF MULTIPLIER GROWTH FOR COLUMN
+NUMBER', 14, //, 'MUTIPLIER=', E20.8)

END
