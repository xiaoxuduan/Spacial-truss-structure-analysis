SUBROUTINE CONKB(NP, NE, NM, NWK, ME, X, Y, Z, AE, NAE,&
& LMT, MAXA, CKK, NN1)
    implicit REAL*8(A-H, O-Z)
    IMPLICIT INTEGER*4(I-N)

    DIMENSION CKK(NWK), X(NP), Y(NP), Z(NP), AE(2, NM),&
    & NAE(NE), LMT(6,NE), ME(2,NE), MAXA(NN1),&
    & AKE(2,2), T(2,6), TT(6,2), TAK(6,6)
    CKK=0
    DO 10 IE=1, NE
        TAK=0
        CALL FKE(NP, NE, NM, IE, X, Y, Z, ME, NAE, AE, AKE)
        CALL FT(IE, NP, NE, X, Y, Z, ME, T)
        CALL MAT(2, 6, T, TT)
        ! if it was aimed to multiply matrix; Matmul can be used;
!        TAK=TT*AKE*T
        TAK=matmul(matmul(TT,AKE),T)
        DO 220 I=1,6
            DO 220 J=1,6
                NI=LMT(I,IE)
                NJ=LMT(J,IE)
                IF((NJ-NNI).GE.0.And.NI*NJ.GT.0) THEN
                    IJ=MAXA(NJ)+NJ-NI
                    CKK(IJ)=CKK(IJ)+TAK(I,J)
                ENDIF
        220 CONTINUE
    10 CONTINUE
    RETURN
END
