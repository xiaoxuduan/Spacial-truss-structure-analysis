SUBROUTINE FLMT(NP, NE, NN, NN1, NR, RR, ND, NF, NDF, ME, IT, LMT)
    implicit REAL*8(A-H, O-Z)
    IMPLICIT INTEGER*4(I-N)

    DIMENSION IT(NF, NP), LMT(NDF, NE), ME(ND, NE), RR(2,NR)
    NN=0; NN1=0; IT=0; LMT=0
    N=0
    DO I=1, NP
        C=0
        DO K=1, NR
            KR=RR(1, K)
            IF(KR.EQ.I) C=RR(2, K)
        ENDDO
        NC=C
        C=C-NC
        DO J=1, NF
            C=C*10.0
            L=C+0.1
            C=C-L
            IF(L.EQ.0) THEN
                N=N+1
                IT(J,I)=N
            ELSE
                IT(J,I)=0
            ENDIF
        ENDDO
    ENDDO
    NN=N
    NN1=NN+1
    DO IE=1, NE
        DO I=1, ND
            NI=ME(I,IE)
            DO J=1,NF
            ! NI??
                LMT((I-1)*NF+J, IE)=IT(J,NI)
            ENDDO
        ENDDO
    ENDDO
    RETURN
END
