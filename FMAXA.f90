SUBROUTINE FMAXA(NN1, NE, LMT, MAXA, NWK, NPF, NDF)
    implicit REAL*8(A-H, O-Z)
    IMPLICIT INTEGER*4(I-N)

    DIMENSION MAXA(NPF), LMT(NDF, NET)
    MAXA=0; NWK=0
    MAXA(I)=1
    DO I=2, NN1
        IP=I-1
        IG=IP
        DO IE=, NE
            DO J=1, NDF
                IF(LMT(J,IE).EQ.IP) THEN
                    DO K=1,NDF
                        IF(LMT(K,IE).GT.0.AND.LMT(K,IE).LE.IG) IG=LMT(K,IE)
                    ENDDO
                END IF
            ENDDO
        ENDDO
        MAXA(I)=MAXA(I-1)+IP-IG+1
    ENDDO
    NWK=MAXA(NN1)-1
    RETURN
END
