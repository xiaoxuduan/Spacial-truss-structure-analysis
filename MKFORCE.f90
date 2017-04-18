SUBROUTINE MKFORCE(NP, NF, NPF, NCF, NN, IT, PF, PP, FTOOL)
    implicit REAL*8(A-H, O-Z)
    IMPLICIT INTEGER*4(I-N)

    DIMENSION IT(NF, NP), PF(4, NCF), PP(NPF), FTOOL(NPF)
    PP=0; FTOOL=0
    DO I=1, NCF
        NOD=PF(1,I)
        DO J=1, NF
            PP(NF*(NOD-1)+J)=PF(J+1, I)
        ENDDO
    ENDDO
    DO I=1, NP
        DO J=1, NF
            LAB=IT(J,I)
            IF(LAB.GT.0.AND.LAB.LE.NN) THEN
                FTOOL(LAB)=PP(NF*(I-1)+J)
            ENDIF
        ENDDO
    ENDDO
RETURN
END
