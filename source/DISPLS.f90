SUBROUTINE DISPLS(NP, NE, NF, NPF, NM, NN, IT, FTOOL,&
& DIST, AE, NAE, X, Y, Z, PP, FF, SG, SM)
    implicit REAL*8(A-H, O-Z)
    IMPLICIT INTEGER*4(I-N)

    DIMENSION IT(NF, NP), DIST(NPF), FTOOL(NPF), T(2,6),&
    & TT(6,2), AE(2,NM), ME(2,NE), NAE(NE), UE(6), U(2),&
    & AKE(2,2), FEI(2), FE(6), FF(NPF), PP(NPF), SG(NE), SM(NE)
    ! WHAT?? SG??
    !SG=0; 
    SM=0; FF=0
    DO I=1, NP
        DO J=1, NF
            LAB=IT(J,I)
            IF(LAB.EQ.0) THEN
                DIST(NF*(I-1)+J)=0.0
            ELSEIF(LAB.GT.0.AND.LAB.LE.NN) THEN
                DIST(NF*(I-1)+J)=FTOOL(LAB)
            ENDIF
        ENDDO
    ENDDO
    DO IE=1,NE
        N1=ME(1,IE); N2=ME(2,IE)
        DO J=1,NF
            UE(J)=DIST(NF*(N1-1)+J)
            UE(NF+J)=DIST(NF*(N2-1)+J)
        ENDDO
        CALL FT(IE, NP, NE, X, Y, Z, ME, T)
        CALL FKE(NP, NE, NM, IE, X, Y, Z, ME, NAE, AE, AKE)
        ! IF U=T*UE,CHANGE TO FUNCTION MATMUL()
        U=MATMUL(T,UE)
        FEI=MATMUL(AKE,U)
        CALL MAT(2, 6, T, TT)
        FE=MATMUL(TT,FEI)
        DO J=1, NF
            FF(NF*(N1-1)+J)=FF(NF*(N1-1)+J)+FE(J)
            FF(NF*(N2-1)+J)=FF(NF*(N2-1)+J)+FE(NF+J)
        ENDDO
        ISW=NAE(IE)
        AO=AE(2,ISW)
        SG(IE)=FEI(2)
        SM(IE)=FEI(2)/AO
    ENDDO
    DO I=1, NPF
        FF(I)=FF(I)-PP(I)
    ENDDO
    RETURN
END
