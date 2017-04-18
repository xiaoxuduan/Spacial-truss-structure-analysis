SUBROUTINE MAT(M, N, A, B)
    implicit REAL*8(A-H, O-Z)
    IMPLICIT INTEGER*4(I-N)

    DIMENSION A(M, N), B(N, M)
    DO I=1, M
        DO J=1, N
            B(J,I)=A(I,J)
        ENDDO
    ENDDO
    RETURN
END
