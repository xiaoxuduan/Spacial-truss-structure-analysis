        !COMPILER-GENERATED INTERFACE MODULE: Sun May 14 15:49:56 2017
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE MAT__genmod
          INTERFACE 
            SUBROUTINE MAT(M,N,A,B)
              INTEGER(KIND=4) :: N
              INTEGER(KIND=4) :: M
              REAL(KIND=4) :: A(M,N)
              REAL(KIND=4) :: B(N,M)
            END SUBROUTINE MAT
          END INTERFACE 
        END MODULE MAT__genmod
