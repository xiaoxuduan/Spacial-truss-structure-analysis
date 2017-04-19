        !COMPILER-GENERATED INTERFACE MODULE: Wed Apr 19 17:07:03 2017
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE MAT__genmod
          INTERFACE 
            SUBROUTINE MAT(M,N,A,B)
              INTEGER(KIND=4) :: N
              INTEGER(KIND=4) :: M
              REAL(KIND=8) :: A(M,N)
              REAL(KIND=8) :: B(N,M)
            END SUBROUTINE MAT
          END INTERFACE 
        END MODULE MAT__genmod
