        !COMPILER-GENERATED INTERFACE MODULE: Sun May 14 15:49:56 2017
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE LDLT__genmod
          INTERFACE 
            SUBROUTINE LDLT(A,MAXA,NN,ISH,IOUT,NWK,NNM)
              INTEGER(KIND=4) :: NNM
              INTEGER(KIND=4) :: NWK
              REAL(KIND=4) :: A(NWK)
              INTEGER(KIND=4) :: MAXA(NNM)
              INTEGER(KIND=4) :: NN
              INTEGER(KIND=4) :: ISH
              INTEGER(KIND=4) :: IOUT
            END SUBROUTINE LDLT
          END INTERFACE 
        END MODULE LDLT__genmod
