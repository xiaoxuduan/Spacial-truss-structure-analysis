        !COMPILER-GENERATED INTERFACE MODULE: Sun May 14 15:49:56 2017
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE REBACK__genmod
          INTERFACE 
            SUBROUTINE REBACK(A,V,MAXA,NN,NWK,NNM)
              INTEGER(KIND=4) :: NNM
              INTEGER(KIND=4) :: NWK
              INTEGER(KIND=4) :: NN
              REAL(KIND=4) :: A(NWK)
              REAL(KIND=4) :: V(NN,1)
              INTEGER(KIND=4) :: MAXA(NNM)
            END SUBROUTINE REBACK
          END INTERFACE 
        END MODULE REBACK__genmod
