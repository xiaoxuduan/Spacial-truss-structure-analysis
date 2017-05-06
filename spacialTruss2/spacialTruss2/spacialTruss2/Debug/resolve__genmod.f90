        !COMPILER-GENERATED INTERFACE MODULE: Wed May 03 10:53:49 2017
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE RESOLVE__genmod
          INTERFACE 
            SUBROUTINE RESOLVE(A,V,MAXA,NN,NWK,NNM)
              INTEGER(KIND=4) :: NNM
              INTEGER(KIND=4) :: NWK
              INTEGER(KIND=4) :: NN
              REAL(KIND=8) :: A(NWK)
              REAL(KIND=8) :: V(NN,1)
              INTEGER(KIND=4) :: MAXA(NNM)
            END SUBROUTINE RESOLVE
          END INTERFACE 
        END MODULE RESOLVE__genmod
