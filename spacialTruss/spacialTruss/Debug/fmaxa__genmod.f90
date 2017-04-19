        !COMPILER-GENERATED INTERFACE MODULE: Wed Apr 19 17:07:03 2017
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE FMAXA__genmod
          INTERFACE 
            SUBROUTINE FMAXA(NN1,NE,LMT,MAXA,NWK,NPF,NDF)
              INTEGER(KIND=4) :: NDF
              INTEGER(KIND=4) :: NPF
              INTEGER(KIND=4) :: NE
              INTEGER(KIND=4) :: NN1
              INTEGER(KIND=4) :: LMT(NDF,NE)
              INTEGER(KIND=4) :: MAXA(NPF)
              INTEGER(KIND=4) :: NWK
            END SUBROUTINE FMAXA
          END INTERFACE 
        END MODULE FMAXA__genmod
