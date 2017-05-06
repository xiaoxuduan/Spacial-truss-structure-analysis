        !COMPILER-GENERATED INTERFACE MODULE: Wed May 03 10:53:49 2017
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE FMAXA__genmod
          INTERFACE 
            SUBROUTINE FMAXA(NN1,NE,LMT,MAXA,NWK,NPF,NDF,ICKK,IDIST,    &
     &IFTOOL,IFF,IPP,ISG,ISM)
              INTEGER(KIND=4) :: NDF
              INTEGER(KIND=4) :: NPF
              INTEGER(KIND=4) :: NE
              INTEGER(KIND=4) :: NN1
              INTEGER(KIND=4) :: LMT(NDF,NE)
              INTEGER(KIND=4) :: MAXA(NPF)
              INTEGER(KIND=4) :: NWK
              INTEGER(KIND=4) :: ICKK
              INTEGER(KIND=4) :: IDIST
              INTEGER(KIND=4) :: IFTOOL
              INTEGER(KIND=4) :: IFF
              INTEGER(KIND=4) :: IPP
              INTEGER(KIND=4) :: ISG
              INTEGER(KIND=4) :: ISM
            END SUBROUTINE FMAXA
          END INTERFACE 
        END MODULE FMAXA__genmod
