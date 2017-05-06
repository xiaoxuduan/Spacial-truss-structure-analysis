        !COMPILER-GENERATED INTERFACE MODULE: Wed May 03 10:53:50 2017
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE CONKB__genmod
          INTERFACE 
            SUBROUTINE CONKB(NP,NE,NM,NWK,ME,X,Y,Z,AE,NAE,LMT,MAXA,CKK, &
     &NN1)
              INTEGER(KIND=4) :: NN1
              INTEGER(KIND=4) :: NWK
              INTEGER(KIND=4) :: NM
              INTEGER(KIND=4) :: NE
              INTEGER(KIND=4) :: NP
              INTEGER(KIND=4) :: ME(2,NE)
              REAL(KIND=8) :: X(NP)
              REAL(KIND=8) :: Y(NP)
              REAL(KIND=8) :: Z(NP)
              REAL(KIND=8) :: AE(2,NM)
              INTEGER(KIND=4) :: NAE(NE)
              INTEGER(KIND=4) :: LMT(6,NE)
              INTEGER(KIND=4) :: MAXA(NN1)
              REAL(KIND=8) :: CKK(NWK)
            END SUBROUTINE CONKB
          END INTERFACE 
        END MODULE CONKB__genmod
