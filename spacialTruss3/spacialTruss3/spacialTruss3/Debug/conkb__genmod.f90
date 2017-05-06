        !COMPILER-GENERATED INTERFACE MODULE: Sat May 06 21:48:17 2017
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE CONKB__genmod
          INTERFACE 
            SUBROUTINE CONKB(NP,NE,NM,NWK,ME,X,Y,Z,AE,NAE,LMT,MAXA,CKK, &
     &NNM)
              INTEGER(KIND=4) :: NNM
              INTEGER(KIND=4) :: NWK
              INTEGER(KIND=4) :: NM
              INTEGER(KIND=4) :: NE
              INTEGER(KIND=4) :: NP
              INTEGER(KIND=4) :: ME(2,NE)
              REAL(KIND=4) :: X(NP)
              REAL(KIND=4) :: Y(NP)
              REAL(KIND=4) :: Z(NP)
              REAL(KIND=4) :: AE(2,NM)
              INTEGER(KIND=4) :: NAE(NE)
              INTEGER(KIND=4) :: LMT(6,NE)
              INTEGER(KIND=4) :: MAXA(NNM)
              REAL(KIND=4) :: CKK(NWK)
            END SUBROUTINE CONKB
          END INTERFACE 
        END MODULE CONKB__genmod
