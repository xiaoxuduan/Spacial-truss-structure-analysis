        !COMPILER-GENERATED INTERFACE MODULE: Tue May 02 18:47:31 2017
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE FLMT__genmod
          INTERFACE 
            SUBROUTINE FLMT(NP,NE,NN,NN1,NR,RR,ND,NF,NDF,ME,IT,LMT)
              INTEGER(KIND=4) :: NDF
              INTEGER(KIND=4) :: NF
              INTEGER(KIND=4) :: ND
              INTEGER(KIND=4) :: NR
              INTEGER(KIND=4) :: NE
              INTEGER(KIND=4) :: NP
              INTEGER(KIND=4) :: NN
              INTEGER(KIND=4) :: NN1
              REAL(KIND=8) :: RR(2,NR)
              INTEGER(KIND=4) :: ME(ND,NE)
              INTEGER(KIND=4) :: IT(NF,NP)
              INTEGER(KIND=4) :: LMT(NDF,NE)
            END SUBROUTINE FLMT
          END INTERFACE 
        END MODULE FLMT__genmod
