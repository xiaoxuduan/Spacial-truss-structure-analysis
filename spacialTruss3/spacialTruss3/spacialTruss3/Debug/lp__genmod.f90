        !COMPILER-GENERATED INTERFACE MODULE: Sat May 06 21:48:16 2017
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE LP__genmod
          INTERFACE 
            SUBROUTINE LP(V,PP,IT,PF,NN,NCF,NF,NP,NPF)
              INTEGER(KIND=4) :: NPF
              INTEGER(KIND=4) :: NP
              INTEGER(KIND=4) :: NF
              INTEGER(KIND=4) :: NCF
              INTEGER(KIND=4) :: NN
              REAL(KIND=4) :: V(NN)
              REAL(KIND=4) :: PP(NPF)
              INTEGER(KIND=4) :: IT(NF,NP)
              REAL(KIND=4) :: PF(4,NCF)
            END SUBROUTINE LP
          END INTERFACE 
        END MODULE LP__genmod
