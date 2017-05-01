        !COMPILER-GENERATED INTERFACE MODULE: Mon May 01 19:19:28 2017
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DATAOUT__genmod
          INTERFACE 
            SUBROUTINE DATAOUT(NP,NE,NPF,DIST,FF,SG,SM)
              INTEGER(KIND=4) :: NPF
              INTEGER(KIND=4) :: NE
              INTEGER(KIND=4) :: NP
              REAL(KIND=8) :: DIST(NPF)
              REAL(KIND=8) :: FF(NPF)
              REAL(KIND=8) :: SG(NE)
              REAL(KIND=8) :: SM(NE)
            END SUBROUTINE DATAOUT
          END INTERFACE 
        END MODULE DATAOUT__genmod
