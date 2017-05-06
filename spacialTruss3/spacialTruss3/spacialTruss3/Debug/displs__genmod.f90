        !COMPILER-GENERATED INTERFACE MODULE: Sat May 06 21:48:17 2017
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DISPLS__genmod
          INTERFACE 
            SUBROUTINE DISPLS(NP,NE,NPF,NM,NN,IT,FTOOL,DIST,AE,NAE,X,Y,Z&
     &,PP,FF,SG,SM,ME,NR,RR,NF)
              INTEGER(KIND=4) :: NR
              INTEGER(KIND=4) :: NM
              INTEGER(KIND=4) :: NPF
              INTEGER(KIND=4) :: NE
              INTEGER(KIND=4) :: NP
              INTEGER(KIND=4) :: NN
              INTEGER(KIND=4) :: IT(3,NP)
              REAL(KIND=4) :: FTOOL(NPF)
              REAL(KIND=4) :: DIST(NPF)
              REAL(KIND=4) :: AE(2,NM)
              INTEGER(KIND=4) :: NAE(NE)
              REAL(KIND=4) :: X(NP)
              REAL(KIND=4) :: Y(NP)
              REAL(KIND=4) :: Z(NP)
              REAL(KIND=4) :: PP(NPF)
              REAL(KIND=4) :: FF(NPF)
              REAL(KIND=4) :: SG(NE)
              REAL(KIND=4) :: SM(NE)
              INTEGER(KIND=4) :: ME(2,NE)
              REAL(KIND=4) :: RR(2,NR)
              INTEGER(KIND=4) :: NF
            END SUBROUTINE DISPLS
          END INTERFACE 
        END MODULE DISPLS__genmod
