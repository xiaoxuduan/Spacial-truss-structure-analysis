        !COMPILER-GENERATED INTERFACE MODULE: Tue May 02 18:59:29 2017
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DISPLS__genmod
          INTERFACE 
            SUBROUTINE DISPLS(NP,NE,NF,NPF,NM,NN,IT,FTOOL,DIST,AE,NAE,X,&
     &Y,Z,PP,FF,SG,SM)
              INTEGER(KIND=4) :: NM
              INTEGER(KIND=4) :: NPF
              INTEGER(KIND=4) :: NF
              INTEGER(KIND=4) :: NE
              INTEGER(KIND=4) :: NP
              INTEGER(KIND=4) :: NN
              INTEGER(KIND=4) :: IT(NF,NP)
              REAL(KIND=8) :: FTOOL(NPF)
              REAL(KIND=8) :: DIST(NPF)
              REAL(KIND=8) :: AE(2,NM)
              INTEGER(KIND=4) :: NAE(NE)
              REAL(KIND=8) :: X(NP)
              REAL(KIND=8) :: Y(NP)
              REAL(KIND=8) :: Z(NP)
              REAL(KIND=8) :: PP(NPF)
              REAL(KIND=8) :: FF(NPF)
              REAL(KIND=8) :: SG(NE)
              REAL(KIND=8) :: SM(NE)
            END SUBROUTINE DISPLS
          END INTERFACE 
        END MODULE DISPLS__genmod
