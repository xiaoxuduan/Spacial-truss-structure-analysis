        !COMPILER-GENERATED INTERFACE MODULE: Wed May 03 10:53:49 2017
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE FKE__genmod
          INTERFACE 
            SUBROUTINE FKE(NP,NE,NM,IE,X,Y,Z,ME,NAE,AE,AKE)
              INTEGER(KIND=4) :: NM
              INTEGER(KIND=4) :: NE
              INTEGER(KIND=4) :: NP
              INTEGER(KIND=4) :: IE
              REAL(KIND=8) :: X(NP)
              REAL(KIND=8) :: Y(NP)
              REAL(KIND=8) :: Z(NP)
              INTEGER(KIND=4) :: ME(2,NE)
              INTEGER(KIND=4) :: NAE(NE)
              REAL(KIND=8) :: AE(2,NM)
              REAL(KIND=8) :: AKE(2,2)
            END SUBROUTINE FKE
          END INTERFACE 
        END MODULE FKE__genmod
