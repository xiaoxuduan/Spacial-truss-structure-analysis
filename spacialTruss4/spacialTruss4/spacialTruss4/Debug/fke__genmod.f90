        !COMPILER-GENERATED INTERFACE MODULE: Sat May 06 17:46:07 2017
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE FKE__genmod
          INTERFACE 
            SUBROUTINE FKE(NP,NE,NM,IE,X,Y,Z,ME,NAE,AE,AKE)
              INTEGER(KIND=4) :: NM
              INTEGER(KIND=4) :: NE
              INTEGER(KIND=4) :: NP
              INTEGER(KIND=4) :: IE
              REAL(KIND=4) :: X(NP)
              REAL(KIND=4) :: Y(NP)
              REAL(KIND=4) :: Z(NP)
              INTEGER(KIND=4) :: ME(2,NE)
              INTEGER(KIND=4) :: NAE(NE)
              REAL(KIND=4) :: AE(2,NM)
              REAL(KIND=4) :: AKE(2,2)
            END SUBROUTINE FKE
          END INTERFACE 
        END MODULE FKE__genmod
