        !COMPILER-GENERATED INTERFACE MODULE: Sat May 06 17:46:07 2017
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE FT__genmod
          INTERFACE 
            SUBROUTINE FT(IE,NP,NE,X,Y,Z,ME,T)
              INTEGER(KIND=4) :: NE
              INTEGER(KIND=4) :: NP
              INTEGER(KIND=4) :: IE
              REAL(KIND=4) :: X(NP)
              REAL(KIND=4) :: Y(NP)
              REAL(KIND=4) :: Z(NP)
              INTEGER(KIND=4) :: ME(2,NE)
              REAL(KIND=4) :: T(2,6)
            END SUBROUTINE FT
          END INTERFACE 
        END MODULE FT__genmod
