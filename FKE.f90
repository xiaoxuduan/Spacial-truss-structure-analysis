SUBROUTINE FKE(NP, NE, NM, IE, X, Y, Z, ME, NAE, AE, AKE)
    implicit REAL*8(A-H, O-Z)
    IMPLICIT INTEGER*4(I-N)

    DIMENSION X(NP), Y(NP), Z(NP), ME(2,NE), NAE(NE), AE(2,NM), AKE(2,2)
    N1=ME(1,IE)
    N2=ME(2,IE)
    X1=X(N1); Y1=Y(N1); Z1=Z(N1)
    X2=X(N2); Y2=Y(N2); Z2=Z(N2)
    BL=SQRT((X2-X1)**2+(Y2-Y1)**2+(Z2-Z1)**2)
    NMI=NAE(IE)
    E0=AE(1,NMI); A0=AE(2,NMI)
    C=E0*A0/BL
    AKE(1,1)=C
    AKE(1,2)=-C
    AKE(2,1)=-C
    AKE(2,2)=C
    RETURN
END
