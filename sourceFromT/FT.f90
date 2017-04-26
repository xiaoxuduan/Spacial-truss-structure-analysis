   SUBROUTINE  FT(IE,NP,NE,X,Y,Z,ME,T)
   IMPLICIT  REAL*8(A-H,O-Z)
   IMPLICIT  INTEGER*4(I-N)
    DIMENSION X(NP),Y(NP) ,Z(NP) ,ME(2,NE) ,T(2,6)
    T=0
    N1=ME(1,IE)；N2=ME(2,IE)
    X1=X(N1)；Y1=Y(N1) ；Z1=Z(N1)
    X2=X(N2)；Y2=Y(N2) ；Z2=Z(N2)
    BL=SQRT((X2-X1)**2+(Y2-Y1)**2+(Z2-Z1)**2)
    CX=(X2-X1)/BL
    CY=(Y2-Y1)/BL
    CZ=(Z2-Z1)/BL
    T(1,1)=CX；T(2,4)=CX
    T(1,2)=CY；T(2,5)=CY
    T(1,3)=CZ；T(2,6)=CZ
   RETURN
   END
