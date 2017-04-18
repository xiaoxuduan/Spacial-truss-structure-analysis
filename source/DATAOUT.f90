SUBROUTINE DATAOUT(NP, NE, NPF, DIST, FF, SG, SM)
    implicit REAL*8(A-H, O-Z)
    IMPLICIT INTEGER*4(I-N)

    DIMENSION DIST(NPF), FF(NPF), SG(NE), SM(NE)
    WRITE(2, 715)(I, (DIST(6*(I-1)+J), J=1, 3), I=1, NP)
715 FORMAT(//5X, 'SOLVED JOINT DISPLACEMENTS DATA'&
& /1X, '  JOINT ', 3X, 12X, 'Dx', 12X, 'Dy', 12X, 'Dz'&
& /(4X, I5, 3X, 3(2X, E12.6)))
    WRITE(2,716)(IE, SG(IE), SM(IE), IE=1, NE)
716 FORMAT(//5X, 'SOLVED ELEMENT INTERNAL FORCE DATA'&
& /1X, '  ELEMENT', 3X, 12X, 'Nx', 8X, 'STRESS'&
& /(4X, I5, 3X, 3(2X, E12.6)))
    WRITE(2, 717)(I, (FF(6*(I-1)+J), J=1, 3), I=1, NP)
717 FORMAT(//5X, 'SOLVED JOINT REACTION DATA'&
& /1X,' JOINT ', 3X, 12X, 'Rx', 12X, 'Ry', 12X, 'Rz'&
& /(4X, I5, 3(2X, E12.6)))
RETURN
END
