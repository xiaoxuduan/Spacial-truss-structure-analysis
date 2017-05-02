       SUBROUTINE DATAIN(NP,NE,NF,ND,NDF,NPF,NM,NR,NCF,&
     &   IME,INAE,IIT,ILMT,IMAXA,&
&   IX,IY,IZ,IRR,IAE,IPF,ICKK,IDIST,IFTOOL,IFF,IPP,ISG,ISM)
        IMPLICIT REAL*8 (A-H,O-Z)
        IMPLICIT INTEGER*4 (I-N)
        COMMON /AT/A(18000000)
        COMMON /IAT/IA(2000000)
        ! read data from file 1, so file 1 should be created firstly with data needed in;
        READ(1,*)NP,NE,NM,NR,NCF
        WRITE(2,701)NP,NE,NM,NR,NCF
  701   FORMAT(//1X,'###OUTPUT OF ORIGINAL INPUT INFORMATION###'&
     &  //5X,'Number of joints                                  JOINTS=',I5&
     &   /5X,'Number of elements                              ELEMENTS=',I5&
     &   /5X,'Number of material property groups    PROPERTY TYPES=',I5&
     &   /5X,'Number of restrained joints                 RESTRAINTS=',I5&
     &   /5X,'Number of concentrative forced joints             NCF=',I5)
!C--------------FORM POINTER----------------------------------
        NF=3
        ND=2
        NDF=ND*NF
        NPF=NP*NF
        IME=0
        INAE=IME+2*NE
        IIT=INAE+NE
        ILMT=IIT+NF*NP
        IMAXA=ILMT+NDF*NE
        IX=0
        IY=IX+NP
        IZ=IY+NP
        IRR=IZ+NP
        IAE=IRR+2*NR
        IPF=IAE+2*NM
        ICKK=IPF+4*NCF
        IDIST=ICKK+NWK
        IFTOOL=IDIST+NPF
        IFF=IFTOOL+NPF
        IPP=IFF+NPF
        ISG=IPP+NPF
        ISM=ISG+NE
        ! Below read should be change all!
        ! chaghe to A(start: end), remove I;
        ! ADD I=1;
        READ(1,*)(A(IX+I),A(IY+I),A(IZ+I),I=1,NP)
        WRITE(2,714)(I,A(IX+I),A(IY+I),A(IZ+I),I=1,NT)
  714   FORMAT(//5X,'GENERATED JOINT COORDINATES DATA'&
     &        /1X,'  JOINT  ',15X,'X',13X,'Y',13X,'Z'&
     &  /(4X,I5,3X,3(2X,E12.6)))
        READ(1,*)(A(IAE+2*(I-1)+1), A(IAE+2*(I-1)+2),I=1,NM)
        READ(1,*)(IA(IME+2*(I-1)+1),IA(IME+2*(I-1)+2),IA(INAE+I),I=1,NE)
        WRITE(2,606)(I,A(IAE+2*(I-1)+1), A(IAE+2*(I-1)+2),I=1,NM)
        WRITE(2,607)(I,IA(IME+2*(I-1)+1),IA(IME+2*(I-1)+2),IA(INAE+I),I=1,NE)
606     FORMAT(/5X,'ELEMENT MATERAIL PROPERTIES DATA'&
&         /2X,'NO.',10X,' E',10X,'Ax'&
     &         /(2X,I3,2(1X,E11.5)))
607    FORMAT(/5X,'TRUSS ELEMENT DEFINITION DATA'&
&         /2X,'NO.',10X,'JOINT_1',10X,'JOINT_2'&
     &         /(2X,I3,2(16X,I5)))
        READ(1,*)(A(IRR+2*(I-1)+1), A(IRR+2*(I-1)+2),I=1,NR)
        WRITE(2,608)(A(IRR+2*(I-1)+1), A(IRR+2*(I-1)+2),I=1,NR)
608     FORMAT(/5X,'JOINT RESTRAINTS DATA'&
&         /2X,'  JOINT',10X,'RESTRAINT',&
     &         /(2X,F7.0,10X,F9.3))
        READ(1,*)((A(IPF+4*(I-1)+J),J=1,4),I=1,NCF)
        WRITE(2,609)((A(IPF+4*(I-1)+J),J=1,4),I=1,NCF)
        ! alter 608 to 609
609     FORMAT(/5X,'CONCENTRATIVE FORCED JOINTS DATA'&
&         /2X,'  JOINT',10X,'Fx', 10X,'Fy',10X,'Fz'&
     &         /(2X,F7.0,3(1X,E11.5)))
        RETURN
        END
