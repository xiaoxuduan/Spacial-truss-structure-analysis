        !C TRUSS.FOR
        !C
        !*******************************************************************************
        !*            SPACIAL TRUSS STRUCTURE ANALYSIS                           *
        !*******************************************************************************
               PROGRAM TRUSS
               IMPLICIT REAL*8 (A-H,O-Z)
               IMPLICIT INTEGER*4 (I-N)
               CHARACTER NAME*40
               !change 18000000 to 1800
               COMMON /AT/A(18000000)
               !change 2000000 to 2000
               COMMON /IAT/IA(2000000)
               A=0;IA=0
               WRITE(*,*)'INPUT FILE NAME?';READ(*,*)NAME;
               CALL OPENF(NAME)
               
               write(4,*) 'before datain'
               CALL DATAIN(NP,NE,NF,ND,NDF,NPF,NM,NR,NCF,
     &   IME,INAE,IIT,ILMT,IMAXA,
     &   IX,IY,IZ,IRR,IAE,IPF,ICKK,IDIST,IFTOOL,IFF,IPP,ISG,ISM,NWK)
               
               write(4,*) 'before flmt'
                   ! change RR TO A(IRR+1), Page 41;
               CALL FLMT(NP,NE,NN,NN1,NR,A(IRR+1),ND,NF,NDF,
     &   IA(IME+1),IA(IIT+1),IA(ILMT+1))
               !add ICKK
        CALL FMAXA(NN1,NE,IA(ILMT+1),IA(IMAXA+1),NWK,NPF,NDF,ICKK)
        
        write(4,*) 'before conkb'
               CALL CONKB(NP,NE,NM,NWK,IA(IME+1),A(IX+1),
     &   A(IY+1),A(IZ+1),A(IAE+1),IA(INAE+1),
     &   IA(ILMT+1),IA(IMAXA+1),A(ICKK+1),NN1)
               
               write(4,*) 'before mkforce'
        CALL MKFORCE(NP,NF,NPF,NCF,NN,
     &   IA(IIT+1),A(IPF+1),A(IPP+1),A(IFTOOL+1))
        write(4,*) 'before ldlt'
         CALL LDLT(A(ICKK+1),IA(IMAXA+1),NN,0,3,NWK,NN1)
         write(4,*) 'before mkforce'
         CALL RESOLVE(A(ICKK+1),A(IFTOOL+1),IA(IMAXA+1),NN,NWK,NN1)
         
         write(4,*) 'before displs'
         write(4,*) idist
        CALL DISPLS(NP,NE,NF,NPF,NM,NN,IA(IIT+1),A(IFTOOL+1),
     &  A(IDIST+1),A(IAE+1),IA(INAE+1),A(IX+1),A(IY+1),A(IZ+1),
     &  A(IPP+1),A(IFF+1),A(ISG+1),A(ISM+1))
        ! CALL DATOUT(18 parameters) while only 7 parameters in DATOUT();
        ! SUBROUTINE DATAOUT(NP,NE,NPF,DIST,FF,SG,SM)
        !CALL DATAOUT(NP,NE,NF,NPF,NM,NN,IA(IIT+1),A(IFTOOL+1),&
        !&  A(IDIST+1),A(IAE+1),IA(INAE+1),A(IX+1),A(IY+1),A(IZ+1),&
        !&  A(IPP+1),A(IFF+1),A(ISG+1),A(ISM+1))
        write(4,*) 'before dataout'
        CALL DATAOUT(NP,NE,NPF,
     &  A(IDIST+1),
     &  A(IFF+1),A(ISG+1),A(ISM+1))
               CALL CLOSEF
               
               read(*,*)
               END

        !C
               SUBROUTINE OPENF(NAME)
               CHARACTER NAME*40
                 NUM=0
                 DO I=1,40
                  IF(NAME(I:I).NE.' ')NUM=NUM+1
                 ENDDO
               OPEN(1,FILE=NAME(1:NUM), STATUS='UNKNOWN')
               OPEN(2,FILE=NAME(1:NUM)//'.RES',STATUS='UNKNOWN')
               OPEN(3,FILE=NAME(1:NUM)//'.ERO',STATUS='UNKNOWN')
               
               OPEN(4,FILE='test.txt',STATUS='UNKNOWN')

               RETURN
               END
        !C
        !C
               SUBROUTINE CLOSEF
               CLOSE(1)
               CLOSE(2)
               CLOSE(3)
               RETURN
               END
        !C
              !add NWK
               SUBROUTINE DATAIN(NP,NE,NF,ND,NDF,NPF,NM,NR,NCF,
     &   IME,INAE,IIT,ILMT,IMAXA,
     &   IX,IY,IZ,IRR,IAE,IPF,ICKK,IDIST,IFTOOL,IFF,IPP,ISG,ISM,NWK)
                IMPLICIT REAL*8 (A-H,O-Z)
                IMPLICIT INTEGER*4 (I-N)
                COMMON /AT/A(18000000)
                COMMON /IAT/IA(2000000)
               
                write(4,*) ICKK, NWK , IDIST
                READ(1,*)NP,NE,NM,NR,NCF
                WRITE(2,701)NP,NE,NM,NR,NCF
  701           FORMAT(//1X,'###OUTPUT OF ORIGINAL INPUT INFORMATION###'
     &  //5X,'Number of joints                             JOINTS=',I5
     &   /5X,'Number of elements                           ELEMENTS=',I5
     &   /5X,'Number of material property groups    PROPERTY TYPES=',I5
     &   /5X,'Number of restrained joints               RESTRAINTS=',I5
     &   /5X,'Number of concentrative forced joints           NCF=',I5)
        !C--------------FORM POINTER----------------------------------
                 ! ADD 
                ! NWK/ 结构总刚度矩阵的大小?=无约束自由度个数的平方？
                ! below only hold  节点约束类型为固定约束
                NWK=(NP-NR)*3*(NP-NR)*3
                
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
                
                write(4,*) ICKK, NWK , IDIST
                IDIST=ICKK+NWK
                write(4,*) ICKK, NWK , IDIST
                
                IFTOOL=IDIST+NPF
                IFF=IFTOOL+NPF
                IPP=IFF+NPF
                ISG=IPP+NPF
                ISM=ISG+NE
                READ(1,*)(A(IX+I),A(IY+I),A(IZ+I),I=1,NP)
                WRITE(2,714)(I,A(IX+I),A(IY+I),A(IZ+I),I=1,NP)
  714           FORMAT(//5X,'GENERATED JOINT COORDINATES DATA'
     &        /1X,'  JOINT  ',15X,'X',13X,'Y',13X,'Z'
     &  /(4X,I5,3X,3(2X,E12.6)))
                !write(2,*) 'flag1'
         READ(1,*)(A(IAE+2*(I-1)+1), A(IAE+2*(I-1)+2),I=1,NM)
        READ(1,*)(IA(IME+2*(I-1)+1),IA(IME+2*(I-1)+2),IA(INAE+I),I=1,NE)
         WRITE(2,606)(I,A(IAE+2*(I-1)+1), A(IAE+2*(I-1)+2),I=1,NM)
        WRITE(2,607)(I,IA(IME+2*(I-1)+1),IA(IME+2*(I-1)+2),IA(INAE+I),
     &     I=1,NE)
  606     FORMAT(/5X,'ELEMENT MATERAIL PROPERTIES DATA'
     &         /2X,'NO.',10X,' E',10X,'Ax'
     &         /(2X,I3,2(1X,E11.5)))
  607  FORMAT(/5X,'TRUSS ELEMENT DEFINITION DATA'
     &         /2X,'NO.',10X,'JOINT_1',10X,'JOINT_2',10x,'element type'
     &         /(2X,I3,3(16X,I5)))
              !write(2,*) 'flag2'
                READ(1,*)(A(IRR+2*(I-1)+1), A(IRR+2*(I-1)+2),I=1,NR)
                WRITE(2,608)(A(IRR+2*(I-1)+1), A(IRR+2*(I-1)+2),I=1,NR)
  608           FORMAT(/5X,'JOINT RESTRAINTS DATA'
     &         /2X,'  JOINT',10X,'RESTRAINT',
     &         /(2X,F7.0,10X,F9.3))
                !write(2,*) 'flag4'
                READ(1,*)((A(IPF+4*(I-1)+J),J=1,4),I=1,NCF)
                WRITE(2,609)((A(IPF+4*(I-1)+J),J=1,4),I=1,NCF)
  609           FORMAT(/5X,'CONCENTRATIVE FORCED JOINTS DATA'
     &         /2X,'  JOINT',10X,'Fx', 10X,'Fy',10X,'Fz'
     &         /(2X,F7.0,3(1X,E11.5)))
                write(4,*) 'flag3'
                read(*,*)
                RETURN
        END
        !C
        SUBROUTINE MKFORCE(NP,NF,NPF,NCF,NN,IT,PF,PP,FTOOL)
        IMPLICIT REAL*8 (A-H,O-Z)
        IMPLICIT INTEGER*4 (I-N)
        DIMENSION IT(NF,NP),PF(4,NCF),PP(NPF), FTOOL(NPF)
               PP=0;FTOOL=0
               DO I=1,NCF
                NOD=PF(1,I)
                DO J=1,NF
                 PP(NF*(NOD-1)+J)=PF(J+1,I)
                ENDDO
               ENDDO
               DO I=1,NP
                 DO J=1,NF
                  LAB=IT(J,I)
                  IF(LAB.GT.0.AND.LAB.LE.NN) THEN
                   FTOOL(LAB)=PP(NF*(I-1)+J)
                  ENDIF
                 ENDDO
               ENDDO
               RETURN
               END
        !C
        !Ccc
        SUBROUTINE DATAOUT(NP,NE,NPF,DIST,FF,SG,SM)
        IMPLICIT REAL*8 (A-H,O-Z)
        IMPLICIT INTEGER*4 (I-N)
        DIMENSION DIST(NPF),FF(NPF),SG(NE),SM(NE)
                WRITE(2,715)(I,(DIST(6*(I-1)+J),J=1,3),I=1,NP)
  715   FORMAT(//5X,'SOLVED JOINT DISPLACEMENTS DATA'
     &        /1X,'  JOINT  ',3X,12X,'Dx',12X,'Dy',12X,'Dz'
     &  /(4X,I5,3X,3(2X,E12.6)))
        WRITE(2,716)(IE,SG(IE),SM(IE),IE=1,NE)
  716   FORMAT(//5X,'SOLVED ELEMENT INTERNAL FORCE DATA'
     &        /1X,'  ELEMENT ',3X,12X,'Nx',8X,'STRESS'
     &  /(4X,I5,3X,3(2X,E12.6)))
        WRITE(2,717)(I,(FF(6*(I-1)+J),J=1,3),I=1,NP)
 717   FORMAT(//5X,'SOLVED JOINT REACTION DATA'
     &        /1X,'  JOINT  ',3X,12X,'Rx',12X,'Ry',12X,'Rz'
     &  /(4X,I5,3X,3(2X,E12.6)))
               RETURN
               END
        !C
        !C
          SUBROUTINE  FLMT(NP,NE,NN,NN1,NR,RR,ND,NF,NDF,ME,IT,LMT)
          IMPLICIT  REAL*8(A-H,O-Z)
          IMPLICIT  INTEGER*4(I-N)
        !C  This program  forms  the  joint&element  numbering  matrix  IT&LMT
          DIMENSION  IT(NF,NP),LMT(NDF,NE),ME(ND,NE),RR(2,NR)
          NN=0;NN1=0;IT=0;LMT=0
          write(*,*) 'flmt begin'
          N=0
          DO  I=1,NP
           C=0
           DO  K=1,NR
        KR=RR(1,K)
        IF(KR.EQ.I)  C=RR(2,K)
           ENDDO
           NC=C
           C=C-NC
           DO  J=1,NF
        C=C*10.0
        L=C+0.1
        C=C-L
        IF(L.EQ.0)THEN
         N=N+1
         IT(J,I)=N
        ELSE
         IT(J,I)=0
        ENDIF
        ENDDO
          ENDDO
          NN=N
          NN1=NN+1
          DO  IE=1,NE
           DO  I=1,ND
        NI=ME(I,IE)
        DO  J=1,NF
         LMT((I-1)*NF+J,IE)=IT(J,NI)
        ENDDO
           ENDDO
          ENDDO
          write(*,*) 'flmt end'
          RETURN
          END
        !C NN1 LMT NPF=NF*NP NDF=ND*NF


        SUBROUTINE FMAXA(NN1,NE,LMT,MAXA,NWK,NPF,NDF,ICKK)
        !C This program forms the MDE address matrix MAXA of [K]
        IMPLICIT REAL*8 (A-H,O-Z)
        IMPLICIT INTEGER*4 (I-N)
        DIMENSION MAXA(NPF),LMT(NDF,NE)
        MAXA=0;NWK=O
        ! what
        ! change to ICKK+1
        !MAXA(1)=1
        MAXA(1)=ICKK+1
        DO I=2,NN1
         IP=I-1
         IG=IP
         DO IE=1,NE
          DO J=1,NDF
           IF(LMT(J,IE).EQ.IP) THEN
        DO K=1,NDF
               IF(LMT(K,IE).GT.0.AND.LMT(K,IE).LE.IG) IG=LMT(K,IE)
        ENDDO
           END IF
          ENDDO
         ENDDO
         MAXA(I)= MAXA(I-1)+IP-IG+1
        ENDDO
        NWK= MAXA(NN1)-1
        RETURN
        END
        !C
              SUBROUTINE CONKB(NP,NE,NM,NWK,ME,X,Y,Z,AE,NAE,
     &         LMT,MAXA,CKK,NN1)
                IMPLICIT REAL*8 (A-H,O-Z)
                IMPLICIT INTEGER*4 (I-N)
                DIMENSION CKK(NWK),X(NP),Y(NP),Z(NP),AE(2,NM),
     &  NAE(NE),LMT(6,NE),ME(2,NE),MAXA(NN1),
     &  AKE(2,2),T(2,6),TT(6,2),TAK(6,6)
                
                write(4,*) MAXA
              CKK=0
              DO 10 IE=1,NE
                 TAK=0
                 CALL FKE(NP,NE,NM,IE,X,Y,Z,ME,NAE,AE,AKE)
                 CALL FT(IE,NP,NE,X,Y,Z,ME,T)
                 CALL MAT(2,6,T,TT)
                 TAK=matmul(matmul(TT,AKE),T)
                 DO 220 I=1,6
                 DO 220 J=1,6
                  NI=LMT(I,IE)
                  NJ=LMT(J,IE)
                  IF((NJ-NI).GE.0.AND.NI*NJ.GT.0) THEN
                   IJ=MAXA(NJ)+NJ-NI
                   CKK(IJ)=CKK(IJ)+TAK(I,J)
                  ENDIF
  220    CONTINUE
   10   CONTINUE
              RETURN
              END
        !C
                SUBROUTINE LDLT(A,MAXA,NN,ISH,IOUT,NWK,NNM)
                IMPLICIT REAL*8(A-H,O-Z)
                IMPLICIT INTEGER*4 (I-N)
                DIMENSION A(NWK),MAXA(NNM)
                
                write(4,*) 'ldlt begin'
                IF(NN.EQ.1) RETURN
                DO 200 N=1,NN
                 KN=MAXA(N)
                 KL=KN+1
                 KU=MAXA(N+1)-1
                 KH=KU-KL
                 IF(KH)304,240,210
  210      K=N-KH
            IC=0
            KLT=KU
            DO 260 J=1,KH
            KLT=KLT-1
            IC=IC+1
            KI=MAXA(K)
            ND=MAXA(K+1)-KI-1
            IF(ND) 260,260,270
270      KK=MIN0(IC,ND)
            C=0.0
            DO 280 L=1,KK
280      C=C+A(KI+L)*A(KLT+L)
            A(KLT)=A(KLT)-C
260      K=K+1
240      K=N
            B=0.0
            DO 300 KK=KL,KU
            K=K-1
            KI=MAXA(K)
            C=A(KK)/A(KI)
            IF(ABS(C).LT.1.0E+07) GOTO 290
            WRITE(IOUT,2010) N,C
            !STOP
290      B=B+C*A(KK)
300      A(KK)=C
            A(KN)=A(KN)-B
304      IF(A(KN)) 310,310,200
310      IF(ISH.EQ.0) GOTO 320
            IF(A(KN).EQ.0.0) A(KN)=-1.0E-16
            GOTO 200
320      WRITE(IOUT,2000) N,A(KN)
            !STOP
200     CONTINUE
        
2000    FORMAT(//' Stop-stiffness matrix not positive    
     &   definite',//,'nonpositive pivot for equation',
     &   I4,//,' pivot =',E20.10)
2010   FORMAT(//,' Stop-sturm sequence check failed 
     &   because of multiplier growth for column 
     &   number',I4,//,  ' Multiplier = ',E20.8)
     
        write(4,*) 'ldlt end'
        RETURN
              END
        !C
        !C
        SUBROUTINE RESOLVE(A,V,MAXA,NN,NWK,NNM)
        IMPLICIT REAL*8(A-H,O-Z)
        IMPLICIT INTEGER*4 (I-N)
        DIMENSION A(NWK),V(NN,1),MAXA(NNM)
        NIP=1
        DO IP=1,NIP
        DO 400 N=1,NN
        KL=MAXA(N)+1
        KU=MAXA(N+1)-1
        IF(KU-KL) 400,410,410
410     K=N
        C=0.0
        DO 420 KK=KL,KU
        K=K-1
420     C=C+A(KK)*V(K,IP)
        V(N,IP)=V(N,IP)-C
400     CONTINUE
        DO 480 N=1,NN
        K=MAXA(N)
480     V(N,IP)=V(N,IP)/A(K)
        IF(NN.EQ.1)RETURN
        N=NN
        DO 500 L=2,NN
        KL=MAXA(N)+1
        KU=MAXA(N+1)-1
        IF(KU-KL) 500,510,510
510     K=N
        DO 520 KK=KL,KU
        K=K-1
520     V(K,IP)=V(K,IP)-A(KK)*V(N,IP)
500     N=N-1
              ENDDO
              RETURN
              END
        !C
        !Ccc
        SUBROUTINE DISPLS(NP,NE,NF,NPF,NM,NN,IT,FTOOL,
     & DIST,AE,NAE,X,Y,Z,PP,FF,SG,SM)
                IMPLICIT REAL*8 (A-H,O-Z)
                IMPLICIT INTEGER*4 (I-N)
                DIMENSION IT(NF,NP),DIST(NPF),FTOOL(NPF),T(2,6),
     & TT(6,2),AE(2,NM),ME(2,NE),NAE(NE),UE(6),U(2), 
     & AKE(2,2),FE1(2),FE(6),FF(NPF),PP(NPF),SG(NE),SM(NE),
     & X(NP),Y(NP) ,Z(NP)
                write(4,*) dist
               SG=0;SM=0;FF=0
               DO I=1,NP
                 DO J=1,NF
                  LAB=IT(J,I)
                  IF(LAB.EQ.0) THEN
                   DIST(NF*(I-1)+J)=0.0
                  ELSEIF(LAB.GT.0.AND.LAB.LE.NN) THEN
                      write(4,*) ftool(lab)
                      write(4,*) 427
                   DIST(NF*(I-1)+J)=FTOOL(LAB)
                   write(4,*) DIST(NF*(I-1)+J)
                   write(4,*) 430
                  ENDIF
                 ENDDO
               ENDDO
               DO IE=1,NE
                 N1=ME(1,IE);N2=ME(2,IE)
                 DO J=1,NF
                UE(J)=DIST(NF*(N1-1)+J)
        UE(NF+J)=DIST(NF*(N2-1)+J)
                 ENDDO
                 CALL FT(IE,NP,NE,X,Y,Z,ME,T)
                 CALL FKE(NP,NE,NM,IE,X,Y,Z,ME,NAE,AE,AKE)
                 U=matmul(T,UE)
                 FE1=matmul(AKE,U)
                 CALL MAT(2,6,T,TT)
                 FE=matmul(TT,FE1)
                 DO J=1,NF
                  FF(NF*(N1-1)+J)=FF(NF*(N1-1)+J)+FE(J)
                  FF(NF*(N2-1)+J)=FF(NF*(N2-1)+J)+FE(NF+J)
                 ENDDO
                 ISW=NAE(IE)
                 AO=AE(2,ISW)
                 SG(IE)=FE1(2)
                 SM(IE)=FE1(2)/AO
               ENDDO
               DO I=1,NPF
                  FF(I)=FF(I)-PP(I)
               ENDDO
               RETURN
      END
        !C
      
      ! ADD
                SUBROUTINE  FKE(NP,NE,NM,IE,X,Y,Z,ME,NAE,AE,AKE)
                IMPLICIT    REAL*8(A-H,O-Z)
                IMPLICIT  INTEGER*4(I-N)
                DIMENSION   X(NP),Y(NP) ,Z(NP) ,ME(2,NE) ,
     &               NAE(NE),AE(2,NM) ,AKE(2,2)
                N1=ME(1,IE)
                N2=ME(2,IE)
                X1=X(N1); Y1=Y(N1); Z1=Z(N1)
                X2=X(N2); Y2=Y(N2); Z2=Z(N2)
                BL=SQRT((X2-X1)**2+(Y2-Y1)**2+(Z2-Z1)**2)
                NMI=NAE(IE)
                E0=AE(1,NMI);A0=AE(2,NMI)
                C=E0*A0/BL
                AKE(1,1)=C 
                AKE(1,2)=-C
                AKE(2,1)=-C
                AKE(2,2)=C
                RETURN
      END
      
      ! ADD
               SUBROUTINE  FT(IE,NP,NE,X,Y,Z,ME,T)
               IMPLICIT  REAL*8(A-H,O-Z)
               IMPLICIT  INTEGER*4(I-N)
                DIMENSION X(NP),Y(NP) ,Z(NP) ,ME(2,NE) ,T(2,6)
                T=0
                N1=ME(1,IE); N2=ME(2,IE)
                X1=X(N1); Y1=Y(N1); Z1=Z(N1)
                X2=X(N2); Y2=Y(N2); Z2=Z(N2)
                BL=SQRT((X2-X1)**2+(Y2-Y1)**2+(Z2-Z1)**2)
                CX=(X2-X1)/BL
                CY=(Y2-Y1)/BL
                CZ=(Z2-Z1)/BL
                T(1,1)=CX; T(2,4)=CX
                T(1,2)=CY; T(2,5)=CY
                T(1,3)=CZ; T(2,6)=CZ
               RETURN
      END

              !ADD
          SUBROUTINE  MAT(M,N,A,B)
	    IMPLICIT  REAL*8(A-H,O-Z)
	    IMPLICIT  INTEGER*4(I-N)
	    DIMENSION  A(M,N),B(N,M)
	    DO I = 1,M
	    DO J = 1,N
	      B(J,I) = A(I,J)
	    END DO
	    END DO
	    RETURN
          END

