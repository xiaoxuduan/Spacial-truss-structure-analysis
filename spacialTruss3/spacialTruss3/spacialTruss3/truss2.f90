CHARACTER Input*20,OUTPUT*20
	WRITE(*,*) 'Input Data File name:'
	READ (*,*)Input
	OPEN (11,FILE=Input,STATUS='UNKNOWN')
	WRITE(*,*) 'Output File name:'
    READ (*,*)OUTPUT
	OPEN(12,FILE=output,STATUS='UNKNOWN')
	WRITE(12,*)'*****************************************'
	WRITE(12,*)'*    Program for Analysis of Space Trusses    *'
	WRITE(12,*)'*      School of Civil Engineering CSU      *'
	WRITE(12,*)'*      2017.5.6 Designed By Duan Xiaoxu       *'
	WRITE(12,*)'*****************************************'
	WRITE(12,*)' '
    WRITE(12,*)'*****************************************'
    WRITE(12,*)'*************The Input Data****************'
    WRITE(12,*)'*****************************************'
	WRITE(12,100)
	READ(11,*)NF,NP,NE,NM,NR,NCF,ND
	WRITE(12,110)NF,NP,NE,NM,NR,NCF,ND
100 FORMAT(6X,'The General Information'/2X,'NF',5X,'NP',5X,'NE',5X,'NM',5X,'NR',5X,'NCF',5X,'ND')
110 FORMAT(2X,I2,6I7)
	NPF=NF*NP
	NDF=ND*NF   
	CALL ANALYSE(NF,NP,NE,NM,NR,NCF,ND,NPF,NDF)
END
!********************************************************************
!总计算程序
subroutine ANALYSE(NF,NP,NE,NM,NR,NCF,ND,NPF,NDF)
    DIMENSION MM(NE),X(NP),Y(NP),Z(NP),ME(ND,NE),IT(NF,NP),RR(ND,NR), NAE(NE),AE(2,NM),PF(4,NCF),LMT(NDF,NE),MAXA(NPF),CKK(1000),V(NPF),DIST(NPF),PP(NPF),FF(NPF),SG(NE),SM(NE)
    READ(11,*)(X(I),Y(I),Z(I),I=1,NP)
    READ(11,*) (MM(i),i=1,ne)
	READ(11,*)((ME(I,J),J=1,NE),I=1,2)  
    READ(11,*)(NAE(I),I=1,NE)
	READ(11,*)((RR(I,J),J=1,NR),I=1,2)
	READ(11,*)((AE(I,J),J=1,NM),I=1,2)
    IF(NCF/=0)THEN
	    READ(11,*)((PF(I,J),J=1,NCF),I=1,4)
	    WRITE(12,160)
	    WRITE(12,161)(int(PF(1,J)),PF(2,J),PF(3,J),PF(4,J),J=1,NCF)
    endif
	WRITE(12,120)
	WRITE(12,121)(I,X(I),Y(I),Z(I),I=1,NP)
	WRITE(12,130)
	WRITE(12,131)(mm(i),ME(1,I),ME(2,I),NAE(I),I=1,NE)
	WRITE(12,140)
	WRITE(12,141)(INT(RR(1,J)),RR(2,J),J=1,NR)
	WRITE(12,150)
	WRITE(12,*)((AE(I,J),J=1,NM),I=1,2)
120    FORMAT(/6X,'The Information of Joints'/2x,'Joint',5X,'X(mm)',5X,'Y(mm)',5X,'Z(mm)')
121    FORMAT(1X,I4,3F8.1)
130    FORMAT(/6X,'The Information of Members'/2x,'Member',2X,'START',4X,'END',6X,'NAE')
131    FORMAT(I4,3I8)
140    FORMAT(/6X,'The Information of SUPPORTS'/2x,'Joint',5X,'S')
141    FORMAT(1X,I4,F8.3)
150    FORMAT(/6X,'The Information of Sections'/4x,'E0(MPa)',8X,'A0(mm^2)')
!151    FORMAT(1X,1PE8.2,F8.4)
160    FORMAT(/6X,'The Loading at Joints'/2x,'Joint',5X,'Fx(N)',5X,'Fy(N)',7X,'Fz(N)')
161    FORMAT(1X,i4,3F8.2)
	CALL FLMT(NP,NE,NN,NNM,NR,RR,ND,NF,NDF,ME,IT,LMT)
	CALL FMAXA(NNM,NE,LMT,MAXA,NWK,NPF,NDF)
	CALL LP(V,PP,IT,PF,NN,NCF,NF,NP,NPF)
	CALL CONKB(NP,NE,NM,NWK,ME,X,Y,Z,AE,NAE,LMT,MAXA,CKK,NNM)
	ISH=1
	CALL LDLT(CKK,MAXA,NN,ISH,IOUT,NWK,NNM)
	CALL REBACK(CKK,V,MAXA,NN,NWK,NNM)
	CALL DISPLS(NP,NE,NPF,NM,NN,IT,V,DIST,AE,NAE,X,Y,Z,PP,FF,SG,SM,ME,NR,RR,NF)
    CALL CLOSEF()
    WRITE(*,*) 'Calculation finished. Press enter to exit.'
    read(*,*)
END 
!********************************************************************
SUBROUTINE CLOSEF()
    CLOSE(11)
    CLOSE(12)
END
    
!矩阵转置子程序
SUBROUTINE  MAT(M,N,A,B)
  DIMENSION  A(M,N),B(N,M)
  DO I=1,M
    DO J=1,N
       B(J,I)=A(I,J)
    END DO
  END DO
RETURN
END

!单元刚度矩阵的形成
SUBROUTINE  FKE(NP,NE,NM,IE,X,Y,Z,ME,NAE,AE,AKE)
  DIMENSION   X(NP),Y(NP),Z(NP),ME(2,NE),NAE(NE),AE(2,NM) ,AKE(2,2)
  N1=ME(1,IE)
  N2=ME(2,IE)
  X1=X(N1);Y1=Y(N1);Z1=Z(N1)
  X2=X(N2);Y2=Y(N2);Z2=Z(N2)
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

!单元坐标转换矩阵
SUBROUTINE  FT(IE,NP,NE,X,Y,Z,ME,T)
  DIMENSION X(NP),Y(NP),Z(NP),ME(2,NE),T(2,6)
  T=0
  N1=ME(1,IE);N2=ME(2,IE)
  X1=X(N1);Y1=Y(N1);Z1=Z(N1)
  X2=X(N2);Y2=Y(N2);Z2=Z(N2)
  BL=SQRT((X2-X1)**2+(Y2-Y1)**2+(Z2-Z1)**2)
  CX=(X2-X1)/BL
  CY=(Y2-Y1)/BL
  CZ=(Z2-Z1)/BL
  T(1,1)=CX;T(2,4)=CX
  T(1,2)=CY;T(2,5)=CY
  T(1,3)=CZ;T(2,6)=CZ
  RETURN
END

!生成单元联系数组LMT 
SUBROUTINE  FLMT(NP,NE,NN,NNM,NR,RR,ND,NF,NDF,ME,IT,LMT)
  DIMENSION  IT(NF,NP),LMT(NDF,NE),ME(ND,NE),RR(2,NR)
  NN=0;NNM=0;IT=0;LMT=0
  N=0
  DO I=1,NP
   C=0
   DO K=1,NR
      KR=RR(1,K)
      IF(KR.EQ.I)  C=RR(2,K)
   ENDDO
   NC=C	          !NC=0,提取了整数部分
   C=C-NC          !C=0.***,例如C=0.111
   DO J=1,NF
      C=C*10.0      !例如C=1.21
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
  NNM=NN+1
  DO IE=1,NE
   DO I=1,ND
      NI=ME(I,IE)
      DO J=1,NF
         LMT((I-1)*NF+J,IE)=IT(J,NI)
      ENDDO
   ENDDO
  ENDDO
  RETURN
END

!二维总刚中对角线元地址数组
SUBROUTINE FMAXA(NNM,NE,LMT,MAXA,NWK,NPF,NDF)
  DIMENSION MAXA(NPF),LMT(NDF,NE)
  MAXA=0;NWK=0
  MAXA(1)=1
  DO I=2,NNM
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
  NWK= MAXA(NNM)-1
  RETURN
END

!生成一维存储结构总刚度矩阵
SUBROUTINE CONKB(NP,NE,NM,NWK,ME,X,Y,Z,AE,NAE,LMT,MAXA,CKK,NNM)
    DIMENSION CKK(NWK),X(NP),Y(NP),Z(NP),AE(2,NM),NAE(NE),LMT(6,NE),ME(2,NE),MAXA(NNM),AK(6,2),AKE(2,2),T(2,6),TT(6,2),TAK(6,6)
    CKK=0
    DO 10 IE=1,NE
        TAK=0
        CALL FKE(NP,NE,NM,IE,X,Y,Z,ME,NAE,AE,AKE)
        CALL FT(IE,NP,NE,X,Y,Z,ME,T)
        CALL MAT(2,6,T,TT)
        AK=MATMUL(TT,AKE)
        TAK=MATMUL(AK,T)       !总体坐标系下的单元刚度矩阵 
        DO 220 I=1,6
        DO 220 J=1,6
            NI=LMT(I,IE)
            NJ=LMT(J,IE)
            IF((NJ-NI).GE.0.AND.NI*NJ.GT.0) THEN
                IJ=MAXA(NJ)+NJ-NI
                CKK(IJ)=CKK(IJ)+TAK(I,J)
            ENDIF
220     CONTINUE
10      CONTINUE
    RETURN
END
    
!生成荷载矩阵
SUBROUTINE LP(V,PP,IT,PF,NN,NCF,NF,NP,NPF)
  DIMENSION V(NN),PP(NPF),IT(NF,NP),PF(4,NCF)
  V=0
  PP=0
  DO I=1,NF
    DO J=1,NP
	  DO K=1,NCF
	    IF(J.EQ.PF(1,K).AND.IT(I,J).NE.0)THEN
	V(IT(I,J))=PF(I+1,K)
	ENDIF
	  ENDDO
	ENDDO
  ENDDO
  DO K=1,NCF
    DO I=1,NP
       IF(I.EQ.PF(1,K))THEN
	 PP(NF*(I-1)+1)=PF(2,K)
         PP(NF*(I-1)+2)=PF(3,K)
	 PP(NF*(I-1)+3)=PF(4,K)
        ENDIF
    ENDDO
  ENDDO
  RETURN
END

!对一维结构总刚度矩阵进行矩阵分解（LDLT）
SUBROUTINE LDLT(A,MAXA,NN,ISH,IOUT,NWK,NNM)
    DIMENSION A(NWK),MAXA(NNM)
    IF(NN.EQ.1) RETURN
    DO 200 N=1,NN
        KN=MAXA(N)
        KL=KN+1
        KU=MAXA(N+1)-1
        KH=KU-KL
        IF(KH)304,240,210
210        K=N-KH
        IC=0
        KLT=KU
        DO 260 J=1,KH
        KLT=KLT-1
        IC=IC+1
        KI=MAXA(K)
        ND=MAXA(K+1)-KI-1
        IF(ND) 260,260,270
270        KK=MIN0(IC,ND)
        C=0.0
        DO 280 L=1,KK
280        C=C+A(KI+L)*A(KLT+L)
        A(KLT)=A(KLT)-C
260        K=K+1
240        K=N
        B=0.0
        DO 300 KK=KL,KU
        K=K-1
        KI=MAXA(K)
        C=A(KK)/A(KI)
        IF(ABS(C).LT.1.0E+07) GOTO 290
        WRITE(IOUT,2010) N,C
        STOP
290      B=B+C*A(KK)
300      A(KK)=C
        A(KN)=A(KN)-B
304      IF(A(KN)) 310,310,200
310      IF(ISH.EQ.0) GOTO 320
        IF(A(KN).EQ.0.0) A(KN)=-1.0E-16
        GOTO 200
320      WRITE(IOUT,2000) N,A(KN)
        STOP
200     CONTINUE
    RETURN
2000	FORMAT(//,' Stop-stiffness matrix not positive + definite',//,'no positive& pivot for equation',I4,//,' pivot =',E20.10)
2010	FORMAT(//,' Stop-strum sequence check failed + because of multiplier& growth for column + number',I4,//,' Multiplier = ',E20.8)
END
    
!回代，求得节点位移
SUBROUTINE REBACK(A,V,MAXA,NN,NWK,NNM)
    DIMENSION A(NWK),V(NN,1),MAXA(NNM)
    NIP=1
    DO IP=1,NIP
        DO 400 N=1,NN
            KL=MAXA(N)+1
            KU=MAXA(N+1)-1
            IF(KU-KL) 400,410,410
410            K=N
            C=0.0
        DO 420 KK=KL,KU
            K=K-1
420          C=C+A(KK)*V(K,IP)
            V(N,IP)=V(N,IP)-C
400       CONTINUE
        DO 480 N=1,NN
            K=MAXA(N)
480          V(N,IP)=V(N,IP)/A(K)
            IF(NN.EQ.1)RETURN
            N=NN
        DO 500 L=2,NN
            KL=MAXA(N)+1
            KU=MAXA(N+1)-1
            IF(KU-KL) 500,510,510
510          K=N
        DO 520 KK=KL,KU
            K=K-1
520          V(K,IP)=V(K,IP)-A(KK)*V(N,IP)
500          N=N-1
    ENDDO
    RETURN
END

!求解杆件内力、支反力和位移
SUBROUTINE DISPLS(NP,NE,NPF,NM,NN,IT,FTOOL,DIST,AE,NAE, X,Y, Z,PP,FF,SG,SM,ME,NR,RR,NF)
    DIMENSION IT(3,NP),DIST(NPF),FTOOL(NPF),X(NP),Y(NP),Z(NP),T(2,6),TT(6,2), AE(2,NM),ME(2,NE),NAE(NE), UE(6),U(2),AKE(2,2),FE1(2),FE(6),FF(NPF),PP(NPF), SG(NE),SM(NE),FF2(NPF),RR(2,NR),FL(3*NR)
    SG=0;SM=0;FF=0;FF2=0
    DO I=1,NP
        DO J=1,NF
        LAB=IT(J,I)
        IF(LAB.EQ.0) THEN
        DIST(3*(I-1)+J)=0.0
        ELSEIF(LAB.GT.0.AND.LAB.LE.NN) THEN
        DIST(3*(I-1)+J)=FTOOL(LAB)
        ENDIF
        ENDDO
    ENDDO
    DO IE=1,NE
        N1=ME(1,IE);N2=ME(2,IE)
        DO J=1,NF
        UE(J)=DIST(3*(N1-1)+J)
        UE(3+J)=DIST(3*(N2-1)+J)
        ENDDO
        CALL FT(IE,NP,NE,X,Y,Z,ME,T)
        CALL FKE(NP,NE,NM,IE,X,Y,Z,ME,NAE,AE,AKE)
        U=MATMUL(T,UE)
        FE1=MATMUL(AKE,U)
        CALL MAT(2,6,T,TT)
        FE=MATMUL(TT,FE1)
        DO J=1,NF
        FF(3*(N1-1)+J)=FF(3*(N1-1)+J)+FE(J)
        FF(3*(N2-1)+J)=FF(3*(N2-1)+J)+FE(3+J)
        ENDDO
        ISW=NAE(IE)
        AO=AE(2,ISW)
        SG(IE)=FE1(2)
        SM(IE)=FE1(2)/AO
        DO I=1,NPF
        FF2(I)=FF(I)-PP(I)
        ENDDO
    ENDDO
        
    !add k=0;
    k=0
    DO I=1,NP
	    DO J=1,NF
	    LAB=IT(J,I)
	        IF(LAB.EQ.0)THEN
	        K=K+1
	        FL(K)=FF2(3*(I-1)+J)
	        ENDIF
	    ENDDO
    ENDDO
    WRITE(12,*)' '
    WRITE(12,*)'****************************************'
    WRITE(12,*)'*********The Results of Calculation**********'
    WRITE(12,*)'****************************************'	      
	WRITE(12,600)
    WRITE(12,610)(I,DIST(3*I-2),DIST(3*I-1),&
    DIST(3*I), I=1,NP)
    WRITE(12,620)
    WRITE(12,630)(IE,SG(IE),SM(IE),IE=1,NE)
	write(3,*) 'sg'
	write(3,*) sg
	write(3,*) 'sm'
	write(3,*) sm
    WRITE(12,640)
    WRITE(12,650)(INT(RR(1,I)),FL(3*I-2),FL(3*I-1),FL(3*I),I=1,NR)
600     FORMAT(6X,'The Joint Displacement'/2x,'Joint',6X,'X(mm)',8X,'Y(mm)',6X,'Z(mm)')
610     FORMAT(1X,I4,2X,1P3E12.2)
620     FORMAT(//6X,'The Terminal Forces'/2x,'Member', 6X,'FN(N)',6X,'σ(MPa)')
630     FORMAT(3X,I4,2X,F8.2,6X,F8.2)
640     FORMAT(//6X,'The Bearing Force'/2x,'Joint',8X,'Fx(N)',8X,'Fy(N)',8X,'Fz(N)')
650     FORMAT(2X,I4,2X,3F10.2)  
    RETURN
END