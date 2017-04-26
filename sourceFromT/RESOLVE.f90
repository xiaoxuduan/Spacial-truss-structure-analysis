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
