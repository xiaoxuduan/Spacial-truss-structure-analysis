SUBROUTINE LDLT(A,MAXA,NN,ISH,IOUT,NWK,NNM)
        IMPLICIT REAL*8(A-H,O-Z)
        IMPLICIT INTEGER*4 (I-N)
        DIMENSION A(NWK),MAXA(NNM)
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
        !RETURN
        ! alter 2001 to 2000
2000      FORMAT(//' Stop-stiffness matrix not positive    + definite',//,'nonpositive pivot for equation',&
&I4,//,' pivot =',E20.10)
!alter 2011 to 2010
2010    FORMAT(//,' Stop-sturm sequence check failed &
& because of multiplier growth for column &
& number',I4,//,  ' Multiplier = ',E20.8)

    return
      END
