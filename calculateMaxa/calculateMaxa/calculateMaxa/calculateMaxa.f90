!  calculateMaxa.f90 
!
!  FUNCTIONS:
!  calculateMaxa - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: calculateMaxa
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

    program calculateMaxa

    !implicit none

    ! Variables
    !integer :: NDF,NE,npf    !real, dimension(48) :: maxa
    !real, dimension(6,30) :: lmt

    DIMENSION MAXA(48),LMT(6,30)
    ! Body of calculateMaxa
    !print *, 'Hello World'
    	!WRITE(*,*) 'Input Data File name:'
	    !READ (*,*)Input
	    OPEN (11,FILE='input',STATUS='UNKNOWN')
	    WRITE(*,*) 'Output File name:'
	    !READ (*,*)OUTPUT
	    OPEN(12,FILE='output',STATUS='UNKNOWN')

    nnm=38+1
    ne=30
    nd=3
    nf=2
    NP=24
    npf=nF*nP
    NDF=ND*NF
    
    read(11,*) ((lmt(I,J),J=1,NE),I=1,ndf) 
    !read(11,*) (maxa(i),I=1,npf) 
            write(12,*) 'lmt'
        write(12,*) lmt
        write(12,*) lmt(1,30),lmt(2,29)
    calL fmaxa(nnm,ne,lmt,maxa,nwk,npf,ndf)
    write(12,*) 'lmt'
    write(12,*) ((lmt(I,J),J=1,NE),I=1,ndf) 
    write(12,*) 'maxa'
    write(12,*) (maxa(i),I=1,npf) 
    write(12,*) 
    end program calculateMaxa

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