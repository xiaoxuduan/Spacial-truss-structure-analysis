SUBROUTINE  MAT（M,N,A,B）
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
