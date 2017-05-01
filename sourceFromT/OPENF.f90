! name is the file name input, 
    SUBROUTINE OPENF(NAME)
    ! change 40 to 4
       CHARACTER NAME*4
         NUM=0
         DO I=1,4
             !count number of non-empty char in name;
          IF(NAME(I:I).NE.' ')NUM=NUM+1
         ENDDO
       OPEN(1,FILE=NAME(1:NUM), STATUS='UNKNOWN')
       OPEN(2,FILE=NAME(1:NUM)//'.RES',STATUS='UNKNOWN')
       OPEN(3,FILE=NAME(1:NUM)//'.ERO',STATUS='UNKNOWN')
       RETURN
    END
