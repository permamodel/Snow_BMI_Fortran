C
C To change this license header, choose License Headers in Project Properties.
C To change this template file, choose Tools | Templates
C and open the template in the editor.
C

C
C File:   main.f
C Author: kangwang
C
C Created on March 25, 2019, 1:09 PM
      
      use snow_model
      type (snow_model_type) :: model
      
      character*256 fconfig 
      
       IF (COMMAND_ARGUMENT_COUNT() .EQ. 0) THEN
        fconfig = 'snow_model_test.cfg'
       ELSE
        CALL GET_COMMAND_ARGUMENT(1, fconfig)
       ENDIF
            
      call initialize(model, fconfig)
      
      print *, model%ICL, model%IOPEN, MODEL%PADJ, MODEL%SIXHRS
      
      open(301, file = 'snow.csv')
      
!      print *, MODEL%N_TIME_STEPS

      DO i = 1, MODEL%N_TIME_STEPS
!      
      call update(model)
      !     
      WRITE(301,'(F9.1,A1,F8.2,A1,F8.1)') MODEL%T,',', MODEL%NEW,',', 
     *  MODEL%NEWP
      WRITE(*  ,'(F0.1,A1,F0.2,A1,F0.1)') MODEL%T,',', MODEL%NEW,',', 
     *  MODEL%NEWP
!      
      ENDDO
      
      close(301)
            
      call finalize(model)

      PRINT*, 'Hello World'
      
      END