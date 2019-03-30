PROGRAM main

use snow_model
type (snow_model_type) :: model

character*256 fconfig

INTEGER I

real,allocatable :: snod(:), time(:)

IF (COMMAND_ARGUMENT_COUNT() .EQ. 0) THEN
    fconfig = 'snow_model_test.cfg'
ELSE
    CALL GET_COMMAND_ARGUMENT(1, fconfig)
ENDIF

call initialize(model, fconfig)

allocate(snod(MODEL % N_TIME_STEPS))
allocate(time(MODEL % N_TIME_STEPS))

print *, model % ICL, model % IOPEN, MODEL % PADJ, MODEL % SIXHRS

open(301, file = 'snow.csv')

!      print *, MODEL%N_TIME_STEPS

DO i = 1, MODEL % N_TIME_STEPS
    !      
    call update(model)
    
    time(i) = MODEL%t
    snod(I) = MODEL%NEW
    !     
    WRITE(301, '(F9.1,A1,F8.2,A1,F8.1)') MODEL % T, ',', MODEL % NEW, ',', &
    MODEL % NEWP
!    WRITE(*, '(F0.1,A1,F0.2,A1,F0.1)') MODEL % T, ',', MODEL % NEW, ',', &
!    MODEL % NEWP
    !      
ENDDO

close(301)

call finalize(model)

END