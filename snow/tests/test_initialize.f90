program test_initialize

  use bmif, only: BMI_SUCCESS
  use bmisnowf
  use fixtures, only: status

  implicit none

  character (len=*), parameter :: config_file1 = "test.cfg"

  type (bmi_snow) :: m
  integer :: status1, status2

  status1 = m%initialize(config_file1)
  status = m%finalize()
  if (status1.ne.BMI_SUCCESS) then
     stop 1
  end if

end program test_initialize
