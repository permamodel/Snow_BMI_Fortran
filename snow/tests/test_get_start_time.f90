program test_get_start_time

  use bmif_1_2, only: BMI_FAILURE
  use bmisnowf
  use fixtures, only: config_file, status

  implicit none

  double precision, parameter :: expected_time = 0.d0

  type (bmi_snow) :: m
  double precision :: start_time

  status = m%initialize(config_file)
  status = m%get_start_time(start_time)
  status = m%finalize()

  if (start_time.ne.expected_time) then
     stop BMI_FAILURE
  end if
end program test_get_start_time
