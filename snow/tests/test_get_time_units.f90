program test_get_time_units

  use bmif_1_2, only: BMI_FAILURE, BMI_MAX_UNITS_NAME
  use bmisnowf
  use fixtures, only: config_file, status

  implicit none

  character (len=*), parameter :: expected_units = "day"

  type (bmi_snow) :: m
  character (len=BMI_MAX_UNITS_NAME) :: units

  status = m%initialize(config_file)
  status = m%get_time_units(units)
  status = m%finalize()

  if (units.ne.expected_units) then
     stop BMI_FAILURE
  end if
end program test_get_time_units
