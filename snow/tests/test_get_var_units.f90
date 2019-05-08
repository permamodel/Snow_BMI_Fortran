program test_get_var_units

  use bmif_1_2, only: BMI_FAILURE, BMI_MAX_UNITS_NAME
  use bmisnowf
  use fixtures, only: config_file, status

  implicit none

  character (len=*), parameter :: var_name = "land_surface_air__temperature"
  character (len=*), parameter :: expected_units = "C"

  type (bmi_snow) :: m
  character (len=BMI_MAX_UNITS_NAME) :: var_units

  status = m%initialize(config_file)
  status = m%get_var_units(var_name, var_units)
  status = m%finalize()

  if (var_units.ne.expected_units) then
     stop BMI_FAILURE
  end if
end program test_get_var_units
