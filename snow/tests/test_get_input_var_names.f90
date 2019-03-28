program test_get_input_var_names

  use bmif, only: BMI_FAILURE, BMI_MAX_VAR_NAME
  use bmisnowf
  use fixtures, only: status

  implicit none

  integer, parameter :: n_inputs = 7
  type (bmi_snow) :: m
  character (len=BMI_MAX_VAR_NAME), allocatable :: expected(:)
  character (len=BMI_MAX_VAR_NAME) :: e1, e2, e3, e4, e5, e6, e7
  character (len=BMI_MAX_VAR_NAME), pointer :: names(:)
  integer :: i

  allocate(expected(n_inputs))
  e1 = "precipitation_mass_flux"
  e2 = "land_surface_air__temperature"
  e3 = "precipitation_mass_flux_adjust_factor"
  e4 = 'snow_class'
  e5 = 'open_area_or_not'
  e6 = 'snowpack__initial_depth'
  e7 = 'initial_snow_density'
  expected = [e1,e2,e3,e4,e5,e6,e7]
  
  status = m%get_input_var_names(names)
  
  ! Visualize
  do i = 1, n_inputs
     write(*,*) trim(names(i))
     write(*,*) trim(expected(i))
  end do
  
  do i=1, size(names)
     if (names(i).ne.expected(i)) then
        stop BMI_FAILURE
     end if
  end do
end program test_get_input_var_names
