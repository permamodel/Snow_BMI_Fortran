program test_get_output_var_names

  use bmif, only: BMI_FAILURE, BMI_MAX_VAR_NAME
  use bmisnowf
  use fixtures, only: status

  implicit none

  integer, parameter :: n_outputs = 2
  character (len=*), parameter, dimension(n_outputs) :: &
       expected = (/'snowpack__depth', 'snowpack__mass-per-volume_density'/)

  type (bmi_snow) :: m
  character (len=BMI_MAX_VAR_NAME), pointer :: names(:)
  integer :: i
  
  status = m%get_output_var_names(names)

  ! Visualize
  write(*,*) names
  write(*,*) expected
  
  do i=1, size(names)
     if (names(i).ne.expected(i)) then
        stop BMI_FAILURE
     end if
  end do
end program test_get_output_var_names
