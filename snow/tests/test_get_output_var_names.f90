program test_get_output_var_names

  use bmif_1_2, only: BMI_FAILURE, BMI_MAX_VAR_NAME
  use bmisnowf
  use fixtures, only: status

  implicit none

  integer, parameter :: n_outputs = 2
  character (len=BMI_MAX_VAR_NAME), allocatable :: expected(:)
  character (len=BMI_MAX_VAR_NAME) :: e1, e2

  type (bmi_snow) :: m
  character (len=BMI_MAX_VAR_NAME), pointer :: names(:)
  integer :: i

  allocate(expected(n_outputs))
  e1 = "snowpack__depth"
  e2 = "snowpack__mass-per-volume_density"
  expected = [e1,e2]
  
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
