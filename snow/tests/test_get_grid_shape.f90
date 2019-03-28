program test_get_grid_shape

  use bmif, only: BMI_FAILURE
  use bmisnowf
  use fixtures, only: config_file, status

  implicit none

  integer, parameter :: grid_id = 0
  integer, parameter :: rank = 2
  integer, dimension(rank), parameter :: expected_shape = [1, 1]

  type (bmi_snow) :: m
  integer, dimension(2) :: grid_shape
  integer :: i

  status = m%initialize(config_file)
  status = m%get_grid_shape(grid_id, grid_shape)
  status = m%finalize()

  do i = 1, rank
     if (grid_shape(i).ne.expected_shape(i)) then
        stop BMI_FAILURE
     end if
  end do
end program test_get_grid_shape
