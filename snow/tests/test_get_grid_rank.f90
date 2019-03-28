program test_get_grid_rank

  use bmif, only: BMI_FAILURE
  use bmisnowf
  use fixtures, only: config_file, status

  implicit none

  integer, parameter :: grid_id = 0
  integer, parameter :: expected_rank = 2

  type (bmi_snow) :: m
  integer :: grid_rank

  status = m%initialize(config_file)
  status = m%get_grid_rank(grid_id, grid_rank)
  status = m%finalize()

  if (grid_rank.ne.expected_rank) then
     stop BMI_FAILURE
  end if
end program test_get_grid_rank
