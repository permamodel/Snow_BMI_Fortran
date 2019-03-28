! Test the BMI get_var_* and get_grid_* functions.
program vargrid_test

  use bmif, only: BMI_MAX_VAR_NAME
  use bmisnowf
  implicit none

  type (bmi_snow) :: m
  integer :: s, i
  character (len=BMI_MAX_VAR_NAME), pointer :: names(:)
  integer :: grid_id
  character (len=BMI_MAX_VAR_NAME) :: astring
  integer :: asize
  double precision, dimension(2) :: darray
  integer, dimension(2) :: iarray

  write (*,"(a)",advance="no") "Initializing..."
  s = m%initialize("test1.cfg")
  write (*,*) "Done."

  s = m%get_output_var_names(names)
  write (*,"(a)") "Output variables:"
  do i = 1, size(names)
     write (*,"(a, 1x, a)") "-", trim(names(i))
  end do

  s = m%get_var_grid(names(1), grid_id)
  write (*,"(a, i3)") "Grid id:", grid_id

  s = m%get_grid_type(grid_id, astring)
  write (*,"(a, 1x, a)") "Grid type:", trim(astring)
  s = m%get_grid_origin(grid_id, darray)
  write (*,"(a, 1x, 2(f8.2))") "Grid origin:", darray
  s = m%get_grid_rank(grid_id, asize)
  write (*,"(a, i3)") "Grid rank:", asize
  s = m%get_grid_shape(grid_id, iarray)
  write (*,"(a, 2(1x, i3))") "Grid shape:", iarray
  s = m%get_grid_size(grid_id, asize)
  write (*,"(a, i8)") "Grid size:", asize
  s = m%get_grid_spacing(grid_id, darray)
  write (*,"(a, 1x, 2(f8.2))") "Grid spacing:", darray

  s = m%get_var_itemsize(names(1), asize)
  write (*,"(a, i8, 1x, a)") "Item size:", asize, "bytes"
  s = m%get_var_nbytes(names(1), asize)
  write (*,"(a, i8, 1x, a)") "Variable size:", asize, "bytes"
  s = m%get_var_type(names(1), astring)
  write (*,"(a, 1x, a)") "Variable type:", trim(astring)
  s = m%get_var_units(names(1), astring)
  write (*,"(a, 1x, a)") "Variable units:", trim(astring)

  write (*,"(a)", advance="no") "Finalizing..."
  s = m%finalize()
  write (*,*) "Done"

end program vargrid_test
