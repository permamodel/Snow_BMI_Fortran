! Run the heat model through its BMI.
program bmi_main

  use bmisnowf
  implicit none

  character (len=*), parameter :: var_name1 = "land_surface_air__temperature"
  character (len=*), parameter :: var_name2 = "precipitation_mass_flux"
  character (len=*), parameter :: var_name3 = "snowpack__depth"
  character (len=*), parameter :: var_name4 = "snowpack__mass-per-volume_density"
  
  character (len=*), parameter :: var_name5 = "precipitation_mass_flux_adjust_factor"
  character (len=*), parameter :: var_name6 = "snow_class"
  character (len=*), parameter :: var_name7 = "open_area_or_not"
  character (len=*), parameter :: var_name8 = "snowpack__initial_depth"
  character (len=*), parameter :: var_name9 = "initial_snow_density"
  
  integer, parameter :: ndims = 2
  integer:: s, grid_id1, grid_size1, grid_id2, grid_size2, i
  integer:: grid_id3, grid_size3, grid_id4, grid_size4
  integer:: grid_id5, grid_size5, grid_id6, grid_size6
  integer:: grid_id7, grid_size7, grid_id8, grid_size8
  integer:: grid_id9, grid_size9
  
  real:: x
  
  character(len=10) var_unit1, var_unit2, var_unit3
  character(len=10) var_unit4, unit5, unit6
  character(len=10) unit7, unit8, unit9
    
  double precision :: current_time, end_time
  real, allocatable :: temperature(:)
  real, allocatable :: precipitation(:)
  real, allocatable :: snow_depth(:)
  real, allocatable :: snow_density(:)
  real, allocatable :: precipitation_adjust_factor(:)
  integer, allocatable :: snow_class(:)
  integer, allocatable :: open_area(:)
  real, allocatable :: initial_snow_depth(:)
  real, allocatable :: initial_snow_density(:)
  
  type (bmi_snow) :: model
  
  character*256 fconfig 
      
  IF (COMMAND_ARGUMENT_COUNT() .EQ. 0) THEN
  fconfig = 'snow_model_test.cfg'
  ELSE
  CALL GET_COMMAND_ARGUMENT(1, fconfig)
  ENDIF
  
  s = model%initialize(fconfig)
  
  write(*,"(a)") "Initialized"
  
  s = model%get_current_time(current_time)
  s = model%get_end_time(end_time)
  
  s = model%get_var_grid(var_name1, grid_id1)
  s = model%get_grid_size(grid_id1, grid_size1)
  s = model%get_var_units(var_name1, var_unit1)
  
  s = model%get_var_grid(var_name2, grid_id2)
  s = model%get_grid_size(grid_id2, grid_size2)  
  s = model%get_var_units(var_name2, var_unit2)
  
  s = model%get_var_grid(var_name3, grid_id3)
  s = model%get_grid_size(grid_id3, grid_size3) 
  s = model%get_var_units(var_name3, var_unit3)
  
  s = model%get_var_grid(var_name4, grid_id4)
  s = model%get_grid_size(grid_id4, grid_size4) 
  s = model%get_var_units(var_name4, var_unit4)

  s = model%get_var_grid(var_name5, grid_id5)
  s = model%get_grid_size(grid_id5, grid_size5) 

  s = model%get_var_grid(var_name6, grid_id6)
  s = model%get_grid_size(grid_id6, grid_size6) 
  
  s = model%get_var_grid(var_name7, grid_id7)
  s = model%get_grid_size(grid_id7, grid_size7) 
  
  s = model%get_var_grid(var_name8, grid_id8)
  s = model%get_grid_size(grid_id8, grid_size8) 

  s = model%get_var_grid(var_name9, grid_id9)
  s = model%get_grid_size(grid_id9, grid_size9) 
  
  write(*, '("Initial Time  = ",f0.1)') current_time     
  write(*, '("Final Time    = ",f0.1)') end_time
        
  allocate(temperature(grid_size1))
  allocate(precipitation(grid_size2))
  allocate(snow_depth(grid_size3))
  allocate(snow_density(grid_size4))
  allocate(precipitation_adjust_factor(grid_size5))
  allocate(snow_class(grid_size6))
  allocate(open_area(grid_size7))
  allocate(initial_snow_depth(grid_size8))
  allocate(initial_snow_density(grid_size8))
  
  s = model%get_value(var_name1, temperature)
  s = model%get_value(var_name2, precipitation)
  s = model%get_value(var_name5, precipitation_adjust_factor)
  s = model%get_value(var_name6, snow_class)
  s = model%get_value(var_name7, open_area)
  s = model%get_value(var_name8, initial_snow_depth)
  s = model%get_value(var_name9, initial_snow_density)
   
  write(*, '("Snow Class       = ",I0)') snow_class
  write(*, '("Open Area or Not = ",I0)') open_area
  write(*, '("P Adjust         = ",f0.2)') precipitation_adjust_factor
  write(*, '("Initial Snow Dep = ",f0.2)') initial_snow_depth
  write(*, '("Initial Snow Den = ",f0.2)') initial_snow_density
            
  print *, 'Updating'
  
  s = model%update_until(DBLE(55.0))
  
  print *, 'Updating'
  
  do i = 1, 5
  
!   s = model%set_value(var_name1, [-5.0]) ! set air temperature
!   s = model%set_value(var_name2, [0.0])  ! set precipitation
!   s = model%set_value(var_name8, [10.0]) ! set initial snow depth
!   s = model%set_value(var_name9, [100.0]) ! set initial snow depth
  
!   s = model%set_value(var_name5, [0.5])  ! set snow class

  s = model%get_current_time(current_time)
  s = model%get_value(var_name1, temperature)
  s = model%get_value(var_name2, precipitation)
   
  s = model%update()
  
  s = model%get_value(var_name3, snow_depth)
  s = model%get_value(var_name4, snow_density)
  
  WRITE(*, '(F0.1, F8.2, A ,F8.2, A,F8.2, A, F6.1, A)') &
  current_time, temperature, trim(var_unit1), &
  precipitation, trim(var_unit2), &
  snow_depth, trim(var_unit3), &
  snow_density, trim(var_unit4)
  
  enddo
  
  deallocate(temperature)
  deallocate(precipitation)
  deallocate(snow_depth)
  deallocate(snow_density)
  deallocate(precipitation_adjust_factor)
  deallocate(snow_class)
  deallocate(open_area)
  deallocate(initial_snow_depth)
  deallocate(initial_snow_density)

end program bmi_main
