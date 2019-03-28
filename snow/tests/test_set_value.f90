program test_set_value

  use bmif, only: BMI_SUCCESS, BMI_FAILURE
  use bmisnowf
  use fixtures, only: status, print_array

  implicit none

  character (len=*), parameter :: config_file = "test.cfg"
  type (bmi_snow) :: m
  integer :: retcode

  retcode = test1()
  if (retcode.ne.BMI_SUCCESS) then
     stop BMI_FAILURE
  end if

contains

  function test1() result(code)
    character (len=*), parameter :: &
         var_name = "land_surface_air__temperature"
    integer, parameter :: size = 1
    real :: x(size), y(size)
    integer :: i, code

    status = m%initialize(config_file)
    status = m%get_value(var_name, x)
    status = m%set_value(var_name, [3.0])
    status = m%get_value(var_name, y)
    status = m%finalize()

    ! Visual inspection.
    write(*,*) "Test 1"
    
    print *, x, y

    code = BMI_SUCCESS
    do i = 1, size
       if (y(i).ne.3.0) then
          code = BMI_FAILURE
          exit
       end if
    end do
  end function test1

  function test2() result(code)
    character (len=*), parameter :: &
         var_name = "precipitation_mass_flux"
    integer, parameter :: size = 1
    real :: x(size), y(size)
    integer :: i, code

    status = m%initialize(config_file)
    status = m%get_value(var_name, x)
    status = m%set_value(var_name, [3.0])
    status = m%get_value(var_name, y)
    status = m%finalize()

    ! Visual inspection.
    write(*,*) "Test 2"
    
    print *, x, y

    code = BMI_SUCCESS
    do i = 1, size
       if (y(i).ne.3.0) then
          code = BMI_FAILURE
          exit
       end if
    end do
  end function test2

  function test3() result(code)
    character (len=*), parameter :: &
         var_name = "snowpack__initial_depth"
    integer, parameter :: size = 1
    real :: x(size), y(size)
    integer :: i, code

    status = m%initialize(config_file)
    status = m%get_value(var_name, x)
    status = m%set_value(var_name, [3.0])
    status = m%get_value(var_name, y)
    status = m%finalize()

    ! Visual inspection.
    write(*,*) "Test 3"
    
    print *, x, y

    code = BMI_SUCCESS
    do i = 1, size
       if (y(i).ne.3.0) then
          code = BMI_FAILURE
          exit
       end if
    end do
  end function test3

end program test_set_value
