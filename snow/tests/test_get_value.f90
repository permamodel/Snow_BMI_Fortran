program test_get_value

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
  
  retcode = test2()
  if (retcode.ne.BMI_SUCCESS) then
     stop BMI_FAILURE
  end if

  retcode = test3()
  if (retcode.ne.BMI_SUCCESS) then
     stop BMI_FAILURE
  end if
  
contains

  ! Test getting plate_surface__temperature.
  function test1() result(code)
    character (len=*), parameter :: &
         var_name = "land_surface_air__temperature"
    integer, parameter :: size = 1
    real, parameter :: expected = 2.5
    real :: tval(size)
    integer :: i, code

    status = m%initialize(config_file)
    status = m%get_value(var_name, tval)
    status = m%finalize()
    
    print *, abs(tval(1) - expected)

    ! Visual inspection.
    write(*,*) "Test 1"

    code = BMI_SUCCESS
    do i = 1, size
       if (abs(tval(i) - expected) .gt. 1E-5) then
          code = BMI_FAILURE
          exit
       end if
    end do
  end function test1

  ! Test getting precipitation.
  function test2() result(code)
    character (len=*), parameter :: &
         var_name = "precipitation_mass_flux"
    integer, parameter :: size = 1
    real, parameter :: expected = 0
    real :: tval(size)
    integer :: i, code

    status = m%initialize(config_file)
    status = m%get_value(var_name, tval)
    status = m%finalize()
    
    print *, abs(tval(1) - expected)

    ! Visual inspection.
    write(*,*) "Test 2"

    code = BMI_SUCCESS
    do i = 1, size
       if (abs(tval(i) - expected) .gt. 1E-5) then
          code = BMI_FAILURE
          exit
       end if
    end do
  end function test2

  ! Test getting snow depth.
  function test3() result(code)
    character (len=*), parameter :: &
         var_name = "snowpack__depth"
    integer, parameter :: size = 1
    real, parameter :: expected = 0
    real :: tval(size)
    integer :: i, code

    status = m%initialize(config_file)
    status = m%get_value(var_name, tval)
    status = m%finalize()
    
    print *, abs(tval(1) - expected)

    ! Visual inspection.
    write(*,*) "Test 3"

    code = BMI_SUCCESS
    do i = 1, size
       if (abs(tval(i) - expected) .gt. 1E-5) then
          code = BMI_FAILURE
          exit
       end if
    end do
  end function test3

end program test_get_value
