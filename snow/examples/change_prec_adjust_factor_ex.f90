! See the effect of changing diffusivity on plate temperature.
program change_prec_adjust_factor_ex

  use bmisnowf
  use testing_helpers, only: print_array
  implicit none

  character (len=*), parameter :: config_file = "test1.cfg"
  character (len=*), parameter :: &
       dname = "precipitation_mass_flux_adjust_factor"
  character (len=*), parameter :: &
       tname = "snowpack__depth"
  double precision, parameter :: end_time = 10.d0

  type (bmi_snow) :: m
  integer :: tgrid_id
  integer, dimension(2) :: tdims
  real :: padj(1), snod(10), snod_new(10), temp(1)
  integer :: status, i
  
  ! Run model to the end with alpha=1.0 (from cfg file).
  status = m%initialize(config_file)

  status = m%get_value(dname, padj)
  write(*,"(a)") "Run 1"
  write(*,"(a, f5.2)") "alpha =", padj
  
  do i = 1, 10
  status = m%update()
  
  status = m%get_value(tname, temp)
  snod(i) = temp(1)
  enddo
  
  status = m%finalize()
    
  ! Run model to the end with alpha=0.8.
  status = m%initialize(config_file)
  padj = 0.8
  status = m%set_value(dname, padj)
  write(*,"(a)") "Run 2"
  write(*,"(a, f5.2)") "alpha =", padj
  do i = 1, 10
  status = m%update()
  status = m%get_value(tname, temp)
  snod_new(i) = temp(1)
  enddo
  status = m%finalize()
  
  write(*,'(A8,A8)') 'Run 1', 'Run 2'
  do i =1,10
  write(*,'(F8.1,F8.1)') snod(i), snod_new(i)
  enddo
    
end program change_prec_adjust_factor_ex
