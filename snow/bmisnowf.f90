module bmisnowf

  use snow_model
  use bmif
  use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_f_pointer
  implicit none

  type, extends (bmi) :: bmi_snow
     private
     type (snow_model_type) :: model
   contains
     procedure :: get_component_name => snow_component_name
     procedure :: get_input_var_names => snow_input_var_names
     procedure :: get_output_var_names => snow_output_var_names
     procedure :: initialize => snow_initialize
     procedure :: finalize => snow_finalize
     procedure :: get_start_time => snow_start_time
     procedure :: get_end_time => snow_end_time
     procedure :: get_current_time => snow_current_time
     procedure :: get_time_step => snow_time_step
     procedure :: get_time_units => snow_time_units
     procedure :: update => snow_update
     procedure :: update_frac => snow_update_frac
     procedure :: update_until => snow_update_until
     procedure :: get_var_grid => snow_var_grid
     procedure :: get_grid_type => snow_grid_type
     procedure :: get_grid_rank => snow_grid_rank
     procedure :: get_grid_shape => snow_grid_shape
     procedure :: get_grid_size => snow_grid_size
     procedure :: get_grid_spacing => snow_grid_spacing
     procedure :: get_grid_origin => snow_grid_origin
     procedure :: get_grid_x => snow_grid_x
     procedure :: get_grid_y => snow_grid_y
     procedure :: get_grid_z => snow_grid_z
     procedure :: get_grid_connectivity => snow_grid_connectivity
     procedure :: get_grid_offset => snow_grid_offset
     procedure :: get_var_type => snow_var_type
     procedure :: get_var_units => snow_var_units
     procedure :: get_var_itemsize => snow_var_itemsize
     procedure :: get_var_nbytes => snow_var_nbytes
     procedure :: get_value_int => snow_get_int
     procedure :: get_value_float => snow_get_float
     procedure :: get_value_double => snow_get_double
     generic :: get_value => &
          get_value_int, &
          get_value_float, &
          get_value_double
     procedure :: get_value_ptr_int => snow_get_ptr_int
     procedure :: get_value_ptr_float => snow_get_ptr_float
     procedure :: get_value_ptr_double => snow_get_ptr_double
     generic :: get_value_ptr => &
          get_value_ptr_int, &
          get_value_ptr_float, &
          get_value_ptr_double
     procedure :: get_value_at_indices_int => snow_get_at_indices_int
     procedure :: get_value_at_indices_float => snow_get_at_indices_float
     procedure :: get_value_at_indices_double => snow_get_at_indices_double
     generic :: get_value_at_indices => &
          get_value_at_indices_int, &
          get_value_at_indices_float, &
          get_value_at_indices_double
     procedure :: set_value_int => snow_set_int
     procedure :: set_value_float => snow_set_float
     procedure :: set_value_double => snow_set_double
     generic :: set_value => &
          set_value_int, &
          set_value_float, &
          set_value_double
     procedure :: set_value_at_indices_int => snow_set_at_indices_int
     procedure :: set_value_at_indices_float => snow_set_at_indices_float
     procedure :: set_value_at_indices_double => snow_set_at_indices_double
     generic :: set_value_at_indices => &
          set_value_at_indices_int, &
          set_value_at_indices_float, &
          set_value_at_indices_double
     procedure :: print_model_info
  end type bmi_snow

  private
  public :: bmi_snow

  character (len=BMI_MAX_COMPONENT_NAME), target :: &
       component_name = "The 1D Snow Model"

  ! Exchange items
  integer, parameter :: input_item_count = 7
  integer, parameter :: output_item_count = 2
  character (len=BMI_MAX_VAR_NAME), target, &
       dimension(input_item_count) :: input_items
  character (len=BMI_MAX_VAR_NAME), target, &
       dimension(output_item_count) :: &
       output_items = (/'snowpack__depth', 'snowpack__mass-per-volume_density'/)

contains

  ! Get the name of the model.
  function snow_component_name(self, name) result (bmi_status)
    class (bmi_snow), intent(in) :: self
    character (len=*), pointer, intent(out) :: name
    integer :: bmi_status

    name => component_name
    bmi_status = BMI_SUCCESS
  end function snow_component_name

  ! List input variables.
  function snow_input_var_names(self, names) result (bmi_status)
    class (bmi_snow), intent(in) :: self
    character (*), pointer, intent(out) :: names(:)
    integer :: bmi_status

    input_items(1) = 'precipitation_mass_flux'
    input_items(2) = 'land_surface_air__temperature'
    input_items(3) = 'precipitation_mass_flux_adjust_factor'
    input_items(4) = 'snow_class'
    input_items(5) = 'open_area_or_not'
    input_items(6) = 'snowpack__initial_depth'
    input_items(7) = 'initial_snow_density'

    names => input_items
    bmi_status = BMI_SUCCESS
  end function snow_input_var_names

  ! List output variables.
  function snow_output_var_names(self, names) result (bmi_status)
    class (bmi_snow), intent(in) :: self
    character (*), pointer, intent(out) :: names(:)
    integer :: bmi_status

    names => output_items
    bmi_status = BMI_SUCCESS
  end function snow_output_var_names

  ! BMI initializer.
  function snow_initialize(self, config_file) result (bmi_status)
    class (bmi_snow), intent(out) :: self
    character (len=*), intent(in) :: config_file
    integer :: bmi_status

    if (len(config_file) > 0) then
       call initialize(self%model, config_file)
    else
       stop
    end if
    bmi_status = BMI_SUCCESS
            
  end function snow_initialize

  ! BMI finalizer.
  function snow_finalize(self) result (bmi_status)
    class (bmi_snow), intent(inout) :: self
    integer :: bmi_status

    call finalize(self%model)
    bmi_status = BMI_SUCCESS
  end function snow_finalize

  ! Model start time.
  function snow_start_time(self, time) result (bmi_status)
    class (bmi_snow), intent(in) :: self
    double precision, intent(out) :: time
    integer :: bmi_status

    time = 0.d0
    bmi_status = BMI_SUCCESS
  end function snow_start_time

  ! Model end time.
  function snow_end_time(self, time) result (bmi_status)
    class (bmi_snow), intent(in) :: self
    double precision, intent(out) :: time
    integer :: bmi_status

    time = dble(self%model%N_TIME_STEPS)
    bmi_status = BMI_SUCCESS
  end function snow_end_time

  ! Model current time.
  function snow_current_time(self, time) result (bmi_status)
    class (bmi_snow), intent(in) :: self
    double precision, intent(out) :: time
    integer :: bmi_status

    time = dble(self%model%t)
    bmi_status = BMI_SUCCESS
  end function snow_current_time

  ! Model time step.
  function snow_time_step(self, time_step) result (bmi_status)
    class (bmi_snow), intent(in) :: self
    double precision, intent(out) :: time_step
    integer :: bmi_status

    time_step = dble(self%model%dt)
    bmi_status = BMI_SUCCESS
  end function snow_time_step

  ! Model time units.
  function snow_time_units(self, time_units) result (bmi_status)
    class (bmi_snow), intent(in) :: self
    character (len=*), intent(out) :: time_units
    integer :: bmi_status

    time_units = "day"
    bmi_status = BMI_SUCCESS
  end function snow_time_units

  ! Advance model by one time step.
  function snow_update(self) result (bmi_status)
    class (bmi_snow), intent(inout) :: self
    integer :: bmi_status

    call update(self%model)
    bmi_status = BMI_SUCCESS
  end function snow_update

  ! Advance the model by a fraction of a time step.
  function snow_update_frac(self, time_frac) result (bmi_status)
    class (bmi_snow), intent(inout) :: self
    double precision, intent(in) :: time_frac
    integer :: bmi_status
    real :: time_step

    if (time_frac > 0.0) then
       time_step = self%model%dt
       self%model%dt = time_step*real(time_frac)
       call update(self%model)
       self%model%dt = time_step
    end if
    bmi_status = BMI_SUCCESS
  end function snow_update_frac

  ! Advance the model until the given time.
  function snow_update_until(self, time) result (bmi_status)
    class (bmi_snow), intent(inout) :: self
    double precision, intent(in) :: time
    integer :: bmi_status
    double precision :: n_steps_real
    integer :: n_steps, i, s

    if (time > self%model%t) then
       n_steps_real = (time - self%model%t) / self%model%dt
       n_steps = floor(n_steps_real)
       do i = 1, n_steps
          s = self%update()
       end do
       s = self%update_frac(n_steps_real - dble(n_steps))
    end if
    bmi_status = BMI_SUCCESS
  end function snow_update_until

  ! Get the grid id for a particular variable.
  function snow_var_grid(self, var_name, grid_id) result (bmi_status)
    class (bmi_snow), intent(in) :: self
    character (len=*), intent(in) :: var_name
    integer, intent(out) :: grid_id
    integer :: bmi_status

    select case(var_name)
    case('land_surface_air__temperature')
       grid_id = 1
       bmi_status = BMI_SUCCESS
    case('precipitation_mass_flux')
       grid_id = 1
       bmi_status = BMI_SUCCESS
    case('snowpack__depth')
       grid_id = 1
       bmi_status = BMI_SUCCESS
    case('snowpack__mass-per-volume_density')
       grid_id = 1
       bmi_status = BMI_SUCCESS
    case('precipitation_mass_flux_adjust_factor')
       grid_id = 1
       bmi_status = BMI_SUCCESS
    case('snow_class')
       grid_id = 1
       bmi_status = BMI_SUCCESS
    case('open_area_or_not')
       grid_id = 1
       bmi_status = BMI_SUCCESS
    case('snowpack__initial_depth')
       grid_id = 1
       bmi_status = BMI_SUCCESS
    case('initial_snow_density')
       grid_id = 1
       bmi_status = BMI_SUCCESS
    case default
       grid_id = -1
       bmi_status = BMI_FAILURE
    end select
  end function snow_var_grid

  ! The type of a variable's grid.
  function snow_grid_type(self, grid_id, grid_type) result (bmi_status)
    class (bmi_snow), intent(in) :: self
    integer, intent(in) :: grid_id
    character (len=*), intent(out) :: grid_type
    integer :: bmi_status

    select case(grid_id)
    case(0)
       grid_type = "uniform_rectilinear"
       bmi_status = BMI_SUCCESS
    case(1)
       grid_type = "scalar"
       bmi_status = BMI_SUCCESS
    case default
       grid_type = "-"
       bmi_status = BMI_FAILURE
    end select
  end function snow_grid_type

  ! The number of dimensions of a grid.
  function snow_grid_rank(self, grid_id, grid_rank) result (bmi_status)
    class (bmi_snow), intent(in) :: self
    integer, intent(in) :: grid_id
    integer, intent(out) :: grid_rank
    integer :: bmi_status

    select case(grid_id)
    case(0)
       grid_rank = 2
       bmi_status = BMI_SUCCESS
    case(1)
       grid_rank = 0
       bmi_status = BMI_SUCCESS
    case default
       grid_rank = -1
       bmi_status = BMI_FAILURE
    end select
  end function snow_grid_rank

  ! The dimensions of a grid.
  function snow_grid_shape(self, grid_id, grid_shape) result (bmi_status)
    class (bmi_snow), intent(in) :: self
    integer, intent(in) :: grid_id
    integer, dimension(:), intent(out) :: grid_shape
    integer :: bmi_status
    
    select case(grid_id)
    case(0)
       grid_shape = [self%model%n_y, self%model%n_x]
       bmi_status = BMI_SUCCESS
    case default
       grid_shape = [-1]
       bmi_status = BMI_FAILURE
    end select
  end function snow_grid_shape

  ! The total number of elements in a grid.
  function snow_grid_size(self, grid_id, grid_size) result (bmi_status)
    class (bmi_snow), intent(in) :: self
    integer, intent(in) :: grid_id
    integer, intent(out) :: grid_size
    integer :: bmi_status

    select case(grid_id)
    case(0)
       grid_size = self%model%n_y * self%model%n_x
       bmi_status = BMI_SUCCESS
    case(1)
       grid_size = 1
       bmi_status = BMI_SUCCESS
    case default
       grid_size = -1
       bmi_status = BMI_FAILURE
    end select
  end function snow_grid_size

  ! The distance between nodes of a grid.
  function snow_grid_spacing(self, grid_id, grid_spacing) result (bmi_status)
    class (bmi_snow), intent(in) :: self
    integer, intent(in) :: grid_id
    double precision, dimension(:), intent(out) :: grid_spacing
    integer :: bmi_status

    select case(grid_id)
    case(0)
       grid_spacing = [self%model%dy, self%model%dx]
       bmi_status = BMI_SUCCESS
    case default
       grid_spacing = [-1.d0]
       bmi_status = BMI_FAILURE
    end select
  end function snow_grid_spacing

  ! Coordinates of grid origin.
  function snow_grid_origin(self, grid_id, grid_origin) result (bmi_status)
    class (bmi_snow), intent(in) :: self
    integer, intent(in) :: grid_id
    double precision, dimension(:), intent(out) :: grid_origin
    integer :: bmi_status

    select case(grid_id)
    case(0)
       grid_origin = [0.d0, 0.d0]
       bmi_status = BMI_SUCCESS
    case default
       grid_origin = [-1.d0]
       bmi_status = BMI_FAILURE
    end select
  end function snow_grid_origin

  ! X-coordinates of grid nodes.
  function snow_grid_x(self, grid_id, grid_x) result (bmi_status)
    class (bmi_snow), intent(in) :: self
    integer, intent(in) :: grid_id
    double precision, dimension(:), intent(out) :: grid_x
    integer :: bmi_status

    select case(grid_id)
    case(1)
       grid_x = [0.d0]
       bmi_status = BMI_SUCCESS
    case default
       grid_x = [-1.d0]
       bmi_status = BMI_FAILURE
    end select
  end function snow_grid_x

  ! Y-coordinates of grid nodes.
  function snow_grid_y(self, grid_id, grid_y) result (bmi_status)
    class (bmi_snow), intent(in) :: self
    integer, intent(in) :: grid_id
    double precision, dimension(:), intent(out) :: grid_y
    integer :: bmi_status

    select case(grid_id)
    case(1)
       grid_y = [0.d0]
       bmi_status = BMI_SUCCESS
    case default
       grid_y = [-1.d0]
       bmi_status = BMI_FAILURE
    end select
  end function snow_grid_y

  ! Z-coordinates of grid nodes.
  function snow_grid_z(self, grid_id, grid_z) result (bmi_status)
    class (bmi_snow), intent(in) :: self
    integer, intent(in) :: grid_id
    double precision, dimension(:), intent(out) :: grid_z
    integer :: bmi_status

    select case(grid_id)
    case(1)
       grid_z = [0.d0]
       bmi_status = BMI_SUCCESS
    case default
       grid_z = [-1.d0]
       bmi_status = BMI_FAILURE
    end select
  end function snow_grid_z

  ! Connectivity array of unstructured grid nodes.
  function snow_grid_connectivity(self, grid_id, grid_conn) &
       result (bmi_status)
    class (bmi_snow), intent(in) :: self
    integer, intent(in) :: grid_id
    integer, dimension(:), intent(out) :: grid_conn
    integer :: bmi_status

    select case(grid_id)
    case(1)
       grid_conn = [0]
       bmi_status = BMI_SUCCESS
    case default
       grid_conn = [-1]
       bmi_status = BMI_FAILURE
    end select
  end function snow_grid_connectivity

  ! Offsets of unstructured grid nodes.
  function snow_grid_offset(self, grid_id, grid_offset) &
       result (bmi_status)
    class (bmi_snow), intent(in) :: self
    integer, intent(in) :: grid_id
    integer, dimension(:), intent(out) :: grid_offset
    integer :: bmi_status

    select case(grid_id)
    case(1)
       grid_offset = [0]
       bmi_status = BMI_SUCCESS
    case default
       grid_offset = [-1]
       bmi_status = BMI_FAILURE
    end select
  end function snow_grid_offset

  ! The data type of the variable, as a string.
  function snow_var_type(self, var_name, var_type) result (bmi_status)
    class (bmi_snow) , intent(in) :: self
    character (len=*), intent(in) :: var_name
    character (len=*), intent(out) :: var_type
    integer :: bmi_status

    select case(var_name)
    case("land_surface_air__temperature")
       var_type = "real"
       bmi_status = BMI_SUCCESS
    case("precipitation_mass_flux")
       var_type = "real"
       bmi_status = BMI_SUCCESS
    case("precipitation_mass_flux_adjust_factor")
       var_type = "real"
       bmi_status = BMI_SUCCESS
    case("snowpack__depth")
       var_type = "real"
       bmi_status = BMI_SUCCESS
    case("snowpack__mass-per-volume_density")
       var_type = "real"
       bmi_status = BMI_SUCCESS
    case("snow_class")
       var_type = "integer"
       bmi_status = BMI_SUCCESS
    case("open_area_or_not")
       var_type = "integer"
       bmi_status = BMI_SUCCESS
    case("snowpack__initial_depth")
       var_type = "real"
       bmi_status = BMI_SUCCESS
    case("initial_snow_density")
       var_type = "real"
       bmi_status = BMI_SUCCESS
    case default
       var_type = "-"
       bmi_status = BMI_FAILURE
    end select
  end function snow_var_type

  ! The units of the given variable.
  function snow_var_units(self, var_name, var_units) result (bmi_status)
    class (bmi_snow), intent(in) :: self
    character (len=*), intent(in) :: var_name
    character (len=*), intent(out) :: var_units
    integer :: bmi_status

    select case(var_name)
    case("snowpack__depth")
       var_units = "cm"
       bmi_status = BMI_SUCCESS
    case("snowpack__mass-per-volume_density")
       var_units = "kg per m3"
       bmi_status = BMI_SUCCESS
    case("snowpack__initial_depth")
       var_units = "cm"
       bmi_status = BMI_SUCCESS
    case("initial_snow_density")
       var_units = "kg per m3"
       bmi_status = BMI_SUCCESS
    case("land_surface_air__temperature")
       var_units = "C"
       bmi_status = BMI_SUCCESS
    case("precipitation_mass_flux")
       var_units = "mm"
       bmi_status = BMI_SUCCESS
    case default
       var_units = "-"
       bmi_status = BMI_FAILURE
    end select
  end function snow_var_units

  ! Memory use per array element.
  function snow_var_itemsize(self, var_name, var_size) result (bmi_status)
    class (bmi_snow), intent(in) :: self
    character (len=*), intent(in) :: var_name
    integer, intent(out) :: var_size
    integer :: bmi_status

    select case(var_name)
    case("land_surface_air__temperature")
       var_size = sizeof(self%model%t2_current)  ! 'sizeof' in gcc & ifort
       bmi_status = BMI_SUCCESS
    case("precipitation_mass_flux")
       var_size = sizeof(self%model%PCPN)        ! 'sizeof' in gcc & ifort
       bmi_status = BMI_SUCCESS
    case("precipitation_mass_flux_adjust_factor")
       var_size = sizeof(self%model%PADJ)        ! 'sizeof' in gcc & ifort
       bmi_status = BMI_SUCCESS
    case("snowpack__initial_depth")
       var_size = sizeof(self%model%OLD)         ! 'sizeof' in gcc & ifort
       bmi_status = BMI_SUCCESS
    case("initial_snow_density")
       var_size = sizeof(self%model%OLD)         ! 'sizeof' in gcc & ifort
       bmi_status = BMI_SUCCESS
    case("snowpack__depth")
       var_size = sizeof(self%model%NEW)         ! 'sizeof' in gcc & ifort
       bmi_status = BMI_SUCCESS
    case("snowpack__mass-per-volume_density")
       var_size = sizeof(self%model%NEWP)        ! 'sizeof' in gcc & ifort
       bmi_status = BMI_SUCCESS
    case default
       var_size = -1
       bmi_status = BMI_FAILURE
    end select
  end function snow_var_itemsize

  ! The size of the given variable.
  function snow_var_nbytes(self, var_name, var_nbytes) result (bmi_status)
    class (bmi_snow), intent(in) :: self
    character (len=*), intent(in) :: var_name
    integer, intent(out) :: var_nbytes
    integer :: bmi_status
    integer :: s1, s2, s3, grid_id, grid_size, item_size

    s1 = self%get_var_grid(var_name, grid_id)
    s2 = self%get_grid_size(grid_id, grid_size)
    s3 = self%get_var_itemsize(var_name, item_size)

    if ((s1 == BMI_SUCCESS).and.(s2 == BMI_SUCCESS).and.(s3 == BMI_SUCCESS)) then
       var_nbytes = item_size * grid_size
       bmi_status = BMI_SUCCESS
    else
       var_nbytes = -1
       bmi_status = BMI_FAILURE
    end if
  end function snow_var_nbytes

  ! Get a copy of a integer variable's values, flattened.
  function snow_get_int(self, var_name, dest) result (bmi_status)
    class (bmi_snow), intent(in) :: self
    character (len=*), intent(in) :: var_name
    integer, intent(inout) :: dest(:)
    integer :: bmi_status
    select case(var_name)
    case("snow_class")
    dest = [self%model%ICL]
    bmi_status = BMI_SUCCESS
    case("open_area_or_not")
    dest = [self%model%IOPEN]
    bmi_status = BMI_SUCCESS
    case default
       dest = [-1]
       bmi_status = BMI_FAILURE
    end select
  end function snow_get_int

  ! Get a copy of a real variable's values, flattened.
  function snow_get_float(self, var_name, dest) result (bmi_status)
    class (bmi_snow), intent(in) :: self
    character (len=*), intent(in) :: var_name
    real, intent(inout) :: dest(:)
    integer :: bmi_status

    select case(var_name)
    case("land_surface_air__temperature")
    dest = [self%model%t2_current]
    bmi_status = BMI_SUCCESS
    case("precipitation_mass_flux")
    dest = [self%model%PCPN]
    bmi_status = BMI_SUCCESS
    case("snowpack__depth")
    dest = [self%model%NEW]
    bmi_status = BMI_SUCCESS
    case("snowpack__mass-per-volume_density")
    dest = [self%model%NEWP]
    bmi_status = BMI_SUCCESS
    case("precipitation_mass_flux_adjust_factor")
    dest = [self%model%PADJ]
    bmi_status = BMI_SUCCESS
    case("snowpack__initial_depth")
    dest = [self%model%OLD]
    bmi_status = BMI_SUCCESS
    case("initial_snow_density")
    dest = [self%model%CD]
    bmi_status = BMI_SUCCESS
    case default
       dest = [-1.0]
       bmi_status = BMI_FAILURE
    end select
  end function snow_get_float

  ! Get a copy of a double variable's values, flattened.
  function snow_get_double(self, var_name, dest) result (bmi_status)
    class (bmi_snow), intent(in) :: self
    character (len=*), intent(in) :: var_name
    double precision, intent(inout) :: dest(:)
    integer :: bmi_status

    select case(var_name)
    case default
       dest = [-1.d0]
       bmi_status = BMI_FAILURE
    end select
  end function snow_get_double

  ! Get a reference to an integer-valued variable, flattened.
  function snow_get_ptr_int(self, var_name, dest) result (bmi_status)
    class (bmi_snow), intent(in) :: self
    character (len=*), intent(in) :: var_name
    integer, pointer, intent(inout) :: dest(:)
    integer :: bmi_status
    type (c_ptr) :: src
    integer :: n_elements

    select case(var_name)
    case default
       bmi_status = BMI_FAILURE
    end select
  end function snow_get_ptr_int

  ! Get a reference to a real-valued variable, flattened.
  function snow_get_ptr_float(self, var_name, dest) result (bmi_status)
    class (bmi_snow), intent(in) :: self
    character (len=*), intent(in) :: var_name
    real, pointer, intent(inout) :: dest(:)
    integer :: bmi_status
    type (c_ptr) :: src
    integer :: n_elements

    return
  end function snow_get_ptr_float

  ! Get a reference to an double-valued variable, flattened.
  function snow_get_ptr_double(self, var_name, dest) result (bmi_status)
    class (bmi_snow), intent(in) :: self
    character (len=*), intent(in) :: var_name
    double precision, pointer, intent(inout) :: dest(:)
    integer :: bmi_status
    type (c_ptr) :: src
    integer :: n_elements

    select case(var_name)
    case default
       bmi_status = BMI_FAILURE
    end select
  end function snow_get_ptr_double

  ! Get values of an integer variable at the given locations.
  function snow_get_at_indices_int(self, var_name, dest, indices) &
       result (bmi_status)
    class (bmi_snow), intent(in) :: self
    character (len=*), intent(in) :: var_name
    integer, intent(inout) :: dest(:)
    integer, intent(in) :: indices(:)
    integer :: bmi_status
    type (c_ptr) src
    integer, pointer :: src_flattened(:)
    integer :: i, n_elements

    select case(var_name)
    case default
       bmi_status = BMI_FAILURE
    end select
  end function snow_get_at_indices_int

  ! Get values of a real variable at the given locations.
  function snow_get_at_indices_float(self, var_name, dest, indices) &
       result (bmi_status)
    class (bmi_snow), intent(in) :: self
    character (len=*), intent(in) :: var_name
    real, intent(inout) :: dest(:)
    integer, intent(in) :: indices(:)
    integer :: bmi_status
    type (c_ptr) src
    real, pointer :: src_flattened(:)
    integer :: i, n_elements

    return
  end function snow_get_at_indices_float

  ! Get values of a double variable at the given locations.
  function snow_get_at_indices_double(self, var_name, dest, indices) &
       result (bmi_status)
    class (bmi_snow), intent(in) :: self
    character (len=*), intent(in) :: var_name
    double precision, intent(inout) :: dest(:)
    integer, intent(in) :: indices(:)
    integer :: bmi_status
    type (c_ptr) src
    double precision, pointer :: src_flattened(:)
    integer :: i, n_elements

    select case(var_name)
    case default
       bmi_status = BMI_FAILURE
    end select
  end function snow_get_at_indices_double

  ! Set new integer values.
  function snow_set_int(self, var_name, src) result (bmi_status)
    class (bmi_snow), intent(inout) :: self
    character (len=*), intent(in) :: var_name
    integer, intent(in) :: src(:)
    integer :: bmi_status
    
    select case(var_name)
    case("snow_class")
       self%model%ICL = src(1)
       bmi_status = BMI_SUCCESS
    case("open_area_or_not")
       self%model%IOPEN = src(1)
       bmi_status = BMI_SUCCESS
    case default
       bmi_status = BMI_FAILURE
    end select
    
  end function snow_set_int

  ! Set new real values.
  function snow_set_float(self, var_name, src) result (bmi_status)
    class (bmi_snow), intent(inout) :: self
    character (len=*), intent(in) :: var_name
    real, intent(in) :: src(:)
    integer :: bmi_status
    
    select case(var_name)
    case("land_surface_air__temperature")
       self%model%t2_current = src(1)
       bmi_status = BMI_SUCCESS
    case("precipitation_mass_flux")
       self%model%PCPN = src(1)
       bmi_status = BMI_SUCCESS
    case("snowpack__initial_depth")
       self%model%OLD = src(1)
       bmi_status = BMI_SUCCESS
    case("initial_snow_density")
       self%model%CD = src(1)
       bmi_status = BMI_SUCCESS
    case("precipitation_mass_flux_adjust_factor")
       self%model%PADJ = src(1)
       bmi_status = BMI_SUCCESS
    case default
       bmi_status = BMI_FAILURE
    end select
    
  end function snow_set_float

  ! Set new double values.
  function snow_set_double(self, var_name, src) result (bmi_status)
    class (bmi_snow), intent(inout) :: self
    character (len=*), intent(in) :: var_name
    double precision, intent(in) :: src(:)
    integer :: bmi_status

    select case(var_name)
    case default
       bmi_status = BMI_FAILURE
    end select
  end function snow_set_double

  ! Set integer values at particular locations.
  function snow_set_at_indices_int(self, var_name, indices, src) &
       result (bmi_status)
    class (bmi_snow), intent(inout) :: self
    character (len=*), intent(in) :: var_name
    integer, intent(in) :: indices(:)
    integer, intent(in) :: src(:)
    integer :: bmi_status
    type (c_ptr) dest
    integer, pointer :: dest_flattened(:)
    integer :: i

    select case(var_name)
    case default
       bmi_status = BMI_FAILURE
    end select
  end function snow_set_at_indices_int

  ! Set real values at particular locations.
  function snow_set_at_indices_float(self, var_name, indices, src) &
       result (bmi_status)
    class (bmi_snow), intent(inout) :: self
    character (len=*), intent(in) :: var_name
    integer, intent(in) :: indices(:)
    real, intent(in) :: src(:)
    integer :: bmi_status
    type (c_ptr) dest
    real, pointer :: dest_flattened(:)
    integer :: i

    return
  end function snow_set_at_indices_float

  ! Set double values at particular locations.
  function snow_set_at_indices_double(self, var_name, indices, src) &
       result (bmi_status)
    class (bmi_snow), intent(inout) :: self
    character (len=*), intent(in) :: var_name
    integer, intent(in) :: indices(:)
    double precision, intent(in) :: src(:)
    integer :: bmi_status
    type (c_ptr) dest
    double precision, pointer :: dest_flattened(:)
    integer :: i

    select case(var_name)
    case default
       bmi_status = BMI_FAILURE
    end select
  end function snow_set_at_indices_double

  ! A non-BMI procedure for model introspection.
  subroutine print_model_info(self)
    class (bmi_snow), intent(in) :: self

    call print_info(self%model)
  end subroutine print_model_info

end module bmisnowf
