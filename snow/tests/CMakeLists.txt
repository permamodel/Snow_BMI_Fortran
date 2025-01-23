# ECSimpleSnow: tests
include(CTest)

include_directories(${CMAKE_Fortran_MODULE_DIRECTORY})

add_library(fixtures OBJECT fixtures.f90)

function(make_test test_name)
  add_test(NAME ${test_name} COMMAND ${test_name})
  add_executable(${test_name} ${test_name}.f90 $<TARGET_OBJECTS:fixtures>)
  target_link_libraries(${test_name} ${bmisnow_lib})
endfunction(make_test)

make_test(test_get_component_name)
make_test(test_initialize)
make_test(test_get_current_time)
make_test(test_get_end_time)
make_test(test_get_grid_size)
make_test(test_get_grid_shape)
make_test(test_get_grid_type)
make_test(test_get_input_var_names)
make_test(test_get_output_var_names)
make_test(test_get_start_time)
make_test(test_get_time_step)
make_test(test_get_time_units)
make_test(test_get_grid_rank)
make_test(test_get_value)
make_test(test_set_value)
make_test(test_get_var_type)
make_test(test_get_var_units)
make_test(test_update)
make_test(test_update_frac)
make_test(test_update_until)
make_test(test_finalize)

file(
  COPY ${CMAKE_CURRENT_SOURCE_DIR}/test.cfg
  DESTINATION ${CMAKE_BINARY_DIR}/snow/tests)
  
file(
  COPY ${data_dir}/test_tair.csv
  DESTINATION ${CMAKE_BINARY_DIR}/snow/tests)
  
file(
  COPY ${data_dir}/test_prec.csv
  DESTINATION ${CMAKE_BINARY_DIR}/snow/tests)
