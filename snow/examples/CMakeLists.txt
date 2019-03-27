include(CTest)

include_directories(${CMAKE_Fortran_MODULE_DIRECTORY})

function(make_example example_name)
  add_test(NAME ${example_name} COMMAND ${example_name})
  set(src_${example_name} ${example_name}.f90 testing_helpers.f90)
  add_executable(${example_name} ${src_${example_name}})
  target_link_libraries(${example_name} ${bmi_lib} ${bmiheat_lib})
endfunction(make_example)

make_example(info_ex)
