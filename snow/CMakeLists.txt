set(pkg_name snow_model)
set(mod_name ${pkg_name})
set(src_${pkg_name} functions.f main.f90)
set(src_bmi${pkg_name} functions.f bmisnowf.f90 bmi.f90)

configure_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/${pkg_name}.pc.cmake
  ${CMAKE_CURRENT_SOURCE_DIR}/${pkg_name}.pc)

add_library(${bmisnow_lib} SHARED ${src_bmi${pkg_name}})

add_executable(run_${pkg_name} ${src_${pkg_name}})

add_executable(run_bmi${pkg_name} bmi_main.f90)
target_link_libraries(run_bmi${pkg_name} ${bmisnow_lib})

install(
  PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/run_${pkg_name}
  DESTINATION bin
  COMPONENT ${pkg_name})
install(
  PROGRAMS ${CMAKE_CURRENT_BINARY_DIR}/run_bmi${pkg_name}
  DESTINATION bin
  COMPONENT ${pkg_name})
install(
  TARGETS ${bmisnow_lib}
  DESTINATION lib
  COMPONENT ${pkg_name})
install(
  FILES ${pkg_name}.pc
  DESTINATION lib/pkgconfig)
install(
  FILES ${CMAKE_Fortran_MODULE_DIRECTORY}/${mod_name}.mod
        ${CMAKE_Fortran_MODULE_DIRECTORY}/${bmisnow_lib}.mod
  DESTINATION include)
