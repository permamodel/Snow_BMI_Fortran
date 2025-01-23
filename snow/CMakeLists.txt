# ECSimplesnow: model
set(pkg_name snow_model)

add_library(snow OBJECT functions.f)
if(WIN32)
  add_library(${bmisnow_lib} STATIC bmisnowf.f90 bmi.f90 $<TARGET_OBJECTS:snow>)
else()
  add_library(${bmisnow_lib} SHARED bmisnowf.f90 bmi.f90 $<TARGET_OBJECTS:snow>)
endif()

add_executable(run_${pkg_name} main.f90 $<TARGET_OBJECTS:snow>)
add_executable(run_bmi${pkg_name} bmi_main.f90)
target_link_libraries(run_bmi${pkg_name} ${bmisnow_lib})

install(
  TARGETS run_${pkg_name}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)
install(
  TARGETS run_bmi${pkg_name}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)
install(
  TARGETS ${bmisnow_lib}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)
install(
  FILES ${CMAKE_Fortran_MODULE_DIRECTORY}/${pkg_name}.mod
        ${CMAKE_Fortran_MODULE_DIRECTORY}/${bmisnow_lib}.mod
  DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
)
