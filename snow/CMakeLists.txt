# ECSimplesnow: model
set(pkg_name snow_model)
set(src_${pkg_name} functions.f main.f90)
set(src_bmi${pkg_name} functions.f bmisnowf.f90 bmi.f90)

# Build a shared library, except on Windows.
if(WIN32)
  add_library(${bmisnow_lib} ${src_bmi${pkg_name}})
else()
  add_library(${bmisnow_lib} SHARED ${src_bmi${pkg_name}})
endif()

add_executable(run_${pkg_name} ${src_${pkg_name}})

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
