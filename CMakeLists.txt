cmake_minimum_required(VERSION 2.8)

project(bmi Fortran)

set(bmi_version 1.0)
set(bmi_lib bmif)
set(bmisnow_lib bmisnowf)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)

set(CMAKE_MACOSX_RPATH 1)
set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")

add_subdirectory(bmi)
add_subdirectory(snow)
add_subdirectory(snow/tests)
add_subdirectory(snow/examples)

include(CTest)
