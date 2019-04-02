cmake_minimum_required(VERSION 2.8)

project(bmisnow Fortran)

set(bmi_version 1.0)
set(bmisnow_lib bmisnowf)
set(data_dir ${CMAKE_SOURCE_DIR}/data)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)

set(CMAKE_MACOSX_RPATH 1)
set(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")

add_subdirectory(snow)
add_subdirectory(snow/tests)
add_subdirectory(snow/examples)

include(CTest)
