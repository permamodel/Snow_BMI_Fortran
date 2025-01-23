# ECSimpleSnow
cmake_minimum_required(VERSION 3.12)

project(bmisnow
    VERSION 1.2.0
    DESCRIPTION "A simple snow model with a Basic Model Interface"
    HOMEPAGE_URL "https://permamodel.github.io"
    LANGUAGES Fortran
)

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
