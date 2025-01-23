# ECSimpleSnow
cmake_minimum_required(VERSION 3.12)

project(bmisnow
    VERSION 1.2.0
    DESCRIPTION "A simple snow model with a Basic Model Interface"
    HOMEPAGE_URL "https://permamodel.github.io"
    LANGUAGES Fortran
)

include(GNUInstallDirs)

set(bmisnow_lib bmisnowf)
set(data_dir ${CMAKE_SOURCE_DIR}/data)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)

add_subdirectory(snow)
add_subdirectory(snow/tests)
add_subdirectory(snow/examples)

include(CTest)
