Name: snow
Description: An example of the heat equation, with a BMI, in Fortran
Version: ${bmi_version}
Libs: -L${CMAKE_INSTALL_PREFIX}/lib -l${bmisnow_lib} -l${bmi_lib}
Cflags: -I${CMAKE_INSTALL_PREFIX}/include -std=f2003