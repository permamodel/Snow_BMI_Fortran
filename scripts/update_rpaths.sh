#!/usr/bin/env bash

run_install_name_tool() {
    install_name_tool \
	-change @rpath/libgfortran.3.dylib \
	    ${CONDA_PREFIX}/lib/libgfortran.3.dylib \
	-change @rpath/libquadmath.0.dylib \
	    ${CONDA_PREFIX}/lib/libquadmath.0.dylib \
	./snow/$2/$1
    echo "- updated $exe"
}

if [[ -z "$CONDA_PREFIX" ]]; then
    CONDA_PREFIX=`python -c "import sys; print(sys.prefix)"`
fi

echo $CONDA_PREFIX

examples="info_ex"
for exe in $examples; do
    run_install_name_tool $exe examples
done

tests=`ls -1 ./snow/tests/ | egrep "test_"`
for exe in $tests; do
    run_install_name_tool $exe tests
done

mains="run_snow_model run_bmisnow_model"
for exe in $mains; do
    run_install_name_tool $exe $pwd
done
