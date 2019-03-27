This is a simple snow model with BMI (Basic Modeling Interface).

The core snow model is developed by Ross Brown, Environment Canada 

BMI and some code changes are made by Kang Wang, University of Colorado Boulder

**If you are using this code, please cite:**

**Brown, R.D., B. Brasnett and D. Robinson. 2003. Gridded North American monthly snow depth and snow water equivalent for GCM evaluation. Atmosphere-Ocean. 41:1-14.**

### Compile with cmake ###

mkdir _build && cd _build

cmake .. -DCMAKE_INSTALL_PREFIX=[install_path]

make install

cd [install_path]/bin/

./run_bmisnow_model [configuration_file]

### TODO list ###

1 - check and replace variable name with standard name

2 - write more unit tests for each function