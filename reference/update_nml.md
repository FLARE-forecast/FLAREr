# Update multiple variables in a GLM/ELCOM namelist file (read-modify-write)

Update multiple variables in a GLM/ELCOM namelist file
(read-modify-write)

## Usage

``` r
update_nml(var_list, var_name_list, working_directory, nml)
```

## Arguments

- var_list:

  List of values to write, parallel to `var_name_list`.

- var_name_list:

  Character vector of namelist variable names to update.

- working_directory:

  Path to the directory containing the nml file.

- nml:

  Filename of the namelist file (e.g. `"glm3.nml"`).

## Value

Invisibly, the updated nml list.
