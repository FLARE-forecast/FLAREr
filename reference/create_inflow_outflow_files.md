# Generating inflow and outflow files in the GLM format using arrow

Generating inflow and outflow files in the GLM format using arrow

## Usage

``` r
create_inflow_outflow_files(
  config,
  config_set_name,
  lake_directory,
  out_dir_fn = NULL
)
```

## Arguments

- config:

  configuration file

- config_set_name:

  specific name of configuration within the configuration directory

- lake_directory:

  directory for FLARE application

- out_dir_fn:

  Optional function of the ensemble positional index (integer, 1-based)
  returning the directory for that member's flow files. Passed through
  to `create_flow_files`. Default `NULL`.

## Value

list with two vectors. One vector is the matrix of inflow_file_names and
the other is the matrix of outflow_file_names

## Details

Processes historical model data and future model files into the GLM
format
