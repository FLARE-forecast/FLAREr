# Get run configuration from s3 bucket

Get run configuration from s3 bucket

## Usage

``` r
get_run_config(
  configure_run_file = "configure_run.yml",
  lake_directory,
  config,
  clean_start = FALSE,
  config_set_name = "default",
  sim_name = NA
)
```

## Arguments

- configure_run_file:

  file name (no path) of run configuration file

- lake_directory:

  full path to repository directory

- config:

  flare configuration object

- clean_start:

  logical; reset the configuration run to the base file in the
  configuration directory

- config_set_name:

  name of configuration set

- sim_name:

  name of simulation

## Value

list of configuration values
