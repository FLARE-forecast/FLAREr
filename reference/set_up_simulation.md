# Set and create directories in the configuration file

Set and create directories in the configuration file

## Usage

``` r
set_up_simulation(
  configure_run_file = "configure_run.yml",
  lake_directory,
  clean_start = FALSE,
  config_set_name = "default",
  sim_name = NA
)
```

## Arguments

- configure_run_file:

  name of run configuration file (do not include full path)

- lake_directory:

  full path to repository directory

- clean_start:

  logical: TRUE = reset run configuration with the file in the
  configuration directory within repository

- config_set_name:

  name of configuration set

- sim_name:

  name of simulation

## Value

list of configuration values (Invisibly); side effect of creating
necessary subdirectories in `lake_directory`

## Examples

``` r
dir <- normalizePath(tempdir(),  winslash = "/")
lake_directory <- file.path(dir, "extdata")
# Copy files to temporarly directory
dir.create(dir,showWarnings = FALSE)
file.copy(system.file("extdata", package = "FLAREr"),
          tempdir(),
          recursive = TRUE)
#> [1] TRUE

set_up_simulation(configure_run_file = "configure_run.yml",
                  lake_directory = lake_directory,
                   clean_start = FALSE,
                   config_set_name = "default",
                   sim_name = NA)
#> run config not found - clean start


```
