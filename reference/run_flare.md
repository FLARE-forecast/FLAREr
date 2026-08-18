# Run FLARE for a single forecast

Run FLARE for a single forecast

## Usage

``` r
run_flare(
  lake_directory,
  configure_run_file,
  config_set_name,
  clean_start = FALSE,
  sim_name = NA
)
```

## Arguments

- lake_directory:

  full path to repository directory

- configure_run_file:

  flare configuration object

- config_set_name:

  directory within configuration/workflow with run configuration files

- clean_start:

  logical: TRUE = reset run configuration with the file in the
  configuration directory within repository

- sim_name:

  sim_name to use to run FLARE and find restart files. Deafult = NA
  (uses the sim_name from run_config) Set the sim_name here when
  multiple simulations are being run from the same config_set_name (e.g.
  scenario forecasting) rather than modifying the
  configuration/configure_run file each time

## Value

the full path to save netcdf file that is used to restart following
forecast

## Details

Combines functions necessary to do a complete execution of FLARE

## Examples

``` r
if (FALSE) { # interactive()
#Load and install dependencies
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
remotes::install_github("flare-forecast/GLMAEDr")
GLMAEDr::glm_install()
Sys.setenv('GLM_PATH'='GLMAEDr')

dir <- normalizePath(tempdir(),  winslash = "/")
lake_directory <- file.path(dir, "extdata")
#Copy files to temporarly directory
dir.create(dir,showWarnings = FALSE)
file.copy(system.file("extdata", package = "FLAREr"),
          tempdir(),
          recursive = TRUE)
run_flare(lake_directory = lake_directory,
          configure_run_file = "configure_run.yml",
          config_set_name = "default")
open_dataset(file.path(lake_directory,"forecasts/parquet")) |>
  filter(variable == "temperature",
        depth == 1) |>
 collect() |>
 ggplot(aes(x = datetime, y = prediction, group = parameter)) +
 geom_line() +
 geom_vline(aes(xintercept = as_datetime(reference_datetime))) +
 labs(title = "1 m water temperature forecast")
}
```
