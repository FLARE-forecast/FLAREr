# Convert historical meteorology and NOAA forecasts to GLM format

Convert historical meteorology and NOAA forecasts to GLM format

## Usage

``` r
create_met_files(
  config,
  lake_directory,
  met_forecast_start_datetime,
  met_start_datetime,
  include_wind_direction = FALSE,
  out_dir_fn = NULL
)
```

## Arguments

- config:

  list of FLARE configurations

- lake_directory:

  directory of lake configurations

- met_forecast_start_datetime:

  start datetime of met forecasts

- met_start_datetime:

  start datetime of met simulation

- include_wind_direction:

  Logical (default FALSE); when TRUE, adds a WindDir column to each
  output CSV.

- out_dir_fn:

  Optional function of the ensemble positional index (integer, 1-based)
  returning the directory path for that member's met file. When non-NULL
  each file is written to out_dir_fn(i); when NULL (default) files
  remain in the run's execute directory.

## Value

list; vector of full path for the converted files and boolean flag if
issues with historical meteorology files

## Details

Function combines historical meteorology and NOAA forecasts to create
meteorology input files in the GLM format. A file is generated for each
ensemble member.

## Author

Quinn Thomas
