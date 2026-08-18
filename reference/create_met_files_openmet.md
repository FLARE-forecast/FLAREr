# Generate GLM ready met files from the Open-Meteo ensemble API

Generate GLM ready met files from the Open-Meteo ensemble API

## Usage

``` r
create_met_files_openmet(
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

  Logical (default `FALSE`); when `TRUE`, adds a `WindDir` column to
  each output CSV using Open-Meteo's `wind_direction_10m` variable.

- out_dir_fn:

  Optional function of the ensemble positional index (integer, 1-based)
  returning the directory path for that member's met file. When non-NULL
  each file is written to `out_dir_fn(i)`; the returned filenames
  reflect those locations. When `NULL` (default) files are written to
  `config$file_path$execute_directory`.

## Value

list; vector of full path for the converted files

## Details

Downloads a live ensemble weather forecast from the Open-Meteo ensemble
API (<https://open-meteo.com/en/docs/ensemble-api>) using the
`ropenmeteo` package and writes one GLM-formatted meteorology CSV per
ensemble member. Only the ensemble-forecast API is supported (no
seasonal/historical/climate modes, no S3-archived alternative to the
live API call). The pivot/rename step intentionally reimplements (rather
than calls)
[`ropenmeteo::write_glm_format()`](http://flare-forecast.org/ropenmeteo/reference/write_glm_format.md)
so files land directly in FLAREr's per-ensemble-member
directory/filename layout and so `include_wind_direction` is supported
(`write_glm_format()`'s own column selection always drops wind
direction).

## Author

Quinn Thomas
