# Generate netcdf restart file

Generate netcdf restart file

## Usage

``` r
write_restart(
  da_forecast_output,
  forecast_output_directory,
  use_short_filename = TRUE
)
```

## Arguments

- da_forecast_output:

  list; object that is returned by run_da_forecast()

- forecast_output_directory:

  string; full path of directory where the netcdf file will be written

- use_short_filename:

  use shortened file name; this results in less informatoin in the file
  name and potentially overwriting existing files

## Value

None

## Details

Function generates a netcdf file from the object that is returned by
run_da_forecast()

## Author

Quinn Thomas
