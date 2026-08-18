# Generate diagnostic plot of FLARE output with observations

Generate diagnostic plot of FLARE output with observations

## Usage

``` r
plotting_general(forecast_df, targets_df, file_name, plots_directory)
```

## Arguments

- forecast_df:

  dataframe; data frame containing the forecast predictions

- targets_df:

  dataframe; data frame containing all observations (targets)

- file_name:

  string; name of pdf file that will be saved

- plots_directory:

  full path of directory where plot will be saved

## Value

None

## Details

Function combines the netcdf output with the long-format observation
file to produce a set of plots for each state variable, calibrated
parameter, and diagnostic variable

## Author

Austin Delany
