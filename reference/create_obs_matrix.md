# Create matrix of observations in the format required by run_da_forecast

Create matrix of observations in the format required by run_da_forecast

## Usage

``` r
create_obs_matrix(cleaned_observations_file_long, obs_config, config)
```

## Arguments

- cleaned_observations_file_long:

  string; file name (with full path) of the long-format observation file

- obs_config:

  list; observations configuration list

- config:

  list; flare configuration list

## Value

matrix that is based to generate_initial_conditions() and
run_da_forecast()

## Details

Creates a matrix of observations that maps the modeled states to the
observed states. The function uses information from columns the
obs_config file.

## Author

Quinn Thomas
