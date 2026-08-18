# Generate model error matrix from configuration files

Generate model error matrix from configuration files

## Usage

``` r
initiate_model_error(config, states_config)
```

## Arguments

- config:

  list; list from reading in configuration yaml

- states_config:

  list; list from reading in states configuration csv

## Value

matrix

## Details

Function uses the configuration files to generate the matrix that
run_da_forecast use to add normally distributed random noise to model
predictions
