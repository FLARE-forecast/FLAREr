# Generate initial conditions for FLARE

Generate initial conditions for FLARE

## Usage

``` r
generate_initial_conditions(
  states_config,
  obs_config,
  pars_config = NULL,
  obs,
  config,
  obs_non_vertical = NULL
)
```

## Arguments

- states_config:

  list; list of state configurations

- obs_config:

  list; list of observation configurations

- pars_config:

  list; list of parameter configurations (Default = NULL)

- obs:

  array; array of the observations. Required dimensions are
  `[nobs, time, depth]`

- config:

  list; list of configurations

- obs_non_vertical:

  list; observations of variables that don't have vertical dimension.
  Default = NULL (non-vertical observation assimilation is not part of
  this build of FLAREr).

## Value

list; list contains the initial conditions objects required by
run_da_forecast()

## Details

Function to generate initial conditions from either default values in
the states_config, observations (if available), or a previous run using
the output as a restart file. For new simulations (no restart file),
parameters are initialized from `pars_config`. If
`config$model_settings$par_init_file` is set, the per-ensemble member
parameter values in that CSV (an `ensemble_member` column with values
`1:ensemble_size` plus one column per parameter named by
`par_names_save`) overwrite the config-initialized parameters; any
parameter not present as a column keeps its config-sampled value.

## Author

Quinn Thomas
