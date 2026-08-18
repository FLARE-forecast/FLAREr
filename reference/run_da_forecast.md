# Run ensemble data assimilation and/or produce forecasts

Run ensemble data assimilation and/or produce forecasts

## Usage

``` r
run_da_forecast(
  states_init,
  pars_init = NULL,
  aux_states_init,
  obs,
  obs_sd,
  model_sd,
  working_directory,
  met_file_names,
  inflow_file_names = NULL,
  outflow_file_names = NULL,
  config,
  pars_config = NULL,
  states_config,
  obs_config,
  da_method = "enkf",
  par_fit_method = "perturb",
  obs_non_vertical = NULL,
  non_vertical_noise_config = NULL
)
```

## Arguments

- states_init:

  array of the initial states. Required dimensions are
  `[states, depths, ensemble]`

- pars_init:

  array of the initial states. Required dimensions are
  `[pars, depths, ensemble]`. (Default = NULL)

- aux_states_init:

  list of initial conditions for auxillary states. These are states in
  the GLM that are require for restarting the model but are not included
  in data assimilation. These are states that are not associated with a
  value in `model_sd`.

- obs:

  array; array of the observations. Required dimensions are
  `[nobs, time, depth]`

- obs_sd:

  vector; vector of standard deviation for observation

- model_sd:

  vector vector of standard deviations describing the model error for
  each state

- working_directory:

  string; full path to directory where model executes

- met_file_names:

  vector; vector of full path meteorology file names

- inflow_file_names:

  vector or matrix;; vector of inflow file names

- outflow_file_names:

  vector or matrix; vector of outflow file names

- config:

  list; list of configurations

- pars_config:

  list; list of parameter configurations (Default = NULL)

- states_config:

  data frame of state configurations (read from `states_config.csv`).
  Required columns: `state_names`, `model_sd`, `vert_decorr_length`,
  `initial_model_sd`, `states_to_obs_1`, `states_to_obs_mapping_1`.
  Optional column: `da_updated` (integer 0/1; defaults to 1 for all
  states if absent). States with `da_updated = 0` are simulated by GLM
  but excluded from the EnKF update step.

- obs_config:

  list; list of observation configurations. Optional column `assimilate`
  (integer 0/1; defaults to 1 for all observations if absent).
  Observations flagged `assimilate = 0` are still simulated, tracked,
  and written to output but are excluded from the DA update; this
  applies to both vertical (`multi_depth = 1`) and non-vertical
  (`multi_depth = 0`) variables.

- da_method:

  string; data assimilation method ("enkf" or "none"; Default = "enkf").

- par_fit_method:

  string; method for adding noise to parameters during calibration

- obs_non_vertical:

  named list of non-vertical observations (from create_obs_non_vertical)

## Value

a named list with the following elements:

- full_time:

  vector of all modeled datetimes

- forecast_start_datetime:

  datetime when the forecast period begins

- states_depth:

  array \[states, depths, time, ensemble\] of DA-updated model states
  indexed by depth

- states_height:

  array \[states, heights, time, ensemble\] of DA-updated model states
  indexed by GLM internal height

- pars:

  array \[pars, ensemble\] of DA-updated parameter values

- obs:

  observation array passed through unchanged

- save_file_name:

  full output filename stem (includes history period)

- save_file_name_short:

  short output filename stem (forecast start date only)

- forecast_iteration_id:

  timestamp string identifying this forecast run

- forecast_project_id:

  sim_name from run config

- time_of_forecast:

  POSIXct timestamp when forecast was generated

- snow_ice_thickness:

  GLM restart variable

- lake_depth:

  array \[time, ensemble\] of lake depths

- model_internal_heights:

  array of GLM internal layer heights

- diagnostics:

  array of per-timestep diagnostic variables

- diagnostics_daily:

  array of daily diagnostic variables

- data_assimilation_flag, forecast_flag, da_qc_flag:

  integer vectors flagging DA/forecast/QC status per timestep

- config, states_config, pars_config, obs_config:

  configuration lists passed through

- met_file_names:

  meteorology file paths used

- log_particle_weights:

  internal bookkeeping array, always log(1) for the EnKF used in this
  build

- inflation:

  covariance inflation factor

- glm_restart_staged:

  path to the staged GLM restart file

## Details

Uses the ensemble data assimilation to predict water quality for a lake
or reservoir. The function requires the initial conditions
(`states_init`) for each state and ensemble member using an array with
the following dimension order: states, depth, ensembles member. If you
are fitting parameters, it also requires initial conditions for each
parameter and ensemble member using an array (`par_init`) with the
following dimension order: parameters, ensemble member. The arrays for
states_init and pars_init can be created using the
[`generate_initial_conditions()`](http://flare-forecast.org/FLAREr/reference/generate_initial_conditions.md)
function, if starting from initial conditions in the `states_config`
data frame or from observations in first time column of the `obs` array.
