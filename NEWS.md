# FLAREr 3.9 (public release)

A major release introducing GLM-native restart files and serverless (FaaSr) execution. FLARE 3.9 requires a build of GLM-AED with the NetCDF restart capacity (GLM-AED version 4); see *Restart files* below for how to obtain it.

## Data assimilation

* New optional `da_setup` fields: `use_inflation_factor` and `use_one_step_lag` (decouples state and parameter updates).
* New required `add_random_noise` field controlling how process noise is added.
* Optional log-space handling of water-quality observations via `log_transform_wq_obs` and `log_transform_wq_zero_collapse` to avoid zero-clamping bias.
* Reflective bounds and a minimum parameter standard deviation to stabilise parameter estimation; fixes to parameter inflation.
* Option to initialize parameters from a normal distribution.

## Observations

* New optional `assimilate` column in `observations_config.csv` (integer `0`/`1`; defaults to `1` when absent). Setting `assimilate = 0` keeps a variable simulated, tracked, and written to forecast output but excludes it from the EnKF update. Existing configurations without the column are unaffected.

## Restart files

* GLM-native restart support. Restart files are now zip archives containing the FLARE DA state (NetCDF) plus per-ensemble GLM restart files, rather than a plain NetCDF file. Old `.nc` restart files are not compatible with 4.0.
* **Requires GLM-AED version 4**, which adds the NetCDF restart capacity this feature depends on. GLM-AED 4 is currently available on the `v4alpha` branch of GLM-AED and can be installed via the `GLMAEDr` package: `remotes::install_github("flare-forecast/GLMAEDr")`.
* New `restart_save_timesteps` option in `output_settings` controls which timesteps (relative to `forecast_start_datetime`) are saved as restart files.

## Storage backends and FaaSr

* All object I/O is routed through a single layer (`flare_io.R`) that targets one of three backends — `local`, `s3`, or `faasr` — selected by the `use_s3` and `use_faasr` flags in `configure_run.yml`. The default (both `FALSE`) is local, so existing configurations are unaffected.
* Integration with [FaaSr](https://faasr.io) for serverless execution on platforms such as GitHub Actions, AWS Lambda, and OpenWhisk. Setting `use_faasr: TRUE` routes I/O through the FaaSr runtime's DataStore helpers.
* New exported, backend-agnostic I/O wrappers: `flare_get_file()`, `flare_put_file()`, `flare_delete_file()`, `flare_get_folder_list()`, and `flare_arrow_s3_bucket()`.
* New `initialize_faasr()` function validates FaaSr/S3 configuration at startup. Anonymous (public, read-only) S3 DataStores are supported.

## Inputs and models

* GLM-AED phytoplankton parameter calibration: parameters stored in `aed_phyto_pars.csv` can now be calibrated by referencing that file in the `par_file` column of `parameter_calibration_config.csv`.
* Added support for the `GLMAEDr` package for installing and managing the GLM binary.
* **Note:** Windows is not currently supported, as GLM-AED version 4 binaries are not yet built for Windows. Use macOS or Linux (e.g. via WSL2) until Windows v4 binaries are available.
* Support for plunging inflows and dynamic benthic variables (the latter not assimilated).

## Documentation and performance

* New and expanded vignettes: concepts, configuration, drivers, and DA hyperparameters.
* Performance improvements to `run_da_forecast()` and the forecast write step.

## Bug fixes

* Restored the weather uncertainty option.
* Fixed `system2()` error handling in `run_glm.R`.
* Fixed an issue where targets were not pre-ordered.
* Fixed an inflow `sub_elev` issue.
* Fixed a FaaSr bucket bug.

# FLAREr 3.0.3

* Added sim_name option to `run_flare()` to support scenario based forecasting workflows.

# FLAREr 3.0.2

* Fixed bug associated with the daily diagnostics where the depths defaulted to a logical if all depth values were NA
* Fixed bug associated with plotting daily diagnostics

# FLAREr 3.0.1

* Fix bug in `check_noaa_ready()` that assumed a specific value for `future_met_model`
* Fixes to testing suite

# FLAREr 3.0.0

* Able to one parameter from a set of parameters that share the same name (e.g., the temperature in one of three sediment zones)
* Separate inflow and outflow models
* Openmeteo API as an option for meteorological inputs
* Updates to restart capacity that now requires GLM-AED version 3.4 or higher
* Access to daily summaries (e.g., lake.csv) from GLM-AED in FLAREr output
* Flexible paths for directory and partitioning of driver files.  Uses `glue::glue` to create path from internal variables.
