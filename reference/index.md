# Package index

## Core Workflow

### Out-of-box workflow

Function for default end-to-end application of FLARE

- [`run_flare()`](http://flare-forecast.org/FLAREr/reference/run_flare.md)
  : Run FLARE for a single forecast

### Configuration

Functions for reading and managing run configuration

- [`get_run_config()`](http://flare-forecast.org/FLAREr/reference/get_run_config.md)
  : Get run configuration from s3 bucket
- [`update_run_config()`](http://flare-forecast.org/FLAREr/reference/update_run_config.md)
  : Update run configuration

### Setup

Functions for initializing simulations and environments

- [`set_up_simulation()`](http://flare-forecast.org/FLAREr/reference/set_up_simulation.md)
  : Set and create directories in the configuration file
- [`initialize_faasr()`](http://flare-forecast.org/FLAREr/reference/initialize_faasr.md)
  : Validate FaaSr / S3 / local-mode configuration at setup time.

### Utilities

Helper functions for automation and checks

- [`check_noaa_present()`](http://flare-forecast.org/FLAREr/reference/check_noaa_present.md)
  : Check if NOAA forecasts have been downloaded and processed

## Advanced / Internal

Lower-level functions for building custom workflows

### Data assimilation

- [`run_da_forecast()`](http://flare-forecast.org/FLAREr/reference/run_da_forecast.md)
  : Run ensemble data assimilation and/or produce forecasts
- [`generate_initial_conditions()`](http://flare-forecast.org/FLAREr/reference/generate_initial_conditions.md)
  : Generate initial conditions for FLARE
- [`generate_states_to_obs_mapping()`](http://flare-forecast.org/FLAREr/reference/generate_states_to_obs_mapping.md)
  : Map the states to the observations
- [`initiate_model_error()`](http://flare-forecast.org/FLAREr/reference/initiate_model_error.md)
  : Generate model error matrix from configuration files

### Input preparation

- [`create_met_files()`](http://flare-forecast.org/FLAREr/reference/create_met_files.md)
  : Convert historical meteorology and NOAA forecasts to GLM format
- [`create_met_files_openmet()`](http://flare-forecast.org/FLAREr/reference/create_met_files_openmet.md)
  : Generate GLM ready met files from the Open-Meteo ensemble API
- [`create_inflow_outflow_files()`](http://flare-forecast.org/FLAREr/reference/create_inflow_outflow_files.md)
  : Generating inflow and outflow files in the GLM format using arrow
- [`create_obs_matrix()`](http://flare-forecast.org/FLAREr/reference/create_obs_matrix.md)
  : Create matrix of observations in the format required by
  run_da_forecast

### Output

- [`write_forecast()`](http://flare-forecast.org/FLAREr/reference/write_forecast.md)
  : Generate parquet output file
- [`write_restart()`](http://flare-forecast.org/FLAREr/reference/write_restart.md)
  : Generate netcdf restart file
- [`plotting_general()`](http://flare-forecast.org/FLAREr/reference/plotting_general.md)
  : Generate diagnostic plot of FLARE output with observations

### Storage and targets

- [`get_targets()`](http://flare-forecast.org/FLAREr/reference/get_targets.md)
  : Download target data from s3

- [`put_targets()`](http://flare-forecast.org/FLAREr/reference/put_targets.md)
  : Save target files to s3 bucket

- [`get_restart_file()`](http://flare-forecast.org/FLAREr/reference/get_restart_file.md)
  : Download restart file from s3 bucket

- [`put_restart_file()`](http://flare-forecast.org/FLAREr/reference/put_restart_file.md)
  : Upload restart netcdf file to s3 bucket

- [`delete_restart()`](http://flare-forecast.org/FLAREr/reference/delete_restart.md)
  : Delete restart file on s3 bucket

- [`delete_sim()`](http://flare-forecast.org/FLAREr/reference/delete_sim.md)
  : Delete simulation on s3 bucket

- [`get_driver_forecast_path()`](http://flare-forecast.org/FLAREr/reference/get_driver_forecast_path.md)
  : Get file path for driver forecasts

- [`flare_arrow_s3_bucket()`](http://flare-forecast.org/FLAREr/reference/flare_arrow_s3_bucket.md)
  :

  Return an
  [`arrow::s3_bucket()`](https://arrow.apache.org/docs/r/reference/s3_bucket.html)
  (or local) handle for a partitioned dataset under a server/prefix.

- [`flare_get_file()`](http://flare-forecast.org/FLAREr/reference/flare_get_file.md)
  : Download a single object to a local path.

- [`flare_put_file()`](http://flare-forecast.org/FLAREr/reference/flare_put_file.md)
  : Upload a single local file as an object.

- [`flare_delete_file()`](http://flare-forecast.org/FLAREr/reference/flare_delete_file.md)
  : Delete a single remote object.

- [`flare_get_folder_list()`](http://flare-forecast.org/FLAREr/reference/flare_get_folder_list.md)
  : List object keys under a prefix.

- [`flare_io_mode()`](http://flare-forecast.org/FLAREr/reference/flare_io_mode.md)
  :

  Resolve the I/O backend for a FLARE run from `config$run_config`.

### NML utilities

- [`read_nml()`](http://flare-forecast.org/FLAREr/reference/read_nml.md)
  : Read a GLM/ELCOM namelist file into an R list
- [`write_nml()`](http://flare-forecast.org/FLAREr/reference/write_nml.md)
  : Write an nml list to a namelist file
- [`modify_nml()`](http://flare-forecast.org/FLAREr/reference/modify_nml.md)
  : Apply in-memory updates to a parsed GLM/ELCOM namelist
- [`update_nml()`](http://flare-forecast.org/FLAREr/reference/update_nml.md)
  : Update multiple variables in a GLM/ELCOM namelist file
  (read-modify-write)
- [`print(`*`<nml>`*`)`](http://flare-forecast.org/FLAREr/reference/print.nml.md)
  : Print NML

### Model utilities

- [`build_R_matrix()`](http://flare-forecast.org/FLAREr/reference/build_R_matrix.md)
  : Build observation error covariance matrix
- [`propose_parameters()`](http://flare-forecast.org/FLAREr/reference/propose_parameters.md)
  : Propose new parameters for use in data assimilation and forecasting
- [`create_flow_files()`](http://flare-forecast.org/FLAREr/reference/create_flow_files.md)
  : Generating a list of flow files in the flare_tempdir
