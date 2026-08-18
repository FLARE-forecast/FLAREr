# Generating a list of flow files in the flare_tempdir

Generating a list of flow files in the flare_tempdir

## Usage

``` r
create_flow_files(
  flow_forecast_dir = NULL,
  flow_historical_dir = NULL,
  flow_type = "inflow",
  variables = c("time", "FLOW", "TEMP", "SALT"),
  out_dir,
  start_datetime,
  end_datetime = NA,
  forecast_start_datetime = NA,
  forecast_horizon = 0,
  site_id,
  use_s3 = FALSE,
  bucket = NULL,
  endpoint = NULL,
  local_directory = NULL,
  use_ler_vars = FALSE,
  config = config,
  out_dir_fn = NULL
)
```

## Arguments

- flow_forecast_dir:

  location of the forecast files

- flow_historical_dir:

  location of the historical files

- flow_type:

  inflow or outflow

- variables:

  what variables are included in the flow file

- out_dir:

  the directory in which to put the flow files (e.g. flare_tempdir)

- start_datetime:

  start of simulation

- end_datetime:

  end of simulation

- forecast_start_datetime:

  start of the forecast period (break between historical + future
  periods)

- forecast_horizon:

  horizon

- site_id:

  site code

- use_s3:

  logical

- bucket:

  s3 storage location

- endpoint:

  s3 storage location

- local_directory:

  local storage location

- use_ler_vars:

  T/F

- config:

  list of FLARE configurations, used for S3 access when `use_s3 = TRUE`

- out_dir_fn:

  Optional function of the ensemble positional index (integer, 1-based)
  returning the directory for that member's flow file. When non-NULL
  each file is written to `out_dir_fn(i)`; when NULL files are written
  to `out_dir`. Default `NULL`.

## Value

matrix of flow_file_names
