# Update run configuration

Update run configuration

## Usage

``` r
update_run_config(
  lake_directory,
  configure_run_file,
  restart_file,
  start_datetime,
  end_datetime,
  forecast_start_datetime,
  forecast_horizon,
  sim_name,
  site_id,
  configure_flare,
  configure_obs,
  use_s3,
  bucket,
  endpoint,
  config = NULL,
  use_https = TRUE
)
```

## Arguments

- lake_directory:

  full path to repository directory

- configure_run_file:

  name of run configuration file (do not include full path)

- restart_file:

  full path of saved FLARE netcdf

- start_datetime:

  first datetime of the simulation

- end_datetime:

  last datetime of the simulation

- forecast_start_datetime:

  datetime that a forecast starts

- forecast_horizon:

  number of days forecasted

- sim_name:

  name of particular FLARE simulation

- site_id:

  four letter code for the site

- configure_flare:

  list of FLARE configurations

- configure_obs:

  list of observation configurations

- use_s3:

  logical; TRUE = use s3

- bucket:

  s3 bucket

- endpoint:

  s3 endpoint

- config:

  Configuration object to be updated

- use_https:

  TRUE/FALSE use https when using s3

## Examples

``` r
dir <- normalizePath(tempdir(),  winslash = "/")
lake_directory <- file.path(dir, "extdata")
#Copy files to temporarly directory
dir.create(dir,showWarnings = FALSE)
file.copy(system.file("extdata", package = "FLAREr"),
          tempdir(),
          recursive = TRUE)
#> [1] TRUE
print(lake_directory)
#> [1] "/tmp/RtmpznmqY1/extdata"
dir.create(file.path(lake_directory, "restart/fcre/test"),
           recursive = TRUE,
           showWarnings = FALSE)
file.copy(file.path(lake_directory, "configuration/default/configure_run.yml"),
          file.path(lake_directory, "restart/fcre/test/configure_run.yml"),
          overwrite = TRUE)
#> [1] TRUE
update_run_config(lake_directory,
                  configure_run_file = "configure_run.yml",
                  restart_file = NA,
                  start_datetime = "2022-10-01 00:00:00",
                  end_datetime = NA,
                  forecast_start_datetime = "2022-10-10 00:00:00",
                  forecast_horizon = 20,
                  sim_name = "test",
                  site_id = "fcre",
                  configure_flare = "configure_flare.yml",
                  configure_obs = NULL,
                  use_s3 = FALSE,
                  bucket = NULL,
                  endpoint = NULL)
```
