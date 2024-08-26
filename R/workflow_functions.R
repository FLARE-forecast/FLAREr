#' Get run configuration from s3 bucket
#'
#' @param configure_run_file file name (no path) of run configuration file
#' @param lake_directory full path to repository directory
#' @param config flare configuration object
#' @param clean_start logical; reset the configuration run to the base file in the configuration directory
#' @param config_set_name name of configuration set
#' @param sim_name name of simulation
#' @keywords internal
#'
#' @return list of configuration values
#' @export
#'
get_run_config <- function(configure_run_file = "configure_run.yml", lake_directory, config, clean_start = FALSE, config_set_name = "default", sim_name = NA){

  run_config <- yaml::read_yaml(file.path(lake_directory,"configuration",config_set_name,configure_run_file))
  if(is.na(sim_name)){
    sim_name <- run_config$sim_name
  }

  dir.create(file.path(lake_directory, "restart", config$location$site_id, sim_name), recursive = TRUE, showWarnings = FALSE)

  if(clean_start | !config$run_config$use_s3){
    restart_exists <- file.exists(file.path(lake_directory, "restart", config$location$site_id, sim_name, configure_run_file))
    if(!restart_exists){
      yaml::write_yaml(run_config, file.path(lake_directory,"restart", config$location$site_id, sim_name, configure_run_file))
    }else{
      #message("Using existing restart file")
    }
  }else if(config$run_config$use_s3){
    restart_exists <- suppressMessages(aws.s3::object_exists(object = file.path(stringr::str_split_fixed(config$s3$warm_start$bucket, "/", n = 2)[2],
                                                                                config$location$site_id, sim_name, configure_run_file),
                                                             bucket = stringr::str_split_fixed(config$s3$warm_start$bucket, "/", n = 2)[1],
                                                             region = stringr::str_split_fixed(config$s3$warm_start$endpoint, pattern = "\\.", n = 2)[1],
                                                             base_url = stringr::str_split_fixed(config$s3$warm_start$endpoint, pattern = "\\.", n = 2)[2],
                                                             use_https = as.logical(Sys.getenv("USE_HTTPS"))))

    if(restart_exists){
      aws.s3::save_object(object = file.path(stringr::str_split_fixed(config$s3$warm_start$bucket, "/", n = 2)[2], config$location$site_id, sim_name, configure_run_file),
                          bucket = stringr::str_split_fixed(config$s3$warm_start$bucket, "/", n = 2)[1],
                          file = file.path(lake_directory, "restart", config$location$site_id, sim_name, configure_run_file),
                          region = stringr::str_split_fixed(config$s3$warm_start$endpoint, pattern = "\\.", n = 2)[1],
                          base_url = stringr::str_split_fixed(config$s3$warm_start$endpoint, pattern = "\\.", n = 2)[2],
                          use_https = as.logical(Sys.getenv("USE_HTTPS")))
    }else{
      yaml::write_yaml(run_config, file.path(lake_directory,"restart", config$location$site_id, sim_name, configure_run_file))
    }
  }
  run_config <- yaml::read_yaml(file.path(lake_directory, "restart", config$location$site_id, sim_name, configure_run_file))
  invisible(run_config)
}


#' Get data from Github repository
#'
#' @param lake_directory full path to repository directory
#' @param directory the branch name on github
#' @param git_repo https of the github repository
#' @keywords internal
#'
#' @export
#'
get_git_repo <- function(lake_directory, directory, git_repo){
  setwd(file.path(lake_directory, "data_raw"))
  if(!dir.exists(file.path(lake_directory, "data_raw", directory))){
    system(paste("git clone --depth 1 --single-branch --branch",directory,git_repo, directory, sep = " "))
  }else{
    setwd(file.path(lake_directory, "data_raw", directory))
    system("git pull")
  }
  setwd(lake_directory)
}

#' Save target files to s3 bucket
#'
#' @param site_id four letter code for the site
#' @param cleaned_insitu_file full path of the cleaned insitu file
#' @param cleaned_met_file full path of the cleaned met file
#' @param cleaned_inflow_file full path of the cleaned inflow file
#' @param use_s3 logical; TRUE = use s3
#' @param config list of FLARE configurations
#' @keywords internal
#'
put_targets <- function(site_id, cleaned_insitu_file = NA, cleaned_met_file = NA, cleaned_inflow_file = NA, use_s3 = FALSE, config){

  if(use_s3){
    if(!is.na(cleaned_insitu_file)){
      aws.s3::put_object(file = cleaned_insitu_file,
                         object = file.path(stringr::str_split_fixed(config$s3$targets$bucket, "/", n = 2)[2], site_id, basename(cleaned_insitu_file)),
                         bucket = stringr::str_split_fixed(config$s3$targets$bucket, "/", n = 2)[1],
                         region = stringr::str_split_fixed(config$s3$targets$endpoint, pattern = "\\.", n = 2)[1],
                         base_url = stringr::str_split_fixed(config$s3$targets$endpoint, pattern = "\\.", n = 2)[2],
                         use_https = as.logical(Sys.getenv("USE_HTTPS")))
    }
    if(!is.na(cleaned_inflow_file)){
      aws.s3::put_object(file = cleaned_inflow_file,
                         object = file.path(stringr::str_split_fixed(config$s3$targets$bucket, "/", n = 2)[2], site_id, basename(cleaned_inflow_file)),
                         bucket = stringr::str_split_fixed(config$s3$targets$bucket, "/", n = 2)[1],
                         region = stringr::str_split_fixed(config$s3$targets$endpoint, pattern = "\\.", n = 2)[1],
                         base_url = stringr::str_split_fixed(config$s3$targets$endpoint, pattern = "\\.", n = 2)[2],
                         use_https = as.logical(Sys.getenv("USE_HTTPS")))
    }
    if(!is.na(cleaned_met_file)){
      aws.s3::put_object(file = cleaned_met_file,
                         object = file.path(stringr::str_split_fixed(config$s3$targets$bucket, "/", n = 2)[2], site_id, basename(cleaned_met_file)),
                         bucket = stringr::str_split_fixed(config$s3$targets$bucket, "/", n = 2)[1],
                         region = stringr::str_split_fixed(config$s3$targets$endpoint, pattern = "\\.", n = 2)[1],
                         base_url = stringr::str_split_fixed(config$s3$targets$endpoint, pattern = "\\.", n = 2)[2],
                         use_https = as.logical(Sys.getenv("USE_HTTPS")))
    }
  }
}

#' Download target data from s3
#'
#' @param lake_directory full path to repository directory
#' @param config flare configuration object
#' @keywords internal
#'
get_targets <- function(lake_directory, config){
  if(config$run_config$use_s3){
    download_s3_objects(lake_directory,
                        bucket = stringr::str_split_fixed(config$s3$targets$bucket, "/", n = 2)[1],
                        prefix = file.path(stringr::str_split_fixed(config$s3$targets$bucket, "/", n = 2)[2], config$location$site_id),
                        region = stringr::str_split_fixed(config$s3$targets$endpoint, pattern = "\\.", n = 2)[1],
                        base_url = stringr::str_split_fixed(config$s3$targets$endpoint, pattern = "\\.", n = 2)[2])
  }
}

#' Get file path for driver forecasts
#'
#' @param config flare configuration object
#' @param forecast_model name of forecast model (i.e "noaa/NOAAGEFS_1hr); path relative to driver directory.
#'
#' @return full path to driver forecast
#' @keywords internal
#'
get_driver_forecast_path <- function(config, forecast_model){
  if(config$run_config$forecast_horizon > 0){
    # Set up timings
    #Weather Drivers
    start_datetime <- lubridate::as_datetime(config$run_config$start_datetime)
    if(is.na(config$run_config$forecast_start_datetime)){
      end_datetime <- lubridate::as_datetime(config$run_config$end_datetime)
      forecast_start_datetime <- end_datetime
    }else{
      forecast_start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime)
      end_datetime <- forecast_start_datetime + lubridate::days(config$run_config$forecast_horizon)
    }
    forecast_hour <- lubridate::hour(forecast_start_datetime)
    if(forecast_hour < 10){forecast_hour <- paste0("0",forecast_hour)}
    forecast_path <- file.path(forecast_model,
                               config$location$site_id,
                               lubridate::as_date(forecast_start_datetime),forecast_hour)
  }else{
    forecast_path <- NULL
  }
  return(forecast_path)
}

#' Set and create directories in the configuration file
#'
#' @param configure_run_file name of run configuration file (do not include full path)
#' @param lake_directory full path to repository directory
#' @param clean_start logical: TRUE = reset run configuration with the file in the configuration directory within repository
#' @param config_set_name name of configuration set
#' @param sim_name name of simulation
#'
#' @return list of configuration values
#' @keywords internal
#'
set_configuration <- function(configure_run_file = "configure_run.yml", lake_directory, clean_start = FALSE, config_set_name = "default", sim_name = NA){

  run_config <- yaml::read_yaml(file.path(lake_directory,"configuration",config_set_name,configure_run_file))
  config <- yaml::read_yaml(file.path(lake_directory,"configuration",config_set_name,run_config$configure_flare))
  config$run_config <- run_config
  config$file_path$qaqc_data_directory <- file.path(lake_directory, "targets", config$location$site_id)
  config$file_path$data_directory <- file.path(lake_directory, "data_raw")
  config$file_path$noaa_directory <- file.path(lake_directory, "drivers")
  config$file_path$configuration_directory <- file.path(lake_directory, "configuration",config_set_name)
  config$file_path$inflow_directory <- file.path(lake_directory, "drivers")
  config$file_path$analysis_directory <- file.path(lake_directory, "analysis")
  config$file_path$forecast_output_directory <- file.path(lake_directory, "forecasts", config$location$site_id)
  dir.create(config$file_path$qaqc_data_directory, recursive = TRUE, showWarnings = FALSE)
  dir.create(config$file_path$forecast_output_directory, recursive = TRUE, showWarnings = FALSE)
  dir.create(config$file_path$analysis_directory, recursive = TRUE, showWarnings = FALSE)
  dir.create(config$file_path$data_directory, recursive = TRUE, showWarnings = FALSE)
  dir.create(config$file_path$noaa_directory, recursive = TRUE, showWarnings = FALSE)
  dir.create(config$file_path$inflow_directory, recursive = TRUE, showWarnings = FALSE)

  run_config <- get_run_config(configure_run_file, lake_directory, config, clean_start, config_set_name = config_set_name, sim_name = sim_name)
  config$run_config <- run_config
  config$file_path$restart_directory <- file.path(lake_directory, "restart", config$location$site_id, config$run_config$sim_name)

  config$file_path$execute_directory <- file.path(lake_directory, "flare_tempdir", config$location$site_id, config$run_config$sim_name)
  dir.create(config$file_path$execute_directory, recursive = TRUE, showWarnings = FALSE)

  if(Sys.getenv(x = "AWS_ACCESS_KEY_ID") == "" & config$run_config$use_s3 == TRUE){
    warning(paste0(" Use s3 is set to TRUE in ",file.path(lake_directory,"configuration",config_set_name,configure_run_file),
                   "AWS_ACCESS_KEY_ID environment variable is not set.  s3 can still be used for downloading"))
  }

  if(Sys.getenv(x = "AWS_SECRET_ACCESS_KEY") == "" & config$run_config$use_s3 == TRUE){
    warning(paste0(" Use s3 is set to TRUE in ",file.path(lake_directory,"configuration",config_set_name,configure_run_file),
                   "AWS_SECRET_ACCESS_KEY environment variable is not set.  s3 can still be used for downloading"))
  }

  invisible(config)
}

#' Download restart file from s3 bucket
#'
#' @param config flare configuration object
#' @param lake_directory full path to repository directory
#'
#' @return list of updated configuration values
#' @keywords internal
#'
get_restart_file <- function(config, lake_directory){
  if(!is.na(config$run_config$restart_file)){
    restart_file <- basename(config$run_config$restart_file)
    if(config$run_config$use_s3){
      aws.s3::save_object(object = file.path(stringr::str_split_fixed(config$s3$forecasts$bucket, "/", n = 2)[2], config$location$site_id, restart_file),
                          bucket = stringr::str_split_fixed(config$s3$forecasts$bucket, "/", n = 2)[1],
                          file = file.path(lake_directory, "forecasts", config$location$site_id, restart_file),
                          region = stringr::str_split_fixed(config$s3$forecasts$endpoint, pattern = "\\.", n = 2)[1],
                          base_url = stringr::str_split_fixed(config$s3$forecasts$endpoint, pattern = "\\.", n = 2)[2],
                          use_https = as.logical(Sys.getenv("USE_HTTPS")))
    }
    config$run_config$restart_file <- file.path(lake_directory, "forecasts", config$location$site_id, restart_file)
  }
  return(config)
}

#' Update run configuration
#'
#' @param lake_directory full path to repository directory
#' @param configure_run_file name of run configuration file (do not include full path)
#' @param restart_file full path of saved FLARE netcdf
#' @param start_datetime first datetime of the simulation
#' @param end_datetime last datetime of the simulation
#' @param forecast_start_datetime datetime that a forecast starts
#' @param forecast_horizon number of days forecasted
#' @param sim_name name of particular FLARE simulation
#' @param site_id four letter code for the site
#' @param configure_flare list of FLARE configurations
#' @param configure_obs list of observation configurations
#' @param use_s3 logical; TRUE = use s3
#' @param bucket s3 bucket
#' @param endpoint s3 endpoint
#' @param use_https TRUE/FALSE use https when using s3

#' @export
#' @examples

#' dir <- tempdir()
#' lake_directory <- file.path(dir, "extdata")
#' #Copy files to temporarly directory
#' dir.create(dir,showWarnings = FALSE)
#' file.copy(system.file("extdata", package = "FLAREr"),
#'           tempdir(),
#'           recursive = TRUE)
#' print(lake_directory)
#' dir.create(file.path(lake_directory, "restart/fcre/test"),
#'            recursive = TRUE,
#'            showWarnings = FALSE)
#' file.copy(file.path(lake_directory, "configuration/default/configure_run.yml"),
#'           file.path(lake_directory, "restart/fcre/test/configure_run.yml"),
#'           overwrite = TRUE)

#'update_run_config2(lake_directory,
#'                   configure_run_file = "configure_run.yml",
#'                   restart_file = NA,
#'                   start_datetime = "2022-10-01 00:00:00",
#'                   end_datetime = NA,
#'                   forecast_start_datetime = "2022-10-10 00:00:00",
#'                   forecast_horizon = 20,
#'                   sim_name = "test",
#'                   site_id = "fcre",
#'                   configure_flare = "configure_flare.yml",
#'                   configure_obs = NULL,
#'                   use_s3 = FALSE,
#'                   bucket = NULL,
#'                   endpoint = NULL)
#'
update_run_config <- function(lake_directory,
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
                               use_https = TRUE){

  run_config <- NULL

  run_config$restart_file <- restart_file
  run_config$start_datetime <- as.character(lubridate::as_datetime(start_datetime))
  if(lubridate::hour(run_config$start_datetime) == 0){
    run_config$start_datetime <- paste(run_config$start_datetime, "00:00:00")
  }

  run_config$forecast_start_datetime <- as.character(lubridate::as_datetime(forecast_start_datetime))
  if(!is.na(run_config$forecast_start_datetime)){
    if(lubridate::hour(run_config$forecast_start_datetime) == 0){
      run_config$forecast_start_datetime <- paste(run_config$forecast_start_datetime, "00:00:00")
    }
  }

  run_config$end_datetime <- as.character(lubridate::as_datetime(end_datetime))
  if(!is.na(run_config$end_datetime)){
    if(lubridate::hour(run_config$end_datetime) == 0){
      run_config$end_datetime <- paste(run_config$end_datetime, "00:00:00")
    }
  }

  run_config$forecast_horizon <- forecast_horizon
  run_config$sim_name <- sim_name
  run_config$configure_flare <- configure_flare
  run_config$configure_obs <- configure_obs
  run_config$use_s3 <- use_s3

  file_name <- file.path(lake_directory,"restart",site_id, sim_name, configure_run_file)
  yaml::write_yaml(run_config, file_name)
  if(use_s3){
    aws.s3::put_object(file = file_name,
                       object = file.path(stringr::str_split_fixed(bucket, "/", n = 2)[2], site_id, sim_name, configure_run_file),
                       bucket = stringr::str_split_fixed(bucket, "/", n = 2)[1],
                       region = stringr::str_split_fixed(endpoint, pattern = "\\.", n = 2)[1],
                       base_url = stringr::str_split_fixed(endpoint, pattern = "\\.", n = 2)[2],
                       use_https = as.logical(Sys.getenv("USE_HTTPS")))
  }
}

#' Upload forecast file and metadata to s3 bucket
#'
#' @param saved_file full path of saved FLARE netcdf
#' @param eml_file_name full path of saved FLARE metadata
#' @param config flare configuration object
#'
#' @keywords internal
#'
put_forecast <- function(saved_file, eml_file_name = NULL, config){
  if(config$run_config$use_s3){
    success <- aws.s3::put_object(file = saved_file,
                                  object = file.path(stringr::str_split_fixed(config$s3$forecasts$bucket, "/", n = 2)[2], config$location$site_id, basename(saved_file)),
                                  bucket = stringr::str_split_fixed(config$s3$forecasts$bucket, "/", n = 2)[1],
                                  region = stringr::str_split_fixed(config$s3$forecasts$endpoint, pattern = "\\.", n = 2)[1],
                                  base_url = stringr::str_split_fixed(config$s3$forecasts$endpoint, pattern = "\\.", n = 2)[2],
                                  use_https = as.logical(Sys.getenv("USE_HTTPS")))
    if(success){
      unlink(saved_file)
    }
    if(!is.null(eml_file_name)){
      success <- aws.s3::put_object(file = eml_file_name,
                                    object = file.path(stringr::str_split_fixed(config$s3$forecasts$bucket, "/", n = 2)[2], config$location$site_id, basename(eml_file_name)),
                                    bucket = stringr::str_split_fixed(config$s3$forecasts$bucket, "/", n = 2)[1],
                                    region = stringr::str_split_fixed(config$s3$forecasts$endpoint, pattern = "\\.", n = 2)[1],
                                    base_url = stringr::str_split_fixed(config$s3$forecasts$endpoint, pattern = "\\.", n = 2)[2],
                                    use_https = as.logical(Sys.getenv("USE_HTTPS")))
      if(success){
        unlink(eml_file_name)
      }
    }
  }
}

#' @title Download file from s3 bucket
#'
#' @param lake_directory full path to repository directory
#' @param bucket name of s3 bucket
#' @param prefix relative path directory within bucket
#' @param region S3 region
#' @param base_url S3 endpoint
#' @noRd
#' @keywords internal
download_s3_objects <- function(lake_directory, bucket, prefix, region, base_url){

  files <- aws.s3::get_bucket(bucket = bucket,
                              prefix = prefix,
                              region = region,
                              base_url = base_url,
                              use_https = as.logical(Sys.getenv("USE_HTTPS")))
  keys <- vapply(files, `[[`, "", "Key", USE.NAMES = FALSE)
  empty <- grepl("/$", keys)
  keys <- keys[!empty]
  if(length(keys) > 0){
    for(i in 1:length(keys)){
      aws.s3::save_object(object = keys[i],
                          bucket = bucket,
                          file = file.path(lake_directory, bucket, keys[i]),
                          region = region,
                          base_url = base_url,
                          use_https = as.logical(Sys.getenv("USE_HTTPS")))
    }
  }
}

#' Delete restart file on s3 bucket
#'
#' @param site_id four letter code for site
#' @param sim_name name of simulation
#' @param bucket s3 bucket
#' @param endpoint S3 endpoint
#'
#' @keywords internal
#'
delete_restart <- function(site_id, sim_name, bucket = "restart", endpoint){
  files <- aws.s3::get_bucket(bucket = stringr::str_split_fixed(bucket, "/", n = 2)[1],
                              prefix = file.path(stringr::str_split_fixed(bucket, "/", n = 2)[2], site_id, sim_name),
                              region = stringr::str_split_fixed(endpoint, pattern = "\\.", n = 2)[1],
                              base_url = stringr::str_split_fixed(endpoint, pattern = "\\.", n = 2)[2],
                              use_https = as.logical(Sys.getenv("USE_HTTPS")))
  keys <- vapply(files, `[[`, "", "Key", USE.NAMES = FALSE)
  empty <- grepl("/$", keys)
  keys <- keys[!empty]
  if(length(keys > 0)){
    for(i in 1:length(keys)){
      aws.s3::delete_object(object = keys[i],
                            bucket = stringr::str_split_fixed(bucket, "/", n = 2)[1],
                            region = stringr::str_split_fixed(endpoint, pattern = "\\.", n = 2)[1],
                            base_url = stringr::str_split_fixed(endpoint, pattern = "\\.", n = 2)[2],
                            use_https = as.logical(Sys.getenv("USE_HTTPS")))
    }
  }
}

#' Set and create directories for observation configuration
#'
#' @param lake_directory full path to repository directory
#' @noRd
#' @return list of configuration values
#' @keywords internal
#'
initialize_obs_processing <- function(lake_directory, observation_yml = NA, config_set_name = "default"){

  curr_dir1 <- file.path(lake_directory, "data_raw")
  if(!dir.exists(curr_dir1)){
    dir.create(curr_dir1, recursive = TRUE)
  }
  curr_dir2 <- file.path(lake_directory, "targets")
  if(!dir.exists(curr_dir2)){
    dir.create(curr_dir2, recursive = TRUE)
  }

  if(!is.na(observation_yml)){
    config_obs <- yaml::read_yaml(file.path(lake_directory,"configuration",config_set_name, observation_yml))
    config_obs$file_path$data_directory <- curr_dir1
    config_obs$file_path$targets_directory <- curr_dir2
    return(config_obs)
  }else{
    return(NULL)
  }

  return(config_obs)
}

#' Check if NOAA forecasts have been downloaded and processed
#'
#' @param lake_directory four-letter code for site
#' @param configure_run_file name of simulation
#' @param config_set_name FLARE configuration object (needed for s3 buckets and endpoit)
#'
#' @return logical
#' @export
#' @examplesIf interactive()
#'
#' dir <- tempdir()
#' lake_directory <- file.path(dir, "extdata")
#' # Copy files to temporarly directory
#' dir.create(dir,showWarnings = FALSE)
#' file.copy(system.file("extdata", package = "FLAREr"),
#'           tempdir(),
#'           recursive = TRUE)
#'
#' check_noaa_present(lake_directory,
#'                    configure_run_file = "configure_run.yml",
#'                    config_set_name = "default")

check_noaa_present <- function(lake_directory, configure_run_file = "configure_run.yml", config_set_name = "default"){

  config <- set_configuration(configure_run_file, lake_directory, config_set_name = config_set_name)

  if(config$run_config$forecast_horizon > 0 & config$met$future_met_model == 'gefs-v12/stage2'){

    met_start_datetime <- lubridate::as_datetime(config$run_config$start_datetime)
    met_forecast_start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime)

    if(config$run_config$forecast_horizon > 16){
      met_forecast_start_datetime <- met_forecast_start_datetime - lubridate::days(config$met$forecast_lag_days)
      if(met_forecast_start_datetime < met_start_datetime){
        met_start_datetime <- met_forecast_start_datetime
        message("horizon is > 16 days so adjusting forecast_start_datetime in the met file generation to use yesterdays forecast. But adjusted forecast_start_datetime < start_datetime")
      }
    }

    forecast_date <- lubridate::as_date(met_forecast_start_datetime)
    forecast_hour <- lubridate::hour(met_forecast_start_datetime)
    site <- config$location$site_id
    forecast_horizon <- config$run_config$forecast_horizon

    vars <- arrow_env_vars()

    forecast_dir <- arrow::s3_bucket(bucket = file.path(config$s3$drivers$bucket,  config$met$future_met_model),
                                         endpoint_override =  config$s3$drivers$endpoint, anonymous = TRUE)
    avail_dates <- gsub("reference_datetime=", "", forecast_dir$ls())

    unset_arrow_vars(vars)

    if(forecast_date %in% lubridate::as_date(avail_dates)){
        avial_horizons <- arrow::open_dataset(forecast_dir$path(paste0("reference_datetime=",as.character(forecast_date)))) |>
          filter(variable == "air_temperature",
                 site_id == site) |>
          collect() |>
          mutate(horizon = as.numeric(datetime - lubridate::as_datetime(forecast_date)) / (60 * 60)) |>
          group_by(parameter) |>
          summarize(max_horizon = max(horizon)) |>
          ungroup() |>
          mutate(over = ifelse(max_horizon >= forecast_horizon * 24, 1, 0)) |>
          summarize(sum = sum(over))

          if(avial_horizons$sum == 31){
            noaa_forecasts_ready <- TRUE
          }else{
            noaa_forecasts_ready <- FALSE
          }
      }else{
        noaa_forecasts_ready <- FALSE
      }
    }else{
      noaa_forecasts_ready <- TRUE
    }

  if(!noaa_forecasts_ready){
    message(paste0("waiting for NOAA forecast: ", config$run_config$forecast_start_datetime))
  }
  return(noaa_forecasts_ready)

}


#' Delete simulation on s3 bucket
#'
#' @param site_id four letter code for site
#' @param sim_name name of simulation
#' @param config FLARE configuration object (needed for s3 buckets and endpoit)
#' @keywords internal
#'
#'
delete_sim <- function(site_id, sim_name, config){

  go <- utils::askYesNo(paste0("Do you want to delete the files for ",sim_name," from ",site_id))

  if(go){
    message("deleting analysis files")
    files <- aws.s3::get_bucket(bucket = stringr::str_split_fixed(config$s3$analysis$bucket, "/", n = 2)[1],
                                prefix = file.path(stringr::str_split_fixed(config$s3$analysis$bucket, "/", n = 2)[2], site_id),
                                region = stringr::str_split_fixed(config$s3$analysis$endpoint, pattern = "\\.", n = 2)[1],
                                base_url = stringr::str_split_fixed(config$s3$analysis$endpoint, pattern = "\\.", n = 2)[2],
                                use_https = as.logical(Sys.getenv("USE_HTTPS")))
    keys <- vapply(files, `[[`, "", "Key", USE.NAMES = FALSE)
    empty <- grepl("/$", keys)
    keys <- keys[!empty]
    keys <- keys[stringr::str_detect(keys, sim_name)]
    if(length(keys > 0)){
      for(i in 1:length(keys)){
        aws.s3::delete_object(object = keys[i],
                              bucket = stringr::str_split_fixed(config$s3$analysis$bucket, "/", n = 2)[1],
                              region = stringr::str_split_fixed(config$s3$analysis$endpoint, pattern = "\\.", n = 2)[1],
                              base_url = stringr::str_split_fixed(config$s3$analysis$endpoint, pattern = "\\.", n = 2)[2],
                              use_https = as.logical(Sys.getenv("USE_HTTPS")))
      }
    }

    #forecasts
    message("deleting forecast files")
    files <- aws.s3::get_bucket(bucket = stringr::str_split_fixed(config$s3$forecasts$bucket, "/", n = 2)[1],
                                prefix = file.path(stringr::str_split_fixed(config$s3$forecasts$bucket, "/", n = 2)[2], site_id),
                                region = stringr::str_split_fixed(config$s3$forecasts$endpoint, pattern = "\\.", n = 2)[1],
                                base_url = stringr::str_split_fixed(config$s3$forecasts$endpoint, pattern = "\\.", n = 2)[2],
                                use_https = as.logical(Sys.getenv("USE_HTTPS")))
    keys <- vapply(files, `[[`, "", "Key", USE.NAMES = FALSE)
    empty <- grepl("/$", keys)
    keys <- keys[!empty]
    keys <- keys[stringr::str_detect(keys, sim_name)]
    if(length(keys > 0)){
      for(i in 1:length(keys)){
        aws.s3::delete_object(object = keys[i],
                              bucket = stringr::str_split_fixed(config$s3$forecasts$bucket, "/", n = 2)[1],
                              region = stringr::str_split_fixed(config$s3$forecasts$endpoint, pattern = "\\.", n = 2)[1],
                              base_url = stringr::str_split_fixed(config$s3$forecasts$endpoint, pattern = "\\.", n = 2)[2],
                              use_https = as.logical(Sys.getenv("USE_HTTPS")))
      }
    }

    message("deleting restart files")
    files <- aws.s3::get_bucket(bucket = stringr::str_split_fixed(config$s3$warm_start$bucket, "/", n = 2)[1],
                                prefix = file.path(stringr::str_split_fixed(config$s3$warm_start$bucket, "/", n = 2)[2], site_id, sim_name),
                                region = stringr::str_split_fixed(config$s3$warm_start$endpoint, pattern = "\\.", n = 2)[1],
                                base_url = stringr::str_split_fixed(config$s3$warm_start$endpoint, pattern = "\\.", n = 2)[2],
                                use_https = as.logical(Sys.getenv("USE_HTTPS")))
    keys <- vapply(files, `[[`, "", "Key", USE.NAMES = FALSE)
    empty <- grepl("/$", keys)
    keys <- keys[!empty]
    if(length(keys > 0)){
      for(i in 1:length(keys)){
        aws.s3::delete_object(object = keys[i],
                              bucket = stringr::str_split_fixed(config$s3$warm_start$bucket, "/", n = 2)[1],
                              region = stringr::str_split_fixed(config$s3$warm_start$endpoint, pattern = "\\.", n = 2)[1],
                              base_url = stringr::str_split_fixed(config$s3$warm_start$endpoint, pattern = "\\.", n = 2)[2],
                              use_https = as.logical(Sys.getenv("USE_HTTPS")))
      }
    }
  }
}


arrow_env_vars <- function(){
  user_region <- Sys.getenv("AWS_DEFAULT_REGION")
  user_meta <- Sys.getenv("AWS_EC2_METADATA_DISABLED")
  Sys.unsetenv("AWS_DEFAULT_REGION")
  Sys.setenv(AWS_EC2_METADATA_DISABLED="TRUE")

  list(user_region=user_region, user_meta = user_meta)
}

unset_arrow_vars <- function(vars) {
  Sys.setenv("AWS_DEFAULT_REGION" = vars$user_region)
  if (vars$user_meta != "") {
    Sys.setenv(AWS_EC2_METADATA_DISABLED = vars$user_meta)
  }
}



