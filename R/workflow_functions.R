#' Get run configuration from s3 bucket
#'
#' @param configure_run_file file name (no path) of run configuration file
#' @param lake_directory full path to repository directory
#' @param config flare configuration object
#' @param clean_start logical; reset the configuration run to the base file in the configuration directory
#'
#' @return
#' @export
#'
get_run_config <- function(configure_run_file = "configure_run.yml", lake_directory, config, clean_start = FALSE, config_set_name = "default"){

  if(clean_start | !config$run_config$use_s3){
    restart_exists <- file.exists(file.path(lake_directory, "restart", config$location$site_id, config$run_config$sim_name, configure_run_file))
    if(!restart_exists){
      file.copy(file.path(lake_directory,"configuration",config_set_name,configure_run_file), file.path(lake_directory, "restart", config$location$site_id, config$run_config$sim_name, configure_run_file))
    }
  }else if(config$run_config$use_s3){
    restart_exists <- suppressMessages(aws.s3::object_exists(object = file.path(config$location$site_id, config$run_config$sim_name, configure_run_file),
                                                             bucket = "restart",
                                                             region = Sys.getenv("AWS_DEFAULT_REGION"),
                                                             use_https = as.logical(Sys.getenv("USE_HTTPS"))))
    if(restart_exists){
      aws.s3::save_object(object = file.path(config$location$site_id, config$run_config$sim_name, configure_run_file),
                          bucket = "restart",
                          file = file.path(lake_directory, "restart", config$location$site_id, config$run_config$sim_name, configure_run_file),
                          region = Sys.getenv("AWS_DEFAULT_REGION"),
                          use_https = as.logical(Sys.getenv("USE_HTTPS")))
    }else{
      file.copy(file.path(lake_directory,"configuration",config_set_name,configure_run_file), file.path(lake_directory, "restart", config$location$site_id, config$run_config$sim_name, configure_run_file))
    }
  }
  run_config <- yaml::read_yaml(file.path(lake_directory, "restart", config$location$site_id, config$run_config$sim_name, configure_run_file))
  return(run_config)
}

#' Get data from Github repository
#'
#' @param lake_directory full path to repository directory
#' @param directory the branch name on github
#' @param git_repo https of the github repository
#'
#' @return
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

#' Download file from EDI data portal
#'
#' @param edi_https https of the EDI package
#' @param file name of the file in the EDI package (not full path)
#' @param lake_directory full path to repository directory
#'
#' @return
#' @export
#'
get_edi_file <- function(edi_https, file, lake_directory){

  if(!file.exists(file.path(lake_directory, "data_raw", file))){
    if(!dir.exists(dirname(file.path(lake_directory, "data_raw", file)))){
      dir.create(dirname(file.path(lake_directory, "data_raw", file)))
    }
    download.file(edi_https,
                  destfile = file.path(lake_directory, "data_raw", file),
                  method="curl")
  }
}


#' Save target files to s3 bucket
#'
#' @param site_id four letter code for the site
#' @param cleaned_insitu_file full path of the cleaned insitu file
#' @param cleaned_met_file full path of the cleaned met file
#' @param cleaned_inflow_file full path of the cleaned inflow file
#' @param use_s3 logical; TRUE = use s3
#'
#' @return
#' @export
#'
put_targets <- function(site_id, cleaned_insitu_file = NA, cleaned_met_file = NA, cleaned_inflow_file = NA, use_s3 = FALSE){

  if(use_s3){
    if(!is.na(cleaned_insitu_file)){
      aws.s3::put_object(file = cleaned_insitu_file,
                         object = file.path(site_id, basename(cleaned_insitu_file)),
                         bucket = "targets",
                         region = Sys.getenv("AWS_DEFAULT_REGION"),
                         use_https = as.logical(Sys.getenv("USE_HTTPS")))
    }
    if(!is.na(cleaned_inflow_file)){
      aws.s3::put_object(file = cleaned_inflow_file,
                         object = file.path(site_id, basename(cleaned_inflow_file)),
                         bucket = "targets",
                         region = Sys.getenv("AWS_DEFAULT_REGION"),
                         use_https = as.logical(Sys.getenv("USE_HTTPS")))
    }
    if(!is.na(cleaned_met_file)){
      aws.s3::put_object(file = cleaned_met_file,
                         object = file.path(site_id, basename(cleaned_met_file)),
                         bucket = "targets",
                         region = Sys.getenv("AWS_DEFAULT_REGION"),
                         use_https = as.logical(Sys.getenv("USE_HTTPS")))
    }
  }
}

#' Download target data from s3
#'
#' @param lake_directory full path to repository directory
#' @param config flare configuration object
#'
#' @return
#' @export
#'
get_targets <- function(lake_directory, config){
  if(config$run_config$use_s3){
    download_s3_objects(lake_directory,
                        bucket = "targets",
                        prefix = config$location$site_id)
  }
}


#' Download stacked NOAA data from s3 bucket
#'
#' @param lake_directory full path to repository directory
#' @param config flare configuration object
#' @param averaged logistical; TRUE = download averaged stacked forecast
#'
#' @return
#' @export
#'
get_stacked_noaa <- function(lake_directory, config, averaged = TRUE){
  if(averaged){
    download_s3_objects(lake_directory, bucket = "drivers", prefix = file.path("noaa/NOAAGEFS_1hr_stacked_average",config$location$site_id))
  }else{
    download_s3_objects(lake_directory, bucket = "drivers", prefix = file.path("noaa/NOAAGEFS_1hr_stacked",config$location$site_id))
  }
}

#' Get file path for driver forecasts
#'
#' @param config flare configuration object
#' @param forecast_model name of forecast model (i.e "noaa/NOAAGEFS_1hr); path relative to driver directory.
#'
#' @return
#' @export
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

#' Download driver forecasts from s3 bucket
#'
#' @param lake_directory full path to repository directory
#' @param forecast_path relative path of the driver forecast (relative to driver directory or bucket)
#'
#' @return
#' @export
#'
get_driver_forecast <- function(lake_directory, forecast_path){

  download_s3_objects(lake_directory,
                      bucket = "drivers",
                      prefix = forecast_path)
}

#' Set and create directories in the configuration file
#'
#' @param configure_run_file name of run configuration file (do not include full path)
#' @param lake_directory full path to repository directory
#' @param clean_start logical: TRUE = reset run configuration with the file in the configuration directory within repository
#'
#' @return
#' @export
#'
set_configuration <- function(configure_run_file = "configure_run.yml", lake_directory, clean_start = FALSE, config_set_name = "default"){
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
  config$file_path$restart_directory <- file.path(lake_directory, "restart", config$location$site_id, config$run_config$sim_name)
  config$file_path$execute_directory <- file.path(lake_directory, "flare_tempdir", config$location$site_id, run_config$sim_name)
  if(!dir.exists(config$file_path$qaqc_data_directory)){
    dir.create(config$file_path$qaqc_data_directory, recursive = TRUE)
  }

  if(!dir.exists(config$file_path$forecast_output_directory)){
    dir.create(config$file_path$forecast_output_directory, recursive = TRUE)
  }

  if(!dir.exists(config$file_path$restart_directory)){
    dir.create(config$file_path$restart_directory, recursive = TRUE)
  }

  if(!dir.exists(config$file_path$analysis_directory)){
    dir.create(config$file_path$analysis_directory, recursive = TRUE)
  }

  if(!dir.exists(config$file_path$data_directory)){
    dir.create(config$file_path$data_directory, recursive = TRUE)
  }

  if(!dir.exists(config$file_path$noaa_directory)){
    dir.create(config$file_path$noaa_directory, recursive = TRUE)
  }

  if(!dir.exists(config$file_path$inflow_directory)){
    dir.create(config$file_path$inflow_directory, recursive = TRUE)
  }

  if(!dir.exists(config$file_path$execute_directory)){
    dir.create(config$file_path$execute_directory, recursive = TRUE)
  }

  run_config <- get_run_config(configure_run_file, lake_directory, config, clean_start, config_set_name = config_set_name)
  config$run_config <- run_config

  return(config)
}

#' Download restart file from s3 bucket
#'
#' @param config flare configuration object
#' @param lake_directory full path to repository directory
#'
#' @return
#' @export
#'
get_restart_file <- function(config, lake_directory){
  if(!is.na(config$run_config$restart_file)){
    restart_file <- basename(config$run_config$restart_file)
    if(config$run_config$use_s3){
      aws.s3::save_object(object = file.path(config$location$site_id, restart_file),
                          bucket = "forecasts",
                          file = file.path(lake_directory, "forecasts", config$location$site_id, restart_file),
                          region = Sys.getenv("AWS_DEFAULT_REGION"),
                          use_https = as.logical(Sys.getenv("USE_HTTPS")))
    }
    config$run_config$restart_file <- file.path(lake_directory, "forecasts", config$location$site_id, restart_file)
  }
  return(config)
}

#' Update run configuration and upload to s3 bucket
#'
#' @param config flare configuration object
#' @param lake_directory full path to repository directory
#' @param configure_run_file name of run configuration file (do not include full path)
#' @param saved_file full path of saved FLARE netcdf
#' @param new_horizon horizon (in days) to update the run configuration with
#' @param day_advance number of days between forecast forecast generation (defaults to 1)
#'
#' @return
#' @export
#'
update_run_config <- function(config, lake_directory, configure_run_file = "configure_run.yml", saved_file = NA, new_horizon = NA, day_advance = NA, new_start_datetime = TRUE){
  if(new_start_datetime){
    config$run_config$start_datetime <- config$run_config$forecast_start_datetime
  }
  if(!is.na(new_horizon)){
    if(!is.na(config$run_config$forecast_horizon)){
      config$run_config$forecast_horizon <- new_horizon
    }
  }
  if(!is.na(day_advance)){
    config$run_config$forecast_start_datetime <- as.character(lubridate::as_datetime(config$run_config$forecast_start_datetime) + lubridate::days(day_advance))
    if(lubridate::hour(config$run_config$forecast_start_datetime) == 0){
      config$run_config$forecast_start_datetime <- paste(config$run_config$forecast_start_datetime, "00:00:00")
    }
  }
  if(!is.na(saved_file)){
    config$run_config$restart_file <- basename(saved_file)
  }
  yaml::write_yaml(config$run_config, file = file.path(lake_directory,"restart",config$location$site_id,config$run_config$sim_name,configure_run_file))
  if(config$run_config$use_s3){
    aws.s3::put_object(file = file.path(lake_directory,"restart",config$location$site_id,config$run_config$sim_name, configure_run_file),
                       object = file.path(config$location$site_id,config$run_config$sim_name, configure_run_file),
                       bucket = "restart",
                       region = Sys.getenv("AWS_DEFAULT_REGION"),
                       use_https = as.logical(Sys.getenv("USE_HTTPS")))
  }
  invisible(config)
}

#' Update run configuration and upload to s3 bucket
#'
#' @param config flare configuration object
#' @param lake_directory full path to repository directory
#' @param configure_run_file name of run configuration file (do not include full path)
#'
#' @return
#' @export
#'
update_run_config_neon <- function(config, lake_directory, configure_run_file = "configure_run.yml"){
  config$run_config$start_datetime <- config$run_config$forecast_start_datetime
  if(config$run_config$forecast_horizon == 0){
    config$run_config$forecast_horizon <- new_horizon
  }
  config$run_config$forecast_start_datetime <- as.character(lubridate::as_datetime(config$run_config$forecast_start_datetime) + lubridate::days(day_advance))
  if(lubridate::hour(config$run_config$forecast_start_datetime) == 0){
    config$run_config$forecast_start_datetime <- paste(config$run_config$forecast_start_datetime, "00:00:00")
  }
  config$run_config$restart_file <- basename(saved_file)
  yaml::write_yaml(config$run_config, file = file.path(lake_directory,"restart",config$location$site_id,config$run_config$sim_name,configure_run_file))
  if(config$run_config$use_s3){
    aws.s3::put_object(file = file.path(lake_directory,"restart",config$location$site_id,config$run_config$sim_name, configure_run_file),
                       object = file.path(config$location$site_id,config$run_config$sim_name, configure_run_file),
                       bucket = "restart",
                       region = Sys.getenv("AWS_DEFAULT_REGION"),
                       use_https = as.logical(Sys.getenv("USE_HTTPS")))
  }
  invisible(config)
}


#' Upload forecast file and metadata to s3 bucket
#'
#' @param saved_file full path of saved FLARE netcdf
#' @param eml_file_name full path of saved FLARE metadata
#' @param config flare configuration object
#'
#' @return
#' @export
#'
put_forecast <- function(saved_file, eml_file_name, config){
  if(config$run_config$use_s3){
    success <- aws.s3::put_object(file = saved_file,
                                  object = file.path(config$location$site_id, basename(saved_file)),
                                  bucket = "forecasts",
                                  region = Sys.getenv("AWS_DEFAULT_REGION"),
                                  use_https = as.logical(Sys.getenv("USE_HTTPS")))
    if(success){
      unlink(saved_file)
    }
    success <- aws.s3::put_object(file = eml_file_name,
                                  object = file.path(config$location$site_id, basename(eml_file_name)),
                                  bucket = "forecasts",
                                  region = Sys.getenv("AWS_DEFAULT_REGION"),
                                  use_https = as.logical(Sys.getenv("USE_HTTPS")))
    if(success){
      unlink(eml_file_name)
    }
  }
}

#' Download file from s3 bucket
#'
#' @param lake_directory full path to repository directory
#' @param bucket name of s3 bucket
#' @param prefix relative path directory within bucket
#'
#' @return
#' @export
#'
download_s3_objects <- function(lake_directory, bucket, prefix){

  files <- aws.s3::get_bucket(bucket = bucket,
                              prefix = prefix,
                              region = Sys.getenv("AWS_DEFAULT_REGION"),
                              use_https = as.logical(Sys.getenv("USE_HTTPS")))
  keys <- vapply(files, `[[`, "", "Key", USE.NAMES = FALSE)
  empty <- grepl("/$", keys)
  keys <- keys[!empty]
  if(length(keys) > 0){
    for(i in 1:length(keys)){
      aws.s3::save_object(object = keys[i],
                          bucket = bucket,
                          file = file.path(lake_directory, bucket, keys[i]),
                          region = Sys.getenv("AWS_DEFAULT_REGION"),
                          use_https = as.logical(Sys.getenv("USE_HTTPS")))
    }
  }
}

#' Delete restart file on s3 bucket
#'
#' @param site four letter code for site
#' @param sim_name name of simulation
#'
#' @return
#' @export
#'
delete_restart <- function(site_id, sim_name){
  files <- aws.s3::get_bucket(bucket = "restart",
                              prefix = file.path(site_id, sim_name),
                              region = Sys.getenv("AWS_DEFAULT_REGION"),
                              use_https = as.logical(Sys.getenv("USE_HTTPS")))
  keys <- vapply(files, `[[`, "", "Key", USE.NAMES = FALSE)
  empty <- grepl("/$", keys)
  keys <- keys[!empty]
  if(length(keys > 0)){
    for(i in 1:length(keys)){
      aws.s3::delete_object(object = keys[i],
                            bucket = "restart",
                            region = Sys.getenv("AWS_DEFAULT_REGION"),
                            use_https = as.logical(Sys.getenv("USE_HTTPS")))
    }
  }
}

#' Set and create directories for observation configuration
#'
#' @param lake_directory full path to repository directory
#'
#' @return
#' @export
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

#' Check if NOAA forecast is on s3 bucket
#'
#' @param lake_directory full path to repository directory
#' @param configuration_file file name (no path) of run configuration file
#'
#' @return
#' @export
#'
check_noaa_present <- function(lake_directory, configure_run_file = "configure_run.yml", config_set_name = "default"){

  config <- set_configuration(configure_run_file,lake_directory, config_set_name = config_set_name)

  noaa_forecast_path <- get_driver_forecast_path(config,
                                                 forecast_model = config$met$forecast_met_model)

  if(config$run_config$forecast_horizon > 0 & !is.null(noaa_forecast_path)){
    noaa_files <- aws.s3::get_bucket(bucket = "drivers",
                                     prefix = noaa_forecast_path,
                                     region = Sys.getenv("AWS_DEFAULT_REGION"),
                                     use_https = as.logical(Sys.getenv("USE_HTTPS")))
    noaa_forecast_path <- file.path(lake_directory,"drivers", noaa_forecast_path)
    keys <- vapply(noaa_files, `[[`, "", "Key", USE.NAMES = FALSE)
    empty <- grepl("/$", keys)
    forecast_files <- keys[!empty]
    noaa_forecasts_ready <- FALSE
  }else{
    forecast_files <- NULL
    noaa_forecasts_ready <- TRUE
  }

  if(length(forecast_files) == 31 | length(forecast_files) == 21){
    noaa_forecasts_ready <- TRUE
  }else{
    if(config$run_config$forecast_horizon > 0){
      message(paste0("waiting for NOAA forecast: ", config$run_config$forecast_start_datetime))
    }
  }
  return(noaa_forecasts_ready)

}

#' Title
#'
#' @param site_id four letter code for site
#' @param sim_name name of simulation
#'
#' @return
#' @export
#'
delete_sim <- function(site_id, sim_name){

  go <- utils::askYesNo(paste0("Do you want to delete the files for ",sim_name," from ",site_id))

  if(go){
    message("deleting analysis files")
    files <- aws.s3::get_bucket(bucket = "analysis",
                                prefix = site_id,
                                region = Sys.getenv("AWS_DEFAULT_REGION"),
                                use_https = as.logical(Sys.getenv("USE_HTTPS")))
    keys <- vapply(files, `[[`, "", "Key", USE.NAMES = FALSE)
    empty <- grepl("/$", keys)
    keys <- keys[!empty]
    keys <- keys[stringr::str_detect(keys, sim_name)]
    if(length(keys > 0)){
      for(i in 1:length(keys)){
        aws.s3::delete_object(object = keys[i],
                              bucket = "analysis",
                              region = Sys.getenv("AWS_DEFAULT_REGION"),
                              use_https = as.logical(Sys.getenv("USE_HTTPS")))
      }
    }

    #forecasts
    message("deleting forecast files")
    files <- aws.s3::get_bucket(bucket = "forecasts",
                                prefix = file.path(site_id),
                                region = Sys.getenv("AWS_DEFAULT_REGION"),
                                use_https = as.logical(Sys.getenv("USE_HTTPS")))
    keys <- vapply(files, `[[`, "", "Key", USE.NAMES = FALSE)
    empty <- grepl("/$", keys)
    keys <- keys[!empty]
    keys <- keys[stringr::str_detect(keys, sim_name)]
    if(length(keys > 0)){
      for(i in 1:length(keys)){
        aws.s3::delete_object(object = keys[i],
                              bucket = "forecasts",
                              region = Sys.getenv("AWS_DEFAULT_REGION"),
                              use_https = as.logical(Sys.getenv("USE_HTTPS")))
      }
    }

    message("deleting restart files")
    files <- aws.s3::get_bucket(bucket = "restart",
                                prefix = file.path(site_id, sim_name),
                                region = Sys.getenv("AWS_DEFAULT_REGION"),
                                use_https = as.logical(Sys.getenv("USE_HTTPS")))
    keys <- vapply(files, `[[`, "", "Key", USE.NAMES = FALSE)
    empty <- grepl("/$", keys)
    keys <- keys[!empty]
    if(length(keys > 0)){
      for(i in 1:length(keys)){
        aws.s3::delete_object(object = keys[i],
                              bucket = "restart",
                              region = Sys.getenv("AWS_DEFAULT_REGION"),
                              use_https = as.logical(Sys.getenv("USE_HTTPS")))
      }
    }
  }
}
