#' Get run configuration from s3 bucket
#'
#' @param configure_run_file
#' @param lake_directory
#' @param config
#' @param clean_start
#'
#' @return
#' @export
#'
#' @examples
get_run_config <- function(configure_run_file, lake_directory, config, clean_start){

  if(clean_start | !config$run_config$use_s3){
    restart_exists <- file.path(lake_directory, "restart", config$location$site_id, config$run_config$sim_name, configure_run_file)
    if(!restart_exists){
      file.copy(file.path(lake_directory,"configuration","FLAREr",configure_run_file), file.path(lake_directory, "restart", config$location$site_id, config$run_config$sim_name, configure_run_file))
    }
  }else if(config$run_config$use_s3){
    restart_exists <- aws.s3::object_exists(object = file.path(config$location$site_id, config$run_config$sim_name, configure_run_file), bucket = "restart")
    if(restart_exists){
      aws.s3::save_object(object = file.path(config$location$site_id, config$run_config$sim_name, configure_run_file), bucket = "restart", file = file.path(lake_directory, "restart", config$location$site_id, config$run_config$sim_name, configure_run_file))
    }else{
      file.copy(file.path(lake_directory,"configuration","FLAREr",configure_run_file), file.path(lake_directory, "restart", config$location$site_id, config$run_config$sim_name, configure_run_file))
    }
  }
  run_config <- yaml::read_yaml(file.path(lake_directory, "restart", config$location$site_id, config$run_config$sim_name, configure_run_file))
  return(run_config)
}

#' Get data from Github repository
#'
#' @param lake_directory
#' @param directory
#' @param git_repo
#'
#' @return
#' @export
#'
#' @examples
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
#' @param edi_https
#' @param file
#' @param lake_directory
#'
#' @return
#' @export
#'
#' @examples
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
#' @param config
#' @param cleaned_insitu_file
#' @param cleaned_met_file
#' @param cleaned_inflow_file
#'
#' @return
#' @export
#'
#' @examples
put_targets <- function(site_id, cleaned_insitu_file = NA, cleaned_met_file = NA, cleaned_inflow_file = NA, use_s3){

  if(use_s3){
    if(!is.na(cleaned_insitu_file)){
      aws.s3::put_object(file = cleaned_insitu_file, object = file.path(site_id, basename(cleaned_insitu_file)), bucket = "targets")
    }
    if(!is.na(cleaned_inflow_file)){
      aws.s3::put_object(file = cleaned_inflow_file, object = file.path(site_id, basename(cleaned_inflow_file)), bucket = "targets")
    }
    if(!is.na(cleaned_met_file)){
      aws.s3::put_object(file = cleaned_met_file, object = file.path(site_id, basename(cleaned_met_file)), bucket = "targets")
    }
  }
}

#' Download target data from s3
#'
#' @param lake_directory
#' @param config
#'
#' @return
#' @export
#'
#' @examples
get_targets <- function(lake_directory, config){
  if(config$run_config$use_s3){
    download_s3_objects(lake_directory, bucket = "targets", prefix = config$location$site_id)
  }
}


#' Download stacked NOAA data from s3 bucket
#'
#' @param lake_directory
#' @param config
#' @param averaged
#'
#' @return
#' @export
#'
#' @examples
get_stacked_noaa <- function(lake_directory, config, averaged = TRUE){
  if(config$run_config$use_s3){
    if(averaged){
      download_s3_objects(lake_directory, bucket = "drivers", prefix = file.path("noaa/NOAAGEFS_1hr_stacked_average/",config$location$site_id))
    }else{
      download_s3_objects(lake_directory, bucket = "drivers", prefix = file.path("noaa/NOAAGEFS_1hr_stacked/",config$location$site_id))
    }
  }
}

#' Get file path for driver forecasts
#'
#' @param config
#' @param forecast_model
#'
#' @return
#' @export
#'
#' @examples
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
#' @param lake_directory
#' @param forecast_path
#'
#' @return
#' @export
#'
#' @examples
get_driver_forecast <- function(lake_directory, forecast_path){

  download_s3_objects(lake_directory,
                      bucket = "drivers",
                      prefix = forecast_path)
}

#' Set and create directories in the configuration file
#'
#' @param configure_run_file
#' @param lake_directory
#' @param clean_start
#'
#' @return
#' @export
#'
#' @examples
set_configuration <- function(configure_run_file, lake_directory, clean_start = FALSE){
  run_config <- yaml::read_yaml(file.path(lake_directory,"configuration","FLAREr",configure_run_file))
  config <- yaml::read_yaml(file.path(lake_directory,"configuration","FLAREr",run_config$configure_flare))
  config$run_config <- run_config
  config$file_path$qaqc_data_directory <- file.path(lake_directory, "targets")
  config$file_path$data_directory <- file.path(lake_directory, "data_raw")
  config$file_path$noaa_directory <- file.path(lake_directory, "drivers")
  config$file_path$configuration_directory <- file.path(lake_directory, "configuration")
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

  run_config <- get_run_config(configure_run_file, lake_directory, config, clean_start = clean_start)
  config$run_config <- run_config

  return(config)
}

#' Download restart file from s3 bucket
#'
#' @param config
#' @param lake_directory
#'
#' @return
#' @export
#'
#' @examples
get_restart_file <- function(config, lake_directory){
  if(!is.na(config$run_config$restart_file)){
    restart_file <- basename(config$run_config$restart_file)
    if(config$run_config$use_s3){
      aws.s3::save_object(object = file.path(config$location$site_id, restart_file),
                          bucket = "forecasts",
                          file = file.path(lake_directory, "forecasts", restart_file))
    }
    config$run_config$restart_file <- file.path(lake_directory, "forecasts", restart_file)
  }
  return(config)
}

#' Update run configuration and upload to s3 bucket
#'
#' @param config
#' @param lake_directory
#' @param configure_run_file
#' @param saved_file
#' @param new_horizon
#' @param day_advance
#'
#' @return
#' @export
#'
#' @examples
update_run_config <- function(config, lake_directory, configure_run_file, saved_file, new_horizon, day_advance = 1){
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
    aws.s3::put_object(file = file.path(lake_directory,"restart",config$location$site_id,config$run_config$sim_name, configure_run_file), object = file.path(config$location$site_id,config$run_config$sim_name, configure_run_file), bucket = "restart")
  }
}

#' Upload forecast file and metadata to s3 bucket
#'
#' @param saved_file
#' @param eml_file_name
#' @param config
#'
#' @return
#' @export
#'
#' @examples
put_forecast <- function(saved_file, eml_file_name, config){
  if(config$run_config$use_s3){
    success <- aws.s3::put_object(file = saved_file, object = file.path(config$location$site_id, basename(saved_file)), bucket = "forecasts")
    if(success){
      unlink(saved_file)
    }
    success <- aws.s3::put_object(file = eml_file_name, object = file.path(config$location$site_id, basename(eml_file_name)), bucket = "forecasts")
    if(success){
      unlink(eml_file_name)
    }
  }
}

#' Download file from s3 bucket
#'
#' @param lake_directory
#' @param bucket
#' @param prefix
#'
#' @return
#' @export
#'
#' @examples
download_s3_objects <- function(lake_directory, bucket, prefix){

  files <- aws.s3::get_bucket(bucket = bucket, prefix = prefix)
  keys <- vapply(files, `[[`, "", "Key", USE.NAMES = FALSE)
  empty <- grepl("/$", keys)
  keys <- keys[!empty]
  if(length(keys) > 0){
    for(i in 1:length(keys)){
      aws.s3::save_object(object = keys[i],bucket = bucket, file = file.path(lake_directory, bucket, keys[i]))
    }
  }
}

#' Delete restart file on s3 bucket
#'
#' @param site
#' @param sim_name
#'
#' @return
#' @export
#'
#' @examples
delete_restart <- function(site, sim_name){
  files <- aws.s3::get_bucket(bucket = "restart", prefix = file.path(site, sim_name))
  keys <- vapply(files, `[[`, "", "Key", USE.NAMES = FALSE)
  empty <- grepl("/$", keys)
  keys <- keys[!empty]
  if(length(keys > 0)){
    for(i in 1:length(keys)){
      aws.s3::delete_object(object = keys[i], bucket = "restart")
    }
  }
}

#' Set and create directories for observation configuration
#'
#' @param lake_directory
#'
#' @return
#' @export
#'
#' @examples
initialize_obs_processing <- function(lake_directory, observation_yml = NA){

  curr_dir1 <- file.path(lake_directory, "data_raw")
  if(!dir.exists(curr_dir1)){
    dir.create(curr_dir1, recursive = TRUE)
  }
  curr_dir2 <- file.path(lake_directory, "targets")
  if(!dir.exists(curr_dir2)){
    dir.create(curr_dir2, recursive = TRUE)
  }

  if(!is.na(observation_yml)){
    config_obs <- yaml::read_yaml(file.path(lake_directory,"configuration","observation_processing", observation_yml))
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
#' @param lake_directory
#' @param s3_mode
#' @param forecast_site
#' @param configuration_file
#'
#' @return
#' @export
#'
#' @examples
check_noaa_present <- function(lake_directory, configure_run_file){

  config <- set_configuration(configure_run_file,lake_directory)

  noaa_forecast_path <- get_driver_forecast_path(config,
                                                 forecast_model = config$met$forecast_met_model)

  if(config$run_config$forecast_horizon > 0 & !is.null(noaa_forecast_path)){
    noaa_files = aws.s3::get_bucket(bucket = "drivers", prefix = noaa_forecast_path)
    noaa_forecast_path <- file.path(lake_directory,"drivers", noaa_forecast_path)
    keys <- vapply(noaa_files, `[[`, "", "Key", USE.NAMES = FALSE)
    empty <- grepl("/$", keys)
    forecast_files <- keys[!empty]
    noaa_forecasts_ready <- FALSE
  }else{
    forecast_files <- NULL
    noaa_forecasts_ready <- TRUE
  }

  if(length(forecast_files) == 31){
    noaa_forecasts_ready <- TRUE
  }else{
    if(config$run_config$forecast_horizon > 0){
      message(paste0("waiting for NOAA forecast: ", config$run_config$forecast_start_datetime))
    }
  }
  return(noaa_forecasts_ready)

}





