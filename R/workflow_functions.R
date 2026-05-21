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

  # List-then-get keeps the local and remote paths uniform:
  # flare_get_folder_list enumerates the local restart folder when
  # mode=local and lists S3 otherwise; flare_get_file is a no-op in
  # mode=local since the file is already at local_folder when the
  # listing reports it.
  #   clean_start=TRUE          -> always rewrite from configuration/
  #   mode=local, file present  -> read back
  #   mode=local, file absent   -> write fresh
  #   mode=remote, file present -> download, read back
  #   mode=remote, file absent  -> write fresh

  run_config <- yaml::read_yaml(file.path(lake_directory,"configuration", config_set_name, configure_run_file))

  if(is.na(sim_name)){
    sim_name <- run_config$sim_name
  }

  server_name   <- "restart"
  remote_folder <- file.path(stringr::str_split_fixed(config$s3$restart$bucket, "/", n = 2)[2],
                             config$location$site_id, sim_name)
  remote_file   <- configure_run_file
  local_folder  <- file.path(lake_directory, "restart", config$location$site_id, sim_name)
  local_file    <- configure_run_file
  local_yaml    <- file.path(local_folder, local_file)

  dir.create(local_folder, recursive = TRUE, showWarnings = FALSE)

  restart_exists <- !clean_start && {
    files <- unlist(flare_get_folder_list(
      server_name = server_name,
      prefix      = remote_folder,
      local_path  = local_folder,
      config      = config
    ))
    length(files) > 0 && any(basename(files) == remote_file)
  }

  if (restart_exists) {
    flare_get_file(
      server_name   = server_name,
      remote_folder = remote_folder,
      remote_file   = remote_file,
      local_folder  = local_folder,
      local_file    = local_file,
      config        = config
    )
  } else {
    if (!clean_start) {
      message("run config not found - clean start")
    }
    yaml::write_yaml(run_config, local_yaml)
  }

  invisible(yaml::read_yaml(local_yaml))
}

#' Get data from Github repository
#'
#' @param lake_directory full path to repository directory
#' @param directory the branch name on github
#' @param git_repo https of the github repository
#' @keywords internal
#'
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
put_targets <- function(site_id, cleaned_insitu_file = NA, cleaned_met_file = NA, cleaned_inflow_file = NA, use_s3 = FALSE, config=NULL){

  # use_s3 is retained for signature compatibility; dispatch is handled by
  # flare_put_file via flare_io_mode().

    if(!is.na(cleaned_insitu_file)){

      # aws.s3::put_object(file = cleaned_insitu_file,
      #                    object = file.path(stringr::str_split_fixed(config$s3$targets$bucket, "/", n = 2)[2], site_id, basename(cleaned_insitu_file)),
      #                    bucket = stringr::str_split_fixed(config$s3$targets$bucket, "/", n = 2)[1],
      #                    region = stringr::str_split_fixed(config$s3$targets$endpoint, pattern = "\\.", n = 2)[1],
      #                    base_url = stringr::str_split_fixed(config$s3$targets$endpoint, pattern = "\\.", n = 2)[2],
      #                    use_https = as.logical(Sys.getenv("USE_HTTPS")))

      server_name <- "targets"
      remote_folder <- file.path(stringr::str_split_fixed(config$s3$targets$bucket, "/", n = 2)[2], site_id)
      remote_file <- basename(cleaned_insitu_file)
      local_file <- basename(cleaned_insitu_file)
      local_folder <- dirname(cleaned_insitu_file)


      flare_put_file(server_name = server_name,
                     local_folder = local_folder,
                     local_file = local_file,
                     remote_folder = remote_folder,
                     remote_file = remote_file,
                     config = config)

    }
    if(!is.na(cleaned_inflow_file)){

      server_name <- "targets"
      remote_folder <- file.path(stringr::str_split_fixed(config$s3$targets$bucket, "/", n = 2)[2], site_id)
      remote_file <- basename(cleaned_inflow_file)
      local_file <- basename(cleaned_inflow_file)
      local_folder <- dirname(cleaned_inflow_file)

      flare_put_file(server_name = server_name,
                     local_folder = local_folder,
                     local_file = local_file,
                     remote_folder = remote_folder,
                     remote_file = remote_file,
                     config = config)

      # aws.s3::put_object(file = cleaned_inflow_file,
      #                    object = file.path(stringr::str_split_fixed(config$s3$targets$bucket, "/", n = 2)[2], site_id, basename(cleaned_inflow_file)),
      #                    bucket = stringr::str_split_fixed(config$s3$targets$bucket, "/", n = 2)[1],
      #                    region = stringr::str_split_fixed(config$s3$targets$endpoint, pattern = "\\.", n = 2)[1],
      #                    base_url = stringr::str_split_fixed(config$s3$targets$endpoint, pattern = "\\.", n = 2)[2],
      #                    use_https = as.logical(Sys.getenv("USE_HTTPS")))
    }
    if(!is.na(cleaned_met_file)){

      server_name <- "targets"
      remote_folder <- file.path(stringr::str_split_fixed(config$s3$targets$bucket, "/", n = 2)[2], site_id)
      remote_file <-  basename(cleaned_met_file)
      local_file <- basename(cleaned_met_file)
      local_folder <- dirname(cleaned_met_file)


      flare_put_file(server_name = server_name,
                     local_folder = local_folder,
                     local_file = local_file,
                     remote_folder = remote_folder,
                     remote_file = remote_file,
                     config = config)


      # aws.s3::put_object(file = cleaned_met_file,
      #                    object = file.path(stringr::str_split_fixed(config$s3$targets$bucket, "/", n = 2)[2], site_id, basename(cleaned_met_file)),
      #                    bucket = stringr::str_split_fixed(config$s3$targets$bucket, "/", n = 2)[1],
      #                    region = stringr::str_split_fixed(config$s3$targets$endpoint, pattern = "\\.", n = 2)[1],
      #                    base_url = stringr::str_split_fixed(config$s3$targets$endpoint, pattern = "\\.", n = 2)[2],
      #                    use_https = as.logical(Sys.getenv("USE_HTTPS")))
    }
}

#' Download target data from s3
#'
#' @param lake_directory full path to the repository directory
#' @param config flare configuration object
#' @keywords internal
#'
get_targets <- function(lake_directory, config=NULL){
  # flare_get_folder_list returns character(0) in mode=local, so no
  # explicit use_s3 gate is needed here.
  download_s3_objects(lake_directory,
                      bucket = stringr::str_split_fixed(config$s3$targets$bucket, "/", n = 2)[1],
                      prefix = file.path(stringr::str_split_fixed(config$s3$targets$bucket, "/", n = 2)[2], config$location$site_id),
                      region = stringr::str_split_fixed(config$s3$targets$endpoint, pattern = "\\.", n = 2)[1],
                      base_url = stringr::str_split_fixed(config$s3$targets$endpoint, pattern = "\\.", n = 2)[2], config = config)
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

#' Download restart file from s3 bucket
#'
#' @param config flare configuration object
#' @param lake_directory full path to repository directory
#'
#' @return list of updated configuration values
#' @keywords internal
#'
get_restart_file <- function(config, lake_directory){

  if(!is.null(config$run_config$restart_file) &&
     !is.na(config$run_config$restart_file)){
    restart_file <- basename(config$run_config$restart_file)
    if(config$run_config$use_s3){
      server_name <- "restart"
      remote_folder <- file.path(
        stringr::str_split_fixed(config$s3$restart$bucket, "/", n = 2)[2],
        config$location$site_id, config$run_config$sim_name)
      local_folder <- file.path(lake_directory, "restart",
                                config$location$site_id,
                                config$run_config$sim_name)
      tryCatch({
        flare_get_file(server_name = server_name,
                       remote_folder = remote_folder,
                       remote_file = restart_file,
                       local_folder = local_folder,
                       local_file = restart_file,
                       config = config)
        downloaded_file <- normalizePath(
          file.path(local_folder, restart_file), mustWork = FALSE)
        if(!file.exists(downloaded_file)) {
          stop(paste("Error: File", restart_file,
                     "does not exist in S3 or failed to download."))
        }
      }, error = function(e) {
        stop(paste("Error in fetching restart file:", e$message))
      })
    }
    config$run_config$restart_file <- file.path(
      lake_directory, "restart", config$location$site_id,
      config$run_config$sim_name, restart_file)
    message("Got restart file")
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
#' @param config Configuration object to be updated
#' @param use_https TRUE/FALSE use https when using s3

#' @export
#' @examples

#' dir <- normalizePath(tempdir(),  winslash = "/")
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

#'update_run_config(lake_directory,
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
                              config,
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
  # Preserve use_faasr through the rewritten YAML so flare_io_mode() resolves
  # correctly on the next set_up_simulation.
  run_config$use_faasr <- config$run_config$use_faasr

  file_name <- file.path(lake_directory,"restart",site_id, sim_name, configure_run_file)
  yaml::write_yaml(run_config, file_name)

  local_folder <- dirname(file_name)
  local_file <- basename(file_name)
  remote_folder <- file.path(stringr::str_split_fixed(bucket, "/", n = 2)[2], site_id, sim_name)
  remote_file <- configure_run_file
  server_name <- "restart"

  flare_put_file(server_name = server_name,
                 local_folder = local_folder,
                 local_file = local_file,
                 remote_folder = remote_folder,
                 remote_file = remote_file,
                 config = config)
}

#' Upload restart netcdf file to s3 bucket
#'
#' @param saved_file full path of saved FLARE netcdf
#' @param config flare configuration object
#'
#' @keywords internal
#'
put_restart_file <- function(saved_file, config){

  if(config$run_config$use_s3){

    # success <- aws.s3::put_object(file = saved_file,
    #                               object = file.path(stringr::str_split_fixed(config$s3$restart$bucket, "/", n = 2)[2], config$location$site_id, config$run_config$sim_name, basename(saved_file)),
    #                               bucket = stringr::str_split_fixed(config$s3$restart$bucket, "/", n = 2)[1],
    #                               region = stringr::str_split_fixed(config$s3$restart$endpoint, pattern = "\\.", n = 2)[1],
    #                               base_url = stringr::str_split_fixed(config$s3$restart$endpoint, pattern = "\\.", n = 2)[2],
    #                               use_https = as.logical(Sys.getenv("USE_HTTPS")))
    #
    success <- tryCatch({

      server_name <- "restart"
      remote_folder <- file.path(stringr::str_split_fixed(config$s3$restart$bucket, "/", n = 2)[2], config$location$site_id, config$run_config$sim_name)
      remote_file <- basename(saved_file)
      local_folder <- dirname(saved_file)
      local_file <- basename(saved_file)

      flare_put_file(server_name = server_name,
                     local_folder = local_folder,
                     local_file = local_file,
                     remote_folder = remote_folder,
                     remote_file = remote_file,
                     config = config)
      TRUE
    }, error = function(e) {
      message("Error during file upload: ", e)
      FALSE
    })

    if(success){
      #message("success in putting restart file")
      unlink(saved_file)
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
download_s3_objects <- function(lake_directory, bucket, prefix, region, base_url, config){

  server_name <- "targets"

  # files <- aws.s3::get_bucket(bucket = bucket,
  #                             prefix = prefix,
  #                             region = region,
  #                             base_url = base_url,
  #                             use_https = as.logical(Sys.getenv("USE_HTTPS")))


  files <- unlist(flare_get_folder_list(server_name = server_name, prefix = prefix, config = config))

  if (!is.null(files)) {
    files <- files[!grepl("/$", files)]

    if(length(files) > 0){
      for(i in 1:length(files)){

        filename <- basename(files[i])
        folder_path <- dirname(files[i])
        remote_folder <- folder_path
        remote_file <- filename
        local_folder <- file.path(lake_directory, bucket, folder_path)
        local_file <- filename


        tryCatch({
          flare_get_file(
            server_name = server_name,
            remote_folder = remote_folder,
            remote_file = remote_file,
            local_folder = local_folder,
            local_file = local_file,
            config = config
          )
        }, error = function(e) {
          message("Error occurred while downloading the file: ", e$message)
          return(FALSE)
        }, warning = function(w) {
          message("Warning: ", w$message)
          return(TRUE)
        })
      }
    }
  }


      # aws.s3::save_object(object = keys[i],
      #                     bucket = bucket,
      #                     file = file.path(lake_directory, bucket, keys[i]),
      #                     region = region,
      #                     base_url = base_url,
      #                     use_https = as.logical(Sys.getenv("USE_HTTPS")))
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
delete_restart <- function(site_id, sim_name, bucket = "restart", endpoint,config=NULL){

  server_name <- "restart"
  prefix <- file.path(stringr::str_split_fixed(bucket, "/", n = 2)[2], site_id, sim_name)

  # files <- aws.s3::get_bucket(bucket = stringr::str_split_fixed(bucket, "/", n = 2)[1],
  #                             prefix = file.path(stringr::str_split_fixed(bucket, "/", n = 2)[2], site_id, sim_name),
  #                             region = stringr::str_split_fixed(endpoint, pattern = "\\.", n = 2)[1],
  #                             base_url = stringr::str_split_fixed(endpoint, pattern = "\\.", n = 2)[2],
  #                             use_https = as.logical(Sys.getenv("USE_HTTPS")))

  files <- unlist(flare_get_folder_list(server_name = server_name, prefix = prefix, config = config))


  if (!is.null(files)) {
    keys <- files[!grepl("/$", files)]

    if(length(files) > 0){
      for(i in 1:length(files)){

        filename <- basename(keys[i])
        folder_path <- dirname(keys[i])

        tryCatch({
          flare_delete_file(
            server_name = server_name,
            remote_folder = folder_path,
            remote_file = filename,
            config = config
          )
        }, error = function(e) {
          message("Error occurred while deleting restart file: ", e$message)
          return(FALSE)
        }, warning = function(w) {
          message("Warning: ", w$message)
          return(TRUE)
        })
      }
    }
      # aws.s3::delete_object(object = keys[i],
      #                       bucket = stringr::str_split_fixed(bucket, "/", n = 2)[1],
      #                       region = stringr::str_split_fixed(endpoint, pattern = "\\.", n = 2)[1],
      #                       base_url = stringr::str_split_fixed(endpoint, pattern = "\\.", n = 2)[2],
      #                       use_https = as.logical(Sys.getenv("USE_HTTPS")))
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
#' dir <- normalizePath(tempdir(),  winslash = "/")
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

  config <- set_up_simulation(configure_run_file, lake_directory, config_set_name = config_set_name)

  noaa_forecasts_ready <- TRUE
  if(config$run_config$forecast_horizon > 0){

    met_start_datetime <- lubridate::as_datetime(config$run_config$start_datetime)
    met_forecast_start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime)

    if(config$run_config$forecast_horizon > 16){
      met_forecast_start_datetime <- met_forecast_start_datetime - lubridate::days(config$met$forecast_lag_days)
      if(met_forecast_start_datetime < met_start_datetime){
        met_start_datetime <- met_forecast_start_datetime
        message("horizon is > 16 days so adjusting forecast_start_datetime in the met file generation to use yesterdays forecast. But adjusted forecast_start_datetime < start_datetime")
      }
    }

    reference_date <- lubridate::as_date(met_forecast_start_datetime)# - lubridate::days(1)
    forecast_hour <- lubridate::hour(met_forecast_start_datetime)
    site_id <- config$location$site_id
    forecast_horizon <- config$run_config$forecast_horizon

    prefix <- glue::glue(stringr::str_split_fixed(config$s3$drivers$bucket, "/", n = 2)[2], "/", config$met$future_met_model)
    server_name <- "drivers"
    forecast_dir <- flare_arrow_s3_bucket(server_name = server_name, faasr_prefix = prefix, config = config)

    #forecast_dir <- arrow::s3_bucket(bucket = glue::glue(config$s3$drivers$bucket, "/", config$met$future_met_model),
                                     #endpoint_override =  config$s3$drivers$endpoint, anonymous = TRUE)

    check_date <- function(forecast_dir){

      tryCatch(
        # This is what I want to do...
        {

          forecast_dir$ls()
          return(TRUE)
        },
        # ... but if an error occurs, tell me what happened:
        error=function(error_message) {
          message("NOAA Forecast is not avialable.")
          message("And below is the error message from R:")
          message(error_message)
          return(FALSE)
        })
    }

    noaa_forecasts_ready <- check_date(forecast_dir)

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
    # files <- aws.s3::get_bucket(bucket = stringr::str_split_fixed(config$s3$analysis$bucket, "/", n = 2)[1],
    #                             prefix = file.path(stringr::str_split_fixed(config$s3$analysis$bucket, "/", n = 2)[2], site_id),
    #                             region = stringr::str_split_fixed(config$s3$analysis$endpoint, pattern = "\\.", n = 2)[1],
    #                             base_url = stringr::str_split_fixed(config$s3$analysis$endpoint, pattern = "\\.", n = 2)[2],
    #                             use_https = as.logical(Sys.getenv("USE_HTTPS")))

    server_name <- "analysis"
    prefix <- file.path(stringr::str_split_fixed(config$s3$analysis$bucket, "/", n = 2)[2], site_id)


    files <- flare_get_folder_list(
      server_name = server_name,
      prefix = prefix,
      config = config
    )

    keys <- files[!grepl("/$", files)]
    keys <- keys[stringr::str_detect(keys, sim_name)]
    if(length(keys > 0)){
      for(i in 1:length(keys)){

        filename <- basename(keys[i])
        folder_path <- dirname(keys[i])

        flare_delete_file(
          server_name = server_name,
          remote_folder = folder_path,
          remote_file = filename,
          config = config
        )

       # aws.s3::delete_object(object = keys[i],
                              #bucket = stringr::str_split_fixed(config$s3$analysis$bucket, "/", n = 2)[1],
                              #region = stringr::str_split_fixed(config$s3$analysis$endpoint, pattern = "\\.", n = 2)[1],
                             # base_url = stringr::str_split_fixed(config$s3$analysis$endpoint, pattern = "\\.", n = 2)[2],
                              #use_https = as.logical(Sys.getenv("USE_HTTPS")))
      }
    }

    #forecasts
    message("deleting forecast files")
    #files <- aws.s3::get_bucket(bucket = stringr::str_split_fixed(config$s3$restart$bucket, "/", n = 2)[1],
                               # prefix = file.path(stringr::str_split_fixed(config$s3$restart$bucket, "/", n = 2)[2], site_id),
                               # region = stringr::str_split_fixed(config$s3$restart$endpoint, pattern = "\\.", n = 2)[1],
                                #base_url = stringr::str_split_fixed(config$s3$restart$endpoint, pattern = "\\.", n = 2)[2],
                                #use_https = as.logical(Sys.getenv("USE_HTTPS")))

    server_name <- "restart"
    prefix <- file.path(stringr::str_split_fixed(config$s3$restart$bucket, "/", n = 2)[2], site_id)

    files <- flare_get_folder_list(
      server_name = server_name,
      prefix = prefix,
      config = config
    )
    keys <- files[!grepl("/$", files)]
    keys <- keys[stringr::str_detect(keys, sim_name)]
    if(length(keys > 0)){
      for(i in 1:length(keys)){

        filename <- basename(keys[i])
        folder_path <- dirname(keys[i])

        flare_delete_file(
          server_name = server_name,
          remote_folder = folder_path,
          remote_file = filename,
          config = config
        )


        #aws.s3::delete_object(object = keys[i],
                             # bucket = stringr::str_split_fixed(config$s3$restart$bucket, "/", n = 2)[1],
                             # region = stringr::str_split_fixed(config$s3$restart$endpoint, pattern = "\\.", n = 2)[1],
                             # base_url = stringr::str_split_fixed(config$s3$restart$endpoint, pattern = "\\.", n = 2)[2],
                              #use_https = as.logical(Sys.getenv("USE_HTTPS")))
      }
    }

    message("deleting restart files")
   # files <- aws.s3::get_bucket(bucket = stringr::str_split_fixed(config$s3$restart$bucket, "/", n = 2)[1],
                              #  prefix = file.path(stringr::str_split_fixed(config$s3$restart$bucket, "/", n = 2)[2], site_id, sim_name),
                                #region = stringr::str_split_fixed(config$s3$restart$endpoint, pattern = "\\.", n = 2)[1],
                                #base_url = stringr::str_split_fixed(config$s3$restart$endpoint, pattern = "\\.", n = 2)[2],
                               # use_https = as.logical(Sys.getenv("USE_HTTPS")))

    server_name <- "restart"
    prefix <- file.path(stringr::str_split_fixed(config$s3$restart$bucket, "/", n = 2)[2], site_id,sim_name)

    files <- flare_get_folder_list(
      server_name = server_name,
      prefix = prefix,
      config = config
    )
    keys <- files[!grepl("/$", files)]
    if(length(keys > 0)){
      for(i in 1:length(keys)){

        filename <- basename(keys[i])
        folder_path <- dirname(keys[i])

        flare_delete_file(
          server_name = server_name,
          remote_folder = folder_path,
          remote_file = filename,
          config = config
        )

       # aws.s3::delete_object(object = keys[i],
                             # bucket = stringr::str_split_fixed(config$s3$restart$bucket, "/", n = 2)[1],
                             # region = stringr::str_split_fixed(config$s3$restart$endpoint, pattern = "\\.", n = 2)[1],
                             # base_url = stringr::str_split_fixed(config$s3$restart$endpoint, pattern = "\\.", n = 2)[2],
                             # use_https = as.logical(Sys.getenv("USE_HTTPS")))
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



