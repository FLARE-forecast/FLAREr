#' @title Run FLARE for a single forecast
#'
#' @details Combines functions necessary to do a complete execution of FLARE
#'
#' @param lake_directory full path to repository directory
#' @param configure_run_file flare configuration object
#' @param config_set_name directory within configuration/workflow with run configuration files
#' @param clean_start logical: TRUE = reset run configuration with the file in the configuration directory within repository
#'
#' @return the full path to save netcdf file that is used to restart following forecast
#' @export
#' @examplesIf interactive()
#' #Load and install dependencies
#' library(dplyr)
#' library(ggplot2)
#' library(readr)
#' library(lubridate)
#' remotes::install_github("rqthomas/GLM3r")
#' Sys.setenv('GLM_PATH'='GLM3r')
#'
#' dir <- normalizePath(tempdir(),  winslash = "/")
#' lake_directory <- file.path(dir, "extdata")
#' #Copy files to temporarly directory
#' dir.create(dir,showWarnings = FALSE)
#' file.copy(system.file("extdata", package = "FLAREr"),
#'           tempdir(),
#'           recursive = TRUE)

#' run_flare(lake_directory = lake_directory,
#'           configure_run_file = "configure_run.yml",
#'           config_set_name = "default")

#' open_dataset(file.path(lake_directory,"forecasts/parquet")) |>
#'   filter(variable == "temperature",
#'         depth == 1) |>
#'  collect() |>
#'  ggplot(aes(x = datetime, y = prediction, group = parameter)) +
#'  geom_line() +
#'  geom_vline(aes(xintercept = as_datetime(reference_datetime))) +
#'  labs(title = "1 m water temperature forecast")
#'
#'
run_flare <- function(lake_directory,
                      configure_run_file,
                      config_set_name,
                      clean_start = FALSE){


  if(!dir.exists(file.path(lake_directory, "configuration", config_set_name))){
    stop(paste0("lake_directory is missing the configuration/",config_set_name," directory"))
  }

  config <- set_up_simulation(configure_run_file, lake_directory, clean_start = clean_start, config_set_name = config_set_name)

  config <- get_restart_file(config, lake_directory)

  message(paste0("     Running forecast that starts on: ", config$run_config$start_datetime))

  if(!is.null(config$model_settings$par_config_file)){
    if(!is.na(config$model_settings$par_config_file)){
      pars_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$par_config_file), col_types = readr::cols())
      if(!setequal(names(pars_config),c("par_names","par_names_save","par_file","par_init","par_init_lowerbound","par_init_upperbound","par_lowerbound","par_upperbound","perturb_par","par_units", "fix_par")) &
         !setequal(names(pars_config),c("par_names","par_names_save","par_file","par_init","par_init_lowerbound","par_init_upperbound","par_lowerbound","par_upperbound","inflat_pars", "perturb_par","par_units", "fix_par"))){
        stop(" par configuration file does not have the correct columns")
      }
    }
  }

  message('Retrieving Observational Data...')

  obs_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$obs_config_file), col_types = readr::cols())
  states_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$states_config_file), col_types = readr::cols())

  if(!"temp" %in% states_config$state_names) stop("missing temp as a state name in states config")
  if(!"salt" %in% states_config$state_names) stop("missing salt as a state name in states config")

  if(is.null(config$met$use_openmeteo)) config$met$use_openmeteo <- FALSE

  met_start_datetime <- lubridate::as_datetime(config$run_config$start_datetime)
  met_forecast_start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime)

  if(config$run_config$forecast_horizon > 16 & !config$met$use_openmeteo){
    met_forecast_start_datetime <- met_forecast_start_datetime - lubridate::days(config$met$forecast_lag_days)
    if(met_forecast_start_datetime < met_start_datetime){
      met_start_datetime <- met_forecast_start_datetime
      message("horizon is > 16 days so adjusting forecast_start_datetime in the met file generation to use yesterdays forecast. But adjusted forecast_start_datetime < start_datetime")
    }
  }

  message('Generating Met Forecasts...')

  if(config$met$use_openmeteo){

    message('Using OpenMeteo Met Drivers...')

    met_out <- create_met_files_openmet(out_dir = config$file_path$execute_directory,
                                          start_datetime = met_start_datetime,
                                          end_datetime = config$run_config$end_datetime,
                                          forecast_start_datetime = met_forecast_start_datetime,
                                          forecast_horizon =  config$run_config$forecast_horizon,
                                          latitude = config$location$latitude,
                                          longitude = config$location$longitude,
                                          site_id = config$location$site_id,
                                          openmeteo_api = config$met$openmeteo_api,
                                          model = config$met$openmeteo_model,
                                          use_archive = config$met$use_openmeteo_archive,
                                          bucket = config$s3$drivers$bucket,
                                          endpoint = config$s3$drivers$endpoint)
  }else{

    met_out <- create_met_files(config, lake_directory, met_forecast_start_datetime, met_start_datetime)
  }

  message('Creating inflow/outflow files...')

  inflow_outflow_files <- create_inflow_outflow_files(config, config_set_name, lake_directory)

  obs_insitu_file <- file.path(config$file_path$qaqc_data_directory, config$da_setup$obs_filename)
  if(!file.exists(obs_insitu_file)){
    stop(paste0(file.path(config$file_path$qaqc_data_directory, config$da_setup$obs_filename), " is not found"))
  }

  obs <- create_obs_matrix(cleaned_observations_file_long = obs_insitu_file,
                                   obs_config = obs_config,
                                   config)


  obs_non_vertical <- create_obs_non_vertical(cleaned_observations_file_long = file.path(config$file_path$qaqc_data_directory,paste0(config$location$site_id, "-targets-insitu.csv")),
                                                      obs_config,
                                                      start_datetime = config$run_config$start_datetime,
                                                      end_datetime = config$run_config$end_datetime,
                                                      forecast_start_datetime = config$run_config$forecast_start_datetime,
                                                      forecast_horizon =  config$run_config$forecast_horizon)

  message('Setting states and initial conditions...')

  states_config <- generate_states_to_obs_mapping(states_config, obs_config)

  model_sd <- initiate_model_error(config, states_config)

  init <- generate_initial_conditions(states_config,
                                              obs_config,
                                              pars_config,
                                              obs,
                                              config,
                                              obs_non_vertical)
  #Run EnKF
  da_forecast_output <- run_da_forecast(states_init = init$states,
                                                pars_init = init$pars,
                                                aux_states_init = init$aux_states_init,
                                                obs = obs,
                                                obs_sd = obs_config$obs_sd,
                                                model_sd = model_sd,
                                                working_directory = config$file_path$execute_directory,
                                                met_file_names = met_out$filenames,
                                                inflow_file_names = inflow_outflow_files$inflow_file_names,
                                                outflow_file_names = inflow_outflow_files$outflow_file_names,
                                                config = config,
                                                pars_config = pars_config,
                                                states_config = states_config,
                                                obs_config = obs_config,
                                                da_method = config$da_setup$da_method,
                                                par_fit_method = config$da_setup$par_fit_method,
                                                obs_secchi = obs_non_vertical$obs_secchi,
                                                obs_depth = obs_non_vertical$obs_depth)

  rm(init)
  rm(obs)
  gc()

  message("Writing restart")
  saved_file <- write_restart(da_forecast_output = da_forecast_output,
                                              forecast_output_directory = config$file_path$restart_directory,
                                              use_short_filename = TRUE)

  message("Writing forecast")
  forecast_df <- write_forecast(da_forecast_output = da_forecast_output,
                                              use_s3 = config$run_config$use_s3,
                                              bucket = config$s3$forecasts_parquet$bucket,
                                              endpoint = config$s3$forecasts_parquet$endpoint,
                                              local_directory = file.path(lake_directory, "forecasts/parquet"))

  rm(da_forecast_output)
  gc()

  if(config$output_settings$generate_plot){
    message("Generating plot")
    targets_df <- read_csv(file.path(config$file_path$qaqc_data_directory,paste0(config$location$site_id, "-targets-insitu.csv")), show_col_types = FALSE)
    plotting_general(forecast_df,
                     targets_df,
                     file_name = paste0(tools::file_path_sans_ext(basename(saved_file)),".pdf"),
                     plots_directory = config$file_path$plots_directory)
  }

  rm(forecast_df)
  gc()

  if(config$run_config$use_s3){
    message("Putting restart on S3")
    put_restart_file(saved_file, config = config)
  }

  message(paste0("successfully generated flare forecats for: ", tools::file_path_sans_ext(basename(saved_file))))

  invisible(list(restart_file = saved_file))
}
