#' @title Run FLARE for a single forecast
#'
#' @details Combines functions necessary to do a complete execution of FLARE
#'
#' @param lake_directory full path to repository directory
#' @param configure_run_file flare configuration object
#' @param config_set_name directory within configuration/workflow with run configuration files
#' @param clean_start logical: TRUE = reset run configuration with the file in the configuration directory within repository
#' @param sim_name sim_name to use to run FLARE and find restart files. Deafult = NA (uses the sim_name from run_config)
#' Set the sim_name here when multiple simulations are being run from the same config_set_name (e.g. scenario forecasting)
#' rather than modifying the configuration/configure_run file each time
#'
#' @return the full path to save netcdf file that is used to restart following forecast
#' @export
#' @examplesIf interactive()
#' #Load and install dependencies
#' library(dplyr)
#' library(ggplot2)
#' library(readr)
#' library(lubridate)
#' remotes::install_github("flare-forecast/GLMAEDr")
#' GLMAEDr::glm_install()
#' Sys.setenv('GLM_PATH'='GLMAEDr')
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

run_flare <- function(lake_directory,
                      configure_run_file,
                      config_set_name,
                      clean_start = FALSE,
                      sim_name = NA){

  if(!dir.exists(file.path(lake_directory, "configuration", config_set_name))){
    stop(paste0("lake_directory is missing the configuration/",config_set_name," directory"))
  }

  config <- set_up_simulation(configure_run_file, lake_directory, clean_start = clean_start, config_set_name = config_set_name,
                              sim_name = sim_name)

  config <- get_restart_file(config, lake_directory)

  # `diagnostics_daily` (daily-summary GLM output variables, e.g. inflow/
  # outflow volumes or daily max/min from lake.csv/outlet CSVs) is not
  # supported in this build of FLAREr. The internal plumbing that reads,
  # restarts, and writes these variables (generate_initial_conditions(),
  # run_da_forecast(), write_forecast(), write_restart(), etc.) is left in
  # place and is already gated on `length(diagnostics_daily$names) > 0`, so
  # to restore this feature, delete the next line (and stop stripping the
  # `diagnostics_daily` block from example configs).
  config$output_settings$diagnostics_daily <- NULL

  message(paste0("Running forecast that starts on: ", config$run_config$start_datetime))

  if(!is.null(config$model_settings$par_config_file)){
    if(!is.na(config$model_settings$par_config_file)){
      pars_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$par_config_file), col_types = readr::cols())

      if("par_init" %in% names(pars_config) && !"par_init_mean" %in% names(pars_config)){
        warning("'par_init' in parameter calibration config is deprecated. Please rename this column to 'par_init_mean'.")
        pars_config <- dplyr::rename(pars_config, par_init_mean = par_init)
      }

      required_par_cols <- c("par_names","par_names_save","par_file","par_init_mean","par_init_lowerbound","par_init_upperbound","par_lowerbound","par_upperbound","perturb_par","par_units","fix_par")
      optional_par_cols <- c("par_min_sd", "par_init_sd")
      if(!all(required_par_cols %in% names(pars_config)) ||
         !all(names(pars_config) %in% c(required_par_cols, optional_par_cols))){
        stop(" par configuration file does not have the correct columns")
      }
    }
  }

  message('Retrieving Observational Data...')

  obs_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$obs_config_file), col_types = readr::cols())
  states_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$states_config_file), col_types = readr::cols())

  if(!"temp" %in% states_config$state_names) stop("missing temp as a state name in states config")
  if(!"salt" %in% states_config$state_names) stop("missing salt as a state name in states config")

  # `da_updated` flags which states participate in data assimilation. States with
  # da_updated == 0 are still modeled by GLM and initialized/output by FLAREr, but
  # are excluded from the EnKF state vector and left untouched in the GLM restart
  # (GLM's own restart carries them forward). A missing column defaults to 1 for
  # every state, reproducing the previous behavior (all states assimilated).
  if(!"da_updated" %in% names(states_config)) states_config$da_updated <- 1L
  states_config$da_updated[is.na(states_config$da_updated)] <- 1L
  if(!all(states_config$da_updated %in% c(0L, 1L))){
    stop("`da_updated` column in states config must contain only 0 or 1")
  }
  if(any(states_config$da_updated == 1L) == FALSE){
    stop("at least one state must have da_updated == 1")
  }
  for(req in c("temp")){
    req_row <- which(states_config$state_names == req)
    if(length(req_row) == 1 && states_config$da_updated[req_row] == 0L){
      warning(paste0("state '", req, "' has da_updated == 0; temp are ",
                     "normally assimilated and are always written to the GLM nml"))
    }
  }

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

  if(isTRUE(config$met$use_openmeteo)){
    met_out <- create_met_files_openmet(config, lake_directory, met_forecast_start_datetime, met_start_datetime)
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

  message('Setting states and initial conditions...')

  nml_file_phy <- config$model_settings$base_AED_nml
  if (!is.null(nml_file_phy) && !is.na(nml_file_phy) && any(grepl("^PHY_", states_config$state_names))) {
    message('Using xcc from aed.nml in states_config...')
    states_config <- update_phy_states_obs_mapping(
      states_config,
      nml_path = file.path(config$file_path$configuration_directory, nml_file_phy))
  }

  states_config <- generate_states_to_obs_mapping(states_config, obs_config)

  model_sd <- initiate_model_error(config, states_config)

  # Non-vertical observation assimilation is not part of this build of
  # FLAREr; these stay NULL so the (still-present) downstream plumbing in
  # generate_initial_conditions()/run_da_forecast() takes its already-safe
  # no-op path.
  obs_non_vertical <- NULL
  non_vertical_noise_config <- NULL

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
                                                obs_non_vertical = obs_non_vertical,
                                                non_vertical_noise_config = non_vertical_noise_config)

  rm(init)
  rm(obs)
  gc()

  message("Writing restart")
  saved_file <- write_restart(da_forecast_output = da_forecast_output,
                                              forecast_output_directory = config$file_path$restart_directory,
                                              use_short_filename = TRUE)

  message("writing forecast")


  forecast_df <- write_forecast(da_forecast_output = da_forecast_output,
                                              use_s3 = config$run_config$use_s3,
                                              bucket = config$s3$forecasts_parquet$bucket,
                                              endpoint = config$s3$forecasts_parquet$endpoint,
                                              local_directory = file.path(lake_directory, "forecasts/parquet"),
                                         config)

  rm(da_forecast_output)
  gc()

  if(config$output_settings$generate_plot){
    message("Generating plot")
    targets_df <- read_csv(obs_insitu_file, show_col_types = FALSE)

    targets_df <- obs_config |>
      rename(variable = target_variable) |>
      select(variable, obs_sd) |>
      right_join(targets_df, by = "variable") |>
      mutate(up95 = observation + 1.96 * obs_sd,
             low95 = observation - 1.96 * obs_sd,
             low95 = ifelse(variable != "temperature" & low95 < 0, 0, low95))

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
