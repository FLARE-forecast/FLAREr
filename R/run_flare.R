#' Run flare workflow (experimental)
#'
#' @param lake_directory
#' @param configure_run_file
#' @param config_set_name
#'
#' @return
#' @export
#'
#' @examples
run_flare <- function(lake_directory,
                      configure_run_file,
                      config_set_name){

  config <- FLAREr::set_configuration(configure_run_file, lake_directory, config_set_name = config_set_name)

  config <- FLAREr::get_restart_file(config, lake_directory)

  message(paste0("     Running forecast that starts on: ", config$run_config$start_datetime))

  #Need to remove the 00 ensemble member because it only goes 16-days in the future
  pars_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$par_config_file), col_types = readr::cols())

  if(!setequal(names(pars_config),c("par_names","par_names_save","par_file","par_init","par_init_lowerbound","par_init_upperbound","par_lowerbound","par_upperbound","inflat_pars","perturb_par","par_units"))){
    stop(paste0(file.path(config$file_path$configuration_directory, config$model_settings$par_config_file), " do not have the following columns: ", c("par_names","par_names_save","par_file","par_init","par_init_lowerbound","par_init_upperbound","par_lowerbound","par_upperbound","inflat_pars","perturb_par","par_units")))
  }

  obs_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$obs_config_file), col_types = readr::cols())
  states_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$states_config_file), col_types = readr::cols())

  if(!"temp" %in% states_config$state_names) stop("missing temp as a state name in states config")
  if(!"salt" %in% states_config$state_names) stop("missing salt as a state name in states config")


  if(!config$met$use_observed_met){
    obs_met_file = NULL
  }else{
    obs_met_file <- file.path(config$file_path$qaqc_data_directory, config$met$observed_met_filename)
    if(!fs::file_exists(obs_met_file)){
      stop(paste0(obs_met_file, " is not found"))
    }
  }

  start_datetime <- lubridate::as_datetime(forecast_start_datetime)
  forecast_start_datetime <- lubridate::as_datetime(forecast_start_datetime)

  if(config$run_config$forecast_horizon > 16 & config$met$use_forecasted_met){
    forecast_start_datetime <- forecast_start_datetime - lubridate::days(1)
    if(forecast_start_datetime < start_datetime){
      start_datetime <- forecast_start_datetime
      message("horizon is > 16 days so adjusting forecast_start_datetime in the met file generation to use yesterdays forecast. But adjusted forecast_start_datetime < start_datetime")
    }
  }

  met_out <- FLAREr::generate_met_files_arrow(obs_met_file = obs_met_file,
                                              out_dir = config$file_path$execute_directory,
                                              start_datetime = start_datetime,
                                              end_datetime = config$run_config$end_datetime,
                                              forecast_start_datetime = forecast_start_datetime,
                                              forecast_horizon =  config$run_config$forecast_horizon,
                                              site_id = config$location$site_id,
                                              use_s3 = TRUE,
                                              bucket = config$s3$drivers$bucket,
                                              endpoint = config$s3$drivers$endpoint,
                                              local_directory = config$met$local_directory,
                                              use_forecast = config$met$use_forecasted_met,
                                              use_ler_vars = config$met$use_ler_vars)

  met_out$filenames <- met_out$filenames[!stringr::str_detect(met_out$filenames, "31")]

  if(config$inflow$include_inflow){
  if(config$run_config$forecast_horizon > 0){
    inflow_forecast_dir = file.path(config$inflow$forecast_inflow_model, lubridate::as_date(config$run_config$forecast_start_datetime))
  }else{
    inflow_forecast_dir <- NULL
  }

  inflow_outflow_files <- FLAREr::create_inflow_outflow_files_arrow(inflow_forecast_dir = inflow_forecast_dir,
                                                                    inflow_obs = config$inflow$observed_filename,
                                                                    variables = variables,
                                                                    out_dir = config$file_path$execute_directory,
                                                                    start_datetime = config$run_config$start_datetime,
                                                                    end_datetime = config$run_config$end_datetime,
                                                                    forecast_start_datetime = config$run_config$forecast_start_datetime,
                                                                    forecast_horizon =  config$run_config$forecast_horizon,
                                                                    site_id = config$location$site_id,
                                                                    use_s3 = config$run_config$use_s3,
                                                                    bucket = config$s3$inflow_drivers$bucket,
                                                                    endpoint = config$s3$inflow_drivers$endpoint,
                                                                    local_directory = file.path(lake_directory, "drivers", inflow_forecast_dir),
                                                                    use_forecast = config$inflow$use_forecasted_inflow,
                                                                    use_ler_vars = config$inflow$use_ler_vars)

  }else{
    inflow_outflow_files <- list()
    inflow_outflow_files$inflow_file_name <- NULL
    inflow_outflow_files$outflow_file_name <- NULL
  }

  obs_insitu_file <- file.path(config$file_path$qaqc_data_directory, config$da_setup$obs_filename)
  if(!fs::file_exists(obs_insitu_file)){
    stop(paste0(obs_insitu_file, " is not found"))
  }

  obs <- FLAREr::create_obs_matrix(cleaned_observations_file_long = obs_insitu_file,
                                   obs_config = obs_config,
                                   config)

  states_config <- FLAREr::generate_states_to_obs_mapping(states_config, obs_config)

  model_sd <- FLAREr::initiate_model_error(config, states_config)

  init <- FLAREr::generate_initial_conditions(states_config,
                                              obs_config,
                                              pars_config,
                                              obs,
                                              config,
                                              historical_met_error = met_out$historical_met_error)
  #Run EnKF
  da_forecast_output <- FLAREr::run_da_forecast(states_init = init$states,
                                                pars_init = init$pars,
                                                aux_states_init = init$aux_states_init,
                                                obs = obs,
                                                obs_sd = obs_config$obs_sd,
                                                model_sd = model_sd,
                                                working_directory = config$file_path$execute_directory,
                                                met_file_names = met_out$filenames,
                                                inflow_file_names = inflow_outflow_files$inflow_file_name,
                                                outflow_file_names = inflow_outflow_files$outflow_file_name,
                                                config = config,
                                                pars_config = pars_config,
                                                states_config = states_config,
                                                obs_config = obs_config,
                                                management = NULL,
                                                da_method = config$da_setup$da_method,
                                                par_fit_method = config$da_setup$par_fit_method,
                                                obs_secchi = NULL,
                                                obs_depth = NULL)

  message("Writing netcdf")
  saved_file <- FLAREr::write_forecast_netcdf(da_forecast_output = da_forecast_output,
                                              forecast_output_directory = config$file_path$forecast_output_directory,
                                              use_short_filename = TRUE)

  message("Writing arrow forecast")
  forecast_df <- FLAREr::write_forecast_arrow(da_forecast_output = da_forecast_output,
                                              use_s3 = config$run_config$use_s3,
                                              bucket = config$s3$forecasts_parquet$bucket,
                                              endpoint = config$s3$forecasts_parquet$endpoint,
                                              local_directory = file.path(lake_directory, "forecasts/parquet"))

  message("Scoring forecasts")
  if(config$output_settings$evaluate_past){
    reference_datetime_format <- "%Y-%m-%d %H:%M:%S"
    past_days <- strftime(lubridate::as_datetime(forecast_df$reference_datetime[1]) - lubridate::days(config$run_config$forecast_horizon), tz = "UTC")

    vars <- FLAREr:::arrow_env_vars()
    s3 <- arrow::s3_bucket(bucket = config$s3$forecasts_parquet$bucket, endpoint_override = config$s3$forecasts_parquet$endpoint)
    past_forecasts <- arrow::open_dataset(s3) |>
      dplyr::filter(model_id == forecast_df$model_id[1],
                    site_id == forecast_df$site_id[1],
                    reference_datetime > past_days) |>
      dplyr::collect()
    FLAREr:::unset_arrow_vars(vars)
  }else{
    past_forecasts <- NULL
  }

  combined_forecasts <- dplyr::bind_rows(forecast_df, past_forecasts)

  FLAREr::generate_forecast_score_arrow(targets_file = obs_insitu_file,
                                        forecast_df = combined_forecasts,
                                        use_s3 = config$run_config$use_s3,
                                        bucket = config$s3$scores$bucket,
                                        endpoint = config$s3$scores$endpoint,
                                        local_directory = file.path(lake_directory, "scores/parquet"),
                                        variable_types = config$output_settings$variables_in_scores)

  #rm(da_forecast_output)
  #gc()
  message("Generating plot")
  FLAREr::plotting_general_2(file_name = saved_file,
                             target_file = obs_insitu_file,
                             ncore = 2,
                             obs_csv = FALSE)

  message("Putting forecast")
  FLAREr::put_forecast(saved_file, eml_file_name = NULL, config)

  #FLAREr::update_run_config(config, lake_directory, configure_run_file, saved_file, new_horizon = 16, day_advance = 1)

  message(paste0("successfully generated flare forecats for: ", basename(saved_file)))

  return(list(restart_file = saved_file))
}
