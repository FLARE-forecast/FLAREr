#' @title Generating inflow and outflow files in the GLM format using arrow
#' @details Processes historical model data and future model files into the GLM format
#' @param config configuration file
#' @param config_set_name specific name of configuration within the configuration directory
#' @param lake_directory directory for FLARE application
#' @return list with two vectors. One vector is the matrix of inflow_file_names and the other is the matrix of outflow_file_names
#' @keywords internal
#'
create_inflow_outflow_files  <- function(config, config_set_name, lake_directory) {


  # The variables differ between inflow and outflow
  #variables_in <- unique(c('time',
  #                         'TEMP',
  #                         'SALT',
  #                         'FLOW',
  #                         toupper(readr::read_csv(file.path('configuration',
  #                                                           config_set_name,
  #                                                           config$model_settings$states_config_file),
  #                                                 show_col_types = F)$state_names))) # state variables need to match the inflow


  variables_in <- unique(c('time',
                           'FLOW',
                           'TEMP',
                           'SALT',
                           readr::read_csv(file.path(lake_directory, 'configuration',
                                                     config_set_name,
                                                     config$model_settings$states_config_file),
                                           show_col_types = F)$state_names)) # state variables need to match the inflow

  #Use the upper case TEMP and SALT by excluding the lowercase ones
  variables_in <- variables_in[!(variables_in %in% c("salt", "temp"))]

  variables_out <- c('time', 'FLOW')

  site_id <- config$location$site_id

  # Specify inflow AND outflow
  if (config$flows$include_inflow & config$flows$include_outflow) {

    # Historical flow?
    if (config$run_config$start_datetime == config$run_config$forecast_start_datetime) {
      # the model directories for historical flows (before forecast_start_datetime)
      inflow_historical_dir <- NULL

      outflow_historical_dir <- NULL
    } else {
      # the model directories for historical flows (before forecast_start_datetime)

      inflow_historical_dir <- glue::glue(config$flows$historical_inflow_model)

      outflow_historical_dir <- glue::glue(config$flows$historical_outflow_model)
    }


    # do we need future flow?
    if (config$run_config$forecast_horizon > 0) {

      if (is.null(config$flows$future_inflow_model) | is.null(config$flows$future_outflow_model)) {
        stop("Need future flow model(s) when horizon > 0")
      }

      reference_date <- lubridate::as_date(config$run_config$forecast_start_datetime)

      inflow_forecast_dir <- glue::glue(config$flows$future_inflow_model)

      outflow_forecast_dir <- glue::glue(config$flows$future_outflow_model)
    } else {
      # if no forecast being run set to NULL
      inflow_forecast_dir <- NULL
      outflow_forecast_dir <- NULL
    }


    # flow_forecast_dir = inflow_forecast_dir
    # flow_historical_dir = inflow_historical_dir
    # flow_type = "inflow"
    # variables = variables_in
    # out_dir = config$file_path$execute_directory
    # start_datetime = config$run_config$start_datetime
    # end_datetime = config$run_config$end_datetime
    # forecast_start_datetime = config$run_config$forecast_start_datetime
    # forecast_horizon = config$run_config$forecast_horizon
    # site_id = config$location$site_id
    # use_s3 = config$flows$use_flows_s3
    # bucket = config$s3$inflow_drivers$bucket
    # endpoint = config$s3$inflow_drivers$endpoint
    # local_directory = file.path(lake_directory, config$flows$local_inflow_directory)
    # use_ler_vars = config$flows$use_ler_vars


    # Generate inflow and outflow files
    inflow_outflow_files <- purrr::pmap(list(flow_forecast_dir = list(inflow_forecast_dir, outflow_forecast_dir),
                                             flow_historical_dir = list(inflow_historical_dir, outflow_historical_dir),
                                             flow_type =  list('inflow', 'outflow'),
                                             variables = list(variables_in, variables_out),
                                             out_dir = config$file_path$execute_directory ,
                                             start_datetime = config$run_config$start_datetime,
                                             end_datetime = config$run_config$end_datetime ,
                                             forecast_start_datetime = config$run_config$forecast_start_datetime ,
                                             forecast_horizon = config$run_config$forecast_horizon ,
                                             site_id = config$location$site_id ,
                                             use_s3 = config$flows$use_flows_s3 ,
                                             bucket = list(config$s3$inflow_drivers$bucket , config$s3$outflow_drivers$bucket),
                                             endpoint = list(config$s3$inflow_drivers$endpoint , config$s3$outflow_drivers$endpoint) ,
                                             local_directory = list(file.path(lake_directory, config$flows$local_inflow_directory),
                                                                    file.path(lake_directory, config$flows$local_outflow_directory)),
                                             use_ler_vars = config$flows$use_ler_vars),
                                        create_flow_files) |>
      purrr::set_names('inflow_file_names', 'outflow_file_names')


  } else if (config$flows$include_inflow & !config$flows$include_outflow) { # Specify INFLOW only

    # the model directories for historical flows (before forecast_start_datetime)
    # Historical flow?
    if (config$run_config$start_datetime == config$run_config$forecast_start_datetime) {
      # the model directories for historical flows (before forecast_start_datetime)
      inflow_historical_dir <- NULL

    } else {
      # the model directories for historical flows (before forecast_start_datetime)
      inflow_historical_dir <- glue::glue(config$flows$historical_inflow_model)

    }

    # do we need future flow?
    if (config$run_config$forecast_horizon > 0 ) {

      if (is.null(config$flows$future_inflow_model)) {
        stop("Need future flow model(s) when horizon > 0")
      }

      inflow_forecast_dir <- glue::glue(config$flows$future_inflow_model)

    } else {
      inflow_forecast_dir <- NULL
    }

    # Generate inflow/outflow files
    inflow_outflow_files <- purrr::pmap(list(flow_forecast_dir = list(inflow_forecast_dir, NULL),
                                             flow_historical_dir = list(inflow_historical_dir, NULL),
                                             flow_type =  list('inflow', 'outflow'),
                                             variables = list(variables_in, variables_out),
                                             out_dir = config$file_path$execute_directory ,
                                             start_datetime = config$run_config$start_datetime,
                                             end_datetime = config$run_config$end_datetime ,
                                             forecast_start_datetime = config$run_config$forecast_start_datetime ,
                                             forecast_horizon = config$run_config$forecast_horizon ,
                                             site_id = config$location$site_id ,
                                             use_s3 = config$flows$use_flows_s3 ,
                                             bucket = list(config$s3$inflow_drivers$bucket , config$s3$outflow_drivers$bucket),
                                             endpoint = config$s3$inflow_drivers$endpoint ,
                                             local_directory = list(file.path(lake_directory, config$flows$local_inflow_directory),
                                                                    file.path(lake_directory, config$flows$local_outflow_directory)),
                                             use_ler_vars = config$flows$use_ler_vars),
                                        create_flow_files)  |>
      purrr::set_names('inflow_file_names', 'outflow_file_names')


  } else if (!config$flows$include_inflow & config$flows$include_outflow) { # Specify OUTFLOW only

    # the model directories for historical flows (before forecast_start_datetime)
    # Historical flow?
    if (config$run_config$start_datetime == config$run_config$forecast_start_datetime) {
      # the model directories for historical flows (before forecast_start_datetime)
      outflow_historical_dir <- NULL

    } else {
      # the model directories for historical flows (before forecast_start_datetime)
      outflow_historical_dir <- glue::glue(config$flows$historical_outflow_model)

    }

    # do we need future flow?
    if (config$run_config$forecast_horizon > 0 ) {

      if (is.null(config$flows$future_outflow_model)) {
        stop("Need future flow model(s) when horizon > 0")
      }

      outflow_forecast_dir <- glue::glue(config$flows$future_outflow_model)

    } else {
      outflow_forecast_dir <- NULL
    }

    # Generate inflow/outflow files
    inflow_outflow_files <- purrr::pmap(list(flow_forecast_dir = list(NULL, outflow_forecast_dir),
                                             flow_historical_dir = list(NULL, outflow_historical_dir),
                                             flow_type =  list('inflow', 'outflow'),
                                             variables = list(variables_in, variables_out),
                                             out_dir = config$file_path$execute_directory ,
                                             start_datetime = config$run_config$start_datetime,
                                             end_datetime = config$run_config$end_datetime ,
                                             forecast_start_datetime = config$run_config$forecast_start_datetime ,
                                             forecast_horizon = config$run_config$forecast_horizon ,
                                             site_id = config$location$site_id ,
                                             use_s3 = config$flows$use_flows_s3 ,
                                             bucket = list(config$s3$inflow_drivers$bucket , config$s3$outflow_drivers$bucket),
                                             endpoint = config$s3$inflow_drivers$endpoint ,
                                             local_directory = list(file.path(lake_directory, config$flows$local_inflow_directory),
                                                                    file.path(lake_directory, config$flows$local_outflow_directory)),
                                             use_ler_vars = config$flows$use_ler_vars),
                                        create_flow_files) |>
      purrr::set_names('inflow_file_names', 'outflow_file_names')

  } else if (!config$flows$include_inflow & !config$flows$include_inflow) {  # don't specify inflows or outflows
    inflow_outflow_files <- list()
    inflow_outflow_files$inflow_file_names <- NULL
    inflow_outflow_files$outflow_file_names <- NULL
  }

  return(inflow_outflow_files)
}
