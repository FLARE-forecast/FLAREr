##' @title Create matrix of observations in the format required by run_da_forecast
##' @details Creates a matrix of observations that maps the modeled states to the observed states. The function uses information from columns the obs_config file.
##' @param cleaned_observations_file_long string; file name (with full path) of the long-format observation file
##' @param obs_config list; observations configuration list
##' @param config list; flare configuration list
##' @return matrix that is based to generate_initial_conditions() and run_da_forecast()
##' @import readr
##' @importFrom lubridate as_datetime as_date hour days
##' @importFrom dplyr filter
##' @author Quinn Thomas
##' @keywords internal
create_obs_matrix <- function(cleaned_observations_file_long,
                              obs_config,
                              config){

  start_datetime <- lubridate::as_datetime(config$run_config$start_datetime)
  if(is.na(config$run_config$forecast_start_datetime)){
    end_datetime <- lubridate::as_datetime(config$run_config$end_datetime)
    forecast_start_datetime <- end_datetime
  }else{
    forecast_start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime)
    end_datetime <- forecast_start_datetime + lubridate::days(config$run_config$forecast_horizon)
  }

  full_time <- seq(start_datetime, end_datetime, by = "1 day")


  if(!is.null(cleaned_observations_file_long)){
    d <- readr::read_csv(cleaned_observations_file_long, show_col_types = FALSE, guess_max = 1000000)

     if(!("multi_depth" %in% names(obs_config))){
       obs_config <- obs_config |> dplyr::mutate(multi_depth = 1)
     }

    obs_config <- obs_config |>
      dplyr::filter(multi_depth == 1)

     df <- as_tibble(expand.grid(variable = obs_config$target_variable, datetime = full_time, depth = config$model_settings$modeled_depths)) |>
      dplyr::mutate(variable = as.character(variable),
             datetime = lubridate::force_tz(datetime, "UTC")) |>
      dplyr::left_join(d, by = dplyr::join_by(variable, datetime, depth))


    obs <- array(df$observation,dim = c(length(obs_config$target_variable), length(full_time), length(config$model_settings$modeled_depths)))

    full_time_forecast <- seq(start_datetime, end_datetime, by = "1 day")
    obs[ , which(full_time_forecast > forecast_start_datetime), ] <- NA
  }else{
    obs <- array(NA, dim = c(1, length(full_time), length(config$model_settings$modeled_depths)))
  }

  return(obs)
}
