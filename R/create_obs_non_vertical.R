#' @title Process observation for variables that don't have depth
#' @details Process observation for variables that don't have depth
#' @param cleaned_observations_file_long string; file name (with full path) of the long-format observation file
#' @param obs_config string; file name (with full path) of the long-format observation file
#' @param start_datetime first datetime of the simulation
#' @param end_datetime last datetime of the simulation
#' @param forecast_start_datetime datetime that a forecast starts
#' @param forecast_horizon number of days forecasted
#' @keywords internal
#'
#' @return list of secchi and depth observations for use in FLAREs
create_obs_non_vertical <- function(cleaned_observations_file_long,
                                    obs_config,
                                    start_datetime,
                                    end_datetime,
                                    forecast_start_datetime,
                                    forecast_horizon){

  start_datetime <- lubridate::as_datetime(start_datetime)
  if(is.na(forecast_start_datetime)){
    end_datetime <- lubridate::as_datetime(end_datetime)
    forecast_start_datetime <- end_datetime
  }else{
    forecast_start_datetime <- lubridate::as_datetime(forecast_start_datetime)
    end_datetime <- forecast_start_datetime + lubridate::days(forecast_horizon)
  }

  if(!("multi_depth" %in% names(obs_config))){
    obs_config <- obs_config |> dplyr::mutate(multi_depth = 1)
  }

  obs_config <- obs_config |>
    dplyr::filter(multi_depth == 0)

  full_time <- seq(start_datetime, end_datetime, by = "1 day")

  if("secchi" %in% unique(obs_config$state_names_obs)){


    obs_secchi <- readr::read_csv(cleaned_observations_file_long, show_col_types = FALSE) |>
      dplyr::filter(variable == "secchi") |>
      dplyr::mutate(date = lubridate::as_date(datetime)) |>
      dplyr::right_join(tibble::tibble(date = lubridate::as_datetime(full_time)), by = "date") |>
      dplyr::mutate(observation = ifelse(date > lubridate::as_date(forecast_start_datetime), NA, observation)) |>
      dplyr::arrange(date) |>
      dplyr::select(observation) |>
      purrr::as_vector()

    obs_secchi <- list(obs = obs_secchi,
                       secchi_sd = obs_config$obs_sd[which(obs_config$state_names_obs == "secchi")])
  }else{
    obs_secchi <- NULL
  }

  if("depth" %in% unique(obs_config$state_names_obs)){
    obs_depth <- readr::read_csv(cleaned_observations_file_long, show_col_types = FALSE) |>
      dplyr::filter(variable == "depth") |>
      dplyr::mutate(date = lubridate::as_date(datetime),
                    hour = hour(datetime)) |>
      dplyr::filter(hour == 0)  |>
      dplyr::right_join(tibble::tibble(date = lubridate::as_datetime(full_time)), by = "date") |>
      dplyr::mutate(observation = ifelse(date > lubridate::as_date(forecast_start_datetime), NA, observation)) |>
      dplyr::arrange(date) |>
      dplyr::select(observation) |>
      purrr::as_vector()

    obs_depth <- list(obs = obs_depth,
                      depth_sd = obs_config$obs_sd[which(obs_config$state_names_obs == "depth")])
  }else{
    obs_depth <- NULL

  }

  return(list(obs_secchi = obs_secchi, obs_depth = obs_depth))
}
