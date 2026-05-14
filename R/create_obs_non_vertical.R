#' @title Process observations for variables that don't have depth
#' @details Reads each non-vertical observation variable (multi_depth == 0 in
#'   obs_config) from the cleaned observations file and aligns it to the
#'   simulation time grid.  Returns a named list keyed by state_names_obs; each
#'   element carries the time-aligned observation vector, its uncertainty SD,
#'   and the metadata needed to connect it to the model (model_source,
#'   model_variable, model_depth_m).
#' @param cleaned_observations_file_long string; file name (with full path) of
#'   the long-format observation file
#' @param obs_config data frame; observation configuration (must include
#'   state_names_obs, obs_sd, multi_depth, model_source, model_variable,
#'   model_depth_m)
#' @param start_datetime first datetime of the simulation
#' @param end_datetime last datetime of the simulation
#' @param forecast_start_datetime datetime that a forecast starts
#' @param forecast_horizon number of days forecasted
#' @keywords internal
#'
#' @return named list; one element per non-vertical variable (keyed by
#'   state_names_obs).  Each element is a list with:
#'   \describe{
#'     \item{obs}{numeric vector aligned to the daily simulation grid}
#'     \item{sd}{observation uncertainty standard deviation}
#'     \item{model_source}{"state" or "diagnostic"}
#'     \item{model_variable}{name of the corresponding model variable}
#'     \item{model_depth_m}{depth in metres for extraction, NA, or "bottom"}
#'   }
create_obs_non_vertical <- function(cleaned_observations_file_long,
                                    obs_config,
                                    start_datetime,
                                    end_datetime,
                                    forecast_start_datetime,
                                    forecast_horizon) {

  start_datetime <- lubridate::as_datetime(start_datetime)
  if (is.na(forecast_start_datetime)) {
    end_datetime <- lubridate::as_datetime(end_datetime)
    forecast_start_datetime <- end_datetime
  } else {
    forecast_start_datetime <- lubridate::as_datetime(forecast_start_datetime)
    end_datetime <- forecast_start_datetime + lubridate::days(forecast_horizon)
  }

  if (!("multi_depth" %in% names(obs_config))) {
    obs_config <- obs_config |> dplyr::mutate(multi_depth = 1)
  }

  # Ensure new columns exist with NA defaults for configs that predate them
  for (col in c("model_source", "model_variable", "model_depth_m")) {
    if (!(col %in% names(obs_config))) {
      obs_config[[col]] <- NA
    }
  }

  non_vertical_config <- obs_config |> dplyr::filter(multi_depth == 0)

  if (nrow(non_vertical_config) == 0) return(list())

  full_time <- seq(start_datetime, end_datetime, by = "1 day")
  time_tbl  <- tibble::tibble(date = lubridate::as_date(full_time))

  obs_long <- readr::read_csv(cleaned_observations_file_long, show_col_types = FALSE)

  obs_non_vertical <- list()

  for (i in seq_len(nrow(non_vertical_config))) {
    var_name <- trimws(non_vertical_config$state_names_obs[i])

    obs_vec <- obs_long |>
      dplyr::filter(variable == var_name) |>
      dplyr::mutate(date = lubridate::as_date(datetime)) |>
      dplyr::right_join(time_tbl, by = "date") |>
      dplyr::mutate(observation = ifelse(
        date > lubridate::as_date(forecast_start_datetime), NA, observation
      )) |>
      dplyr::arrange(date) |>
      dplyr::pull(observation)

    obs_non_vertical[[var_name]] <- list(
      obs           = obs_vec,
      sd            = non_vertical_config$obs_sd[i],
      model_source  = non_vertical_config$model_source[i],
      model_variable = non_vertical_config$model_variable[i],
      model_depth_m  = non_vertical_config$model_depth_m[i]
    )
  }

  obs_non_vertical
}
