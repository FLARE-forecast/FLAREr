#' Create filenames used in netcdf output
#'
#' @param full_time vector of datestimes that are modeled
#' @param hist_days number of days simulated before a forecast begins
#'
#' @noRd
#' @return list of filenames used in the netcdf generation
create_filenames <- function(full_time, hist_days, forecast_days, config) {

  pad_zero <- function(x) formatC(x, width = 2, flag = "0")

  file_name_h_day   <- pad_zero(lubridate::day(full_time[1]))
  file_name_h_month <- pad_zero(lubridate::month(full_time[1]))

  forecast_start      <- full_time[hist_days + 1]
  file_name_f_day     <- pad_zero(lubridate::day(forecast_start))
  file_name_f_month   <- pad_zero(lubridate::month(forecast_start))

  time_of_forecast <- Sys.time()
  forecast_iteration_id <- paste0(
    lubridate::year(time_of_forecast),
    pad_zero(lubridate::month(time_of_forecast)),
    pad_zero(lubridate::day(time_of_forecast)),
    "T",
    pad_zero(lubridate::hour(time_of_forecast)),
    pad_zero(lubridate::minute(time_of_forecast)),
    pad_zero(round(lubridate::second(time_of_forecast), 0))
  )

  save_file_name <- paste0(
    config$run_config$sim_name, "_H_",
    lubridate::year(full_time[1]), "_",
    file_name_h_month, "_",
    file_name_h_day, "_",
    lubridate::year(forecast_start), "_",
    file_name_f_month, "_",
    file_name_f_day, "_F_",
    forecast_days, "_",
    forecast_iteration_id
  )

  if (length(full_time) >= hist_days + 1) {
    save_file_name_short <- paste0(
      config$location$site_id, "-",
      lubridate::year(forecast_start), "-",
      file_name_f_month, "-",
      file_name_f_day, "-",
      config$run_config$sim_name
    )
  } else {
    save_file_name_short <- paste0(
      config$location$site_id, "-",
      lubridate::year(forecast_start), "-",
      file_name_f_month, "-",
      file_name_f_day, "-",
      config$run_config$sim_name, "_spinup"
    )
  }

  list(
    save_file_name        = save_file_name,
    save_file_name_short  = save_file_name_short,
    forecast_iteration_id = forecast_iteration_id,
    time_of_forecast      = time_of_forecast
  )
}
