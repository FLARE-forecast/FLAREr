
#' Generate management list required to simulate oxygen inputs
#'
#' @param config
#' @param start_datetime
#' @param end_datetime
#'
#' @return
#' @export
#'
#' @examples
generate_oxygen_management <- function(config){



  d <- readr::read_csv(file.path(config$data_location, config$sss_fname), col_type = readr::cols(
    time = readr::col_date(format = ""),
    FLOW = readr::col_double(),
    OXY_oxy = readr::col_double())
  )

  start_datetime <- lubridate::as_datetime(config$run_config$start_datetime)
  if(is.na(config$run_config$forecast_start_datetime)){
    end_datetime <- lubridate::as_datetime(config$run_config$end_datetime)
    forecast_start_datetime <- end_datetime
  }else{
    forecast_start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime)
    end_datetime <- forecast_start_datetime + lubridate::days(config$run_config$forecast_horizon)
  }

  full_time_day <- seq(lubridate::as_date(start_datetime), lubridate::as_date(end_datetime), by = "1 day")

  sss_flow <- rep(0, length(full_time_day))
  sss_OXY_oxy <- rep(0, length(full_time_day))

  if(length(which(d$time == full_time_day[1])) > 0){

    for(i in 1:(length(full_time_day))){
      index <- which(d$time == full_time_day[i])
      if(length(index) > 0){
        sss_flow[i] <- unlist(d[index, "FLOW"])
        sss_OXY_oxy[i] <- unlist(d[index, "OXY_oxy"])
      }
    }
  }
  management_input <- data.frame(sss_flow = sss_flow, sss_OXY_oxy = sss_OXY_oxy)

  management <- list()
  management$management_input <- management_input
  management$simulate_sss <- config$simulate_sss
  management$forecast_sss_on <- config$run_config$forecast_sss_on
  management$sss_depth <- config$sss_depth
  management$use_specified_sss <- config$use_specified_sss
  management$specified_sss_inflow_file <- config$specified_sss_inflow_file
  management$specified_sss_outflow_file <- config$specified_sss_outflow_file
  management$forecast_sss_flow <- config$forecast_sss_flow
  management$forecast_sss_oxy <- config$forecast_sss_oxy

  return(management)
}
