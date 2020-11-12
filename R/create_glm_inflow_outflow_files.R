#' Title
#'
#' @param inflow_file
#' @param met_file_names
#' @param working_directory
#' @param start_datetime_local
#' @param end_datetime_local
#' @param forecast_start_datetime_local
#' @param local_tzone
#' @param inflow_process_uncertainty
#' @param future_inflow_flow_coeff
#' @param future_inflow_flow_error
#' @param future_inflow_temp_coeff
#' @param future_inflow_temp_error
#' @param use_future_inflow
#'
#' @return
#' @export
#'
#' @examples
create_glm_inflow_outflow_files <- function(inflow_file,
                                            met_file_names,
                                            working_directory,
                                            start_datetime_local,
                                            end_datetime_local,
                                            forecast_start_datetime_local,
                                            local_tzone,
                                            inflow_process_uncertainty,
                                            future_inflow_flow_coeff,
                                            future_inflow_flow_error,
                                            future_inflow_temp_coeff,
                                            future_inflow_temp_error,
                                            use_future_inflow,
                                            state_names = NULL,
                                            specified_files = NULL)

{

  hist_days <- as.numeric(forecast_start_datetime_local - start_datetime_local)
  start_forecast_step <- max(1 + hist_days, 2)
  full_time_local <- seq(start_datetime_local, end_datetime_local, by = "1 day")
  full_time_day_local <- lubridate::as_date(full_time_local)

  inflow <- readr::read_csv(inflow_file, col_types = readr::cols())

  curr_all_days <- NULL

  for(m in 1:length(met_file_names)){
    curr_met_daily <- readr::read_csv(met_file_names[m],
                                      col_types = readr::cols()) %>%
      dplyr::mutate(time = lubridate::with_tz(time, tzone = local_tzone)) %>%
      dplyr::mutate(time = lubridate::as_date(time)) %>%
      dplyr::group_by(time) %>%
      dplyr::summarize(Rain = mean(Rain),
                       AirTemp = mean(AirTemp),.groups = 'drop') %>%
      dplyr::mutate(ensemble = m) %>%
      dplyr::mutate(AirTemp_lag1 = dplyr::lag(AirTemp, 1),
                    Rain_lag1 = dplyr::lag(Rain, 1))

    curr_all_days <- rbind(curr_all_days,curr_met_daily)
  }


  tmp <- dplyr::left_join(curr_all_days, inflow, by = "time")

  forecasts_days <- full_time_day_local[start_forecast_step:length(full_time_day_local)]
  if(use_future_inflow == FALSE || start_forecast_step == length(full_time_day_local)){
    forecasts_days <- NULL
  }

  tmp <- tmp %>%
    dplyr::mutate(forecast = ifelse(time %in% forecasts_days, 1, 0),
                  TEMP = ifelse(forecast == 1, NA, TEMP),
                  FLOW = ifelse(forecast == 1, NA, FLOW))

  if(inflow_process_uncertainty == TRUE){
    inflow_error <- rnorm(nrow(tmp), 0, future_inflow_flow_error)
    temp_error <- rnorm(nrow(tmp), 0, future_inflow_temp_error)
  }else{
    inflow_error <- rep(0.0, nrow(tmp))
    temp_error <- rep(0.0, nrow(tmp))
  }

  for(i in 1:nrow(tmp)){
    if(tmp$forecast[i] == 0 & is.na(tmp$FLOW[i])){
      tmp[i, c("FLOW", "TEMP",wq_names_tmp)]  <- inflow %>%
        dplyr::filter(time < full_time_day_local[start_forecast_step]) %>%
        dplyr::mutate(doy = lubridate::yday(time)) %>%
        dplyr::filter(doy == lubridate::yday(tmp$time[i])) %>%
        dplyr::summarize_at(.vars = c("FLOW", "TEMP"), mean, na.rm = TRUE) %>%
        dplyr::unlist()
    }

    if(tmp$forecast[i] == 1){
      tmp$FLOW[i] = future_inflow_flow_coeff[1] +
        future_inflow_flow_coeff[2] * tmp$FLOW[i - 1] +
        future_inflow_flow_coeff[3] * tmp$Rain_lag1[i] + inflow_error[i]
      tmp$TEMP[i] = future_inflow_temp_coeff[1] +
        future_inflow_temp_coeff[2] * tmp$TEMP[i-1] +
        future_inflow_temp_coeff[3] * tmp$AirTemp_lag1[i] + temp_error[i]
    }
  }

  tmp <- tmp %>%
    dplyr::mutate(FLOW = ifelse(FLOW < 0.0, 0.0, FLOW))

  file_name_base <- basename(met_file_names) %>%
    stringr::str_sub(4)
  inflow_file_names <- file.path(working_directory, paste0("inflow", file_name_base))
  outflow_file_names <- file.path(working_directory, paste0("outflow", file_name_base))

  for(i in 1:dplyr::n_distinct(tmp$ensemble)){
    tmp2 <- tmp %>%
      dplyr::filter(ensemble == i) %>%
      dplyr::mutate(SALT = 0.0) %>%
      dplyr::select(time, FLOW, TEMP, SALT) %>%
      dplyr::mutate_at(dplyr::vars(c("FLOW", "TEMP", "SALT")), list(~round(., 4)))

    readr::write_csv(x = tmp2,
                     path = inflow_file_names[i],
                     quote_escape = "none")

    tmp2 <- tmp2 %>%
      dplyr::select(time, FLOW)

    readr::write_csv(x = tmp2,
                     path = outflow_file_names[i],
                     quote_escape = "none")
  }

  if(!is.null(specified_files)){
    #Reorder state names in the inflow to be the same order as in the states config file
    #sets missing states in the inflow file to zero.

    for(i in 1:length(specified_files)){

      d <- readr::read_csv(specified_files[i], col_types = readr::cols())

      if(length(names(d)) > 2){

        state_names[which(state_names == "temp")] <- "TEMP"

        missing_states <- state_names[which(!(state_names %in% names(d)))]

        for(k in 1:length(missing_states)){
          new_names <- c(names(d), missing_states[k])
          d <- cbind(d, rep(0, nrow(d)))
          names(d) <- new_names
        }

        wq_names <- names(d)[which(!(names(d) %in% c("time", "FLOW", "TEMP", "SALT")))]

        d <- d %>%
          dplyr::select("time", "FLOW", "TEMP", "SALT", all_of(state_names))

        readr::write_csv(d, specified_files[i])
      }
    }
  }



  return(list(inflow_file_name = as.character(inflow_file_names),
              outflow_file_name = as.character(outflow_file_names)))
}
