#' @param full_time_local
#' @param working_directory
#' @param input_file_tz
#' @param start_forecast_step
#' @param inflow_file1
#' @param inflow_file2
#' @param chemistry_file
#' @param local_tzone
#' @param met_file_names
#' @param forecast_days
#' @param inflow_process_uncertainty
#' @param future_inflow_flow_coeff
#' @param future_inflow_flow_error
#' @param future_inflow_temp_coeff
#' @param future_inflow_temp_error
#' @param states_config
#' @param include_wq
#' @param use_future_inflow
#' @param doc_scalar
#'
#' @title Create inflow and outflow files in GLM format
#' @return None
#'
#' @export
#'
#' @author Quinn Thomas
#'
#'

create_inflow_outflow_file <- function(full_time_local,
                                       working_directory,
                                       input_file_tz = 'EST5EDT',
                                       start_forecast_step,
                                       inflow_file1,
                                       inflow_file2,
                                       chemistry_file,
                                       local_tzone,
                                       met_file_names,
                                       forecast_days,
                                       inflow_process_uncertainty,
                                       future_inflow_flow_coeff,
                                       future_inflow_flow_error,
                                       future_inflow_temp_coeff,
                                       future_inflow_temp_error,
                                       states_config,
                                       include_wq,
                                       use_future_inflow,
                                       doc_scalar)
{

  full_time_day_local <- lubridate::as_date(full_time_local)

  inflow <- readr::read_csv(inflow_file1, col_types = readr::cols())
  if(!is.na(inflow_file2)){
    wetland <- readr::read_csv(inflow_file2, col_types = readr::cols())
  }

  if(include_wq){
    wq_names_tmp <- states_config$state_names[which(states_config$state_names %in% names(inflow))]
  }else{
    wq_names_tmp <- NULL
  }

  curr_all_days <- NULL

  for(m in 1:length(met_file_names)){
    curr_met_daily <- readr::read_csv(paste0(working_directory,"/",met_file_names[m]),
                               col_types = readr::cols()) %>%
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

  if(config$inflow_process_uncertainty == TRUE){
    inflow_error <- rnorm(nrow(tmp), 0, future_inflow_flow_error)
    temp_error <- rnorm(nrow(tmp), 0, future_inflow_temp_error)
  }else{
    inflow_error <- rep(0.0, nrow(tmp))
    temp_error <- rep(0.0, nrow(tmp))
  }

  for(i in 1:nrow(tmp)){
    if(tmp$forecast[i] == 0 & is.na(tmp$FLOW[i]) & !include_wq){
      tmp[i, c("FLOW", "TEMP",wq_names_tmp)]  <- inflow %>%
        dplyr::filter(time < full_time_day_local[start_forecast_step]) %>%
        dplyr::mutate(doy = lubridate::yday(time)) %>%
        dplyr::filter(doy == lubridate::yday(tmp$time[i])) %>%
        dplyr::summarize_at(.vars = c("FLOW", "TEMP", wq_names_tmp), mean, na.rm = TRUE) %>%
        dplyr::unlist()
    }

    if(tmp$forecast[i] == 1){
      tmp$FLOW[i] = config$future_inflow_flow_coeff[1] +
        config$future_inflow_flow_coeff[2] * tmp$FLOW[i - 1] +
        config$future_inflow_flow_coeff[3] * tmp$Rain_lag1[i] + inflow_error[i]
      tmp$TEMP[i] = config$future_inflow_temp_coeff[1] +
        config$future_inflow_temp_coeff[2] * tmp$TEMP[i-1] +
        config$future_inflow_temp_coeff[3] * tmp$AirTemp_lag1[i] + temp_error[i]
      if(include_wq){
        tmp[i, c(wq_names_tmp)] <- inflow %>%
          dplyr::filter(time < full_time_day_local[start_forecast_step]) %>%
          dplyr::mutate(doy = yday(time)) %>%
          dplyr::filter(doy == yday(tmp$time[i])) %>%
          dplyr::summarize_at(.vars = c(wq_names_tmp), mean, na.rm = TRUE)
      }
    }
  }

  tmp <- tmp %>%
    dplyr::mutate(FLOW = ifelse(FLOW < 0.0, 0.0, FLOW))

  file_name_base <- met_file_names %>%
    stringr::str_sub(4)
  inflow1_file_names <- paste0("inflow", file_name_base)
  outflow_file_names <- paste0("outflow", file_name_base)

  for(i in 1:dplyr::n_distinct(tmp$ensemble)){
    tmp2 <- tmp %>%
      dplyr::filter(ensemble == i) %>%
      dplyr::mutate(SALT = 0.0) %>%
      dplyr::select(time, FLOW, TEMP, SALT, all_of(wq_names_tmp)) %>%
      dplyr::mutate_at(dplyr::vars(c("FLOW", "TEMP", "SALT", all_of(wq_names_tmp))), list(~round(., 4)))

    if("OGM_docr" %in% wq_names_tmp){
    tmp2 <- tmp2 %>%
      dplyr::mutate(OGM_docr = OGM_docr * doc_scalar)
    }
    if("OGM_donr" %in% wq_names_tmp){
      tmp2 <- tmp2 %>%
        dplyr::mutate(OGM_donr = OGM_donr * doc_scalar)
    }
    if("OGM_dopr" %in% wq_names_tmp){
      tmp2 <- tmp2 %>%
        dplyr::mutate(OGM_dopr = OGM_dopr * doc_scalar)
    }

    readr::write_csv(x = tmp2,
              path = paste0(working_directory,"/",inflow1_file_names[i]),
              quote_escape = "none")

    tmp2 <- tmp2 %>%
      dplyr::select(time, FLOW)

    readr::write_csv(x = tmp2,
              path = paste0(working_directory,"/",outflow_file_names[i]),
              quote_escape = "none")
  }



  return(list(inflow_file_names = as.character(inflow1_file_names),
              spillway_file_names = as.character(outflow_file_names),
              wetland_file_names = as.character(inflow1_file_names)))
}
