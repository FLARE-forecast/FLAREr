#' @title Download and Downscale NOAA GEFS for a single site
#' @return None
#'
#' @param site_index, index of site_list, lat_list, lon_list to be downloaded
#' @param lat_list, vector of latitudes that correspond to site codes
#' @param lon_list, vector of longitudes that correspond to site codes
#' @param site_list, vector of site codes, used in directory and file name generation
#' @param downscale, logical specifying whether to downscale from 6-hr to 1-hr
#' @param overwrite, logical stating to overwrite any existing output_file
#' @param model_name, directory name for the 6-hr forecast, this will be used in directory and file name generation
#' @param model_name_ds, directory name for the 1-hr forecast, this will be used in directory and file name generation
#' @param output_directory, directory where the model output will be save
#' @noRd
#'
#' @author Quinn Thomas
#'
#'
#'
create_sss_input_output <- function(x,
                                    i,
                                    m,
                                    full_time,
                                    working_directory,
                                    wq_start,
                                    management_input,
                                    hist_days,
                                    forecast_sss_on,
                                    sss_depth,
                                    use_specified_sss,
                                    state_names,
                                    modeled_depths,
                                    forecast_sss_flow,
                                    forecast_sss_oxy,
                                    salt){

  full_time_day_local <- lubridate::as_date(full_time)

  sss_oxy_factor <- 1.0

  depth_index <- which.min(abs(modeled_depths - sss_depth))

  time_sss <- c(full_time_day_local[i - 1],full_time_day_local[i])

  oxy <- x[wq_start[which(state_names == "OXY_oxy")] + depth_index - 1]
  temp<- x[wq_start[which(state_names == "temp")] + depth_index - 1]
  salt <- salt[depth_index]

  if(i > (hist_days + 1)){
    if(forecast_sss_on){
      if(use_specified_sss){
        FLOW1 <- management_input[i-1, 1]
        OXY1 <- oxy + management_input[i-1, 2]  * sss_oxy_factor
      }else{
        FLOW1 <- forecast_SSS_flow * (1/(60*60*24))
        OXY1 <- forecast_SSS_Oxy * sss_oxy_factor
      }
    }else{
      FLOW1 <- 0.0
      OXY1 <-  0.0
    }
  }else{
    FLOW1 <- management_input[i-1, 1]
    OXY1 <-  oxy + management_input[i-1, 2]  * sss_oxy_factor
  }

  if(i > (hist_days + 1)){
    if(forecast_sss_on){
      if(use_specified_sss){
        FLOW2 <- management_input[i, 1]
        OXY2 <- oxy + management_input[i, 2]  * sss_oxy_factor
      }else{
        FLOW2 <- forecast_SSS_flow * (1/(60*60*24))
        OXY2 <- oxy+ forecast_SSS_Oxy * sss_oxy_factor
      }
    }else{
      FLOW2 <- 0.0
      OXY2 <- 0.0
    }
  }else{
    FLOW2 <- management_input[i, 1]
    OXY2 <- oxy + management_input[i, 2]  * sss_oxy_factor
  }

  FLOW <- round(c(FLOW1, FLOW2), 5)
  TEMP <- round(rep(temp,2), 3)
  OXY_oxy <- round(c(OXY1, OXY2), 3)
  SALT <- round(rep(salt,2), 3)


  sss_inflow <- data.frame(time = time_sss, FLOW = FLOW, TEMP = TEMP, SALT = SALT)

  non_temp_states <- state_names[which(!(state_names %in% c("temp", "salt")))]

  for(i in 1:length(non_temp_states)){
    if(non_temp_states[i] == "OXY_oxy"){
      sss_inflow  <- cbind(sss_inflow, OXY_oxy)
    }else{
      sss_inflow <- cbind(sss_inflow, round(rep(x[wq_start[which(state_names == non_temp_states[i])] + depth_index - 1],2), 3))
    }
  }

  names(sss_inflow) <- c("time", "FLOW", "TEMP", "SALT", non_temp_states)

  sss_outflow <- data.frame(time = time_sss, FLOW = FLOW)

  write.csv(sss_inflow, paste0(working_directory, "/sss_inflow.csv"), row.names = FALSE, quote = FALSE)
  write.csv(sss_outflow, paste0(working_directory, "/sss_outflow.csv"), row.names = FALSE, quote = FALSE)
}
