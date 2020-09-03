##' @title Download and Downscale NOAA GEFS for a single site
##' @return None
##'
##' @param site_index, index of site_list, lat_list, lon_list to be downloaded
##' @param lat_list, vector of latitudes that correspond to site codes
##' @param lon_list, vector of longitudes that correspond to site codes
##' @param site_list, vector of site codes, used in directory and file name generation
##' @param downscale, logical specifying whether to downscale from 6-hr to 1-hr
##' @param overwrite, logical stating to overwrite any existing output_file
##' @param model_name, directory name for the 6-hr forecast, this will be used in directory and file name generation
##' @param model_name_ds, directory name for the 1-hr forecast, this will be used in directory and file name generation
##' @param output_directory, directory where the model output will be save
##' @export
##'
##' @author Quinn Thomas
##'
##'
##'
create_sss_input_output <- function(x, i, m, full_time_local, 
                                    working_directory, wq_start, 
                                    management_input, hist_days, 
                                    forecast_sss_on,
                                    sss_depth,
                                    use_specified_sss,
                                    states_config,
                                    include_wq){
  
  full_time_day_local <- lubridate::as_date(full_time_local)
  
  potential_names <- c("OXY_oxy",
                       "SIL_rsi",
                       "NIT_amm",
                       "NIT_nit",
                       "PHS_frp",
                       "OGM_doc",
                       "OGM_docr",
                       "OGM_poc",
                       "OGM_don",
                       "OGM_donr",
                       "OGM_pon",
                       "OGM_dop",
                       "OGM_dopr",
                       "OGM_pop")

  
  sss_oxy_factor <- 1.0
  
  depth_index <- which.min(abs(modeled_depths - sss_depth))
  
  if(include_wq){
    wq_names_tmp <- states_config$state_names[which(states_config$state_names %in% potential_names)]
  }else{
    wq_names_tmp <- NULL
  }
  
  time_sss <- c(full_time_day_local[i - 1],full_time_day_local[i])
  if(i > (hist_days + 1)){
    if(forecast_sss_on){
      if(use_specified_sss){
        FLOW1 <- management_input[i-1, 1]
        OXY1 <- management_input[i-1, 2]  * sss_oxy_factor
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
    OXY1 <-  management_input[i-1, 2]  * sss_oxy_factor
  }
  
  if(i > (hist_days + 1)){
    if(forecast_sss_on){
      if(use_specified_sss){
        FLOW2 <- management_input[i, 1]
        OXY2 <- management_input[i, 2]  * sss_oxy_factor
      }else{
        FLOW2 <- forecast_SSS_flow * (1/(60*60*24))
        OXY2 <- forecast_SSS_Oxy * sss_oxy_factor
      }
    }else{
      FLOW2 <- 0.0
      OXY2 <- 0.0
    }
  }else{
    FLOW2 <- management_input[i, 1]
    OXY2 <- management_input[i, 2]  * sss_oxy_factor    
  }
  
  FLOW <- round(c(FLOW1, FLOW2), 5)
  TEMP <- round(rep(x[depth_index],2), 3)
  SALT <- rep(0,2)
  
  #OXY_EQ <- Eq.Ox.conc(TEMP[1], elevation.m = 506,
  #           bar.press = NULL, bar.units = NULL,
  #           out.DO.meas = "mg/L",
  #           salinity = 0, salinity.units = "pp.thou")*1000*(1/32)
  
  #if(OXY1 > OXY_EQ){OXY1 = OXY_EQ}
  #if(OXY2 > OXY_EQ){OXY2 = OXY_EQ}
  
  OXY_oxy <- round(c(OXY1, OXY2), 3)
  
  if(length(which(states_config$state_names != "OXY_oxy")) == 1){
    sss_inflow <- data.frame(time = time_sss, FLOW = FLOW, TEMP = TEMP, SALT = SALT, OXY_oxy = OXY_oxy)
  }else{
    
    NIT_amm <- round(rep(x[wq_start[which(states_config$state_names == "NIT_amm")-1] + depth_index - 1],2), 3)
    NIT_nit <- round(rep(x[wq_start[which(states_config$state_names == "NIT_nit")-1] + depth_index - 1],2), 3)
    PHS_frp <- round(rep(x[wq_start[which(states_config$state_names == "PHS_frp")-1] + depth_index - 1],2), 3)
    OGM_doc <- round(rep(x[wq_start[which(states_config$state_names == "OGM_doc")-1] + depth_index - 1],2), 3)
    OGM_docr <- round(rep(x[wq_start[which(states_config$state_names == "OGM_docr")-1] + depth_index - 1],2), 3)
    OGM_poc <- round(rep(x[wq_start[which(states_config$state_names == "OGM_poc")-1] + depth_index - 1],2), 3)
    OGM_don <- round(rep(x[wq_start[which(states_config$state_names == "OGM_don")-1] + depth_index - 1],2), 3)
    OGM_donr <- round(rep(x[wq_start[which(states_config$state_names == "OGM_donr")-1] + depth_index - 1],2), 3)
    OGM_dop <- round(rep(x[wq_start[which(states_config$state_names == "OGM_dop")-1] + depth_index - 1],2), 3)
    OGM_dopr <- round(rep(x[wq_start[which(states_config$state_names == "OGM_dop")-1] + depth_index - 1],2), 3)
    OGM_pop <- round(rep(x[wq_start[which(states_config$state_names == "OGM_pop")-1] + depth_index - 1],2), 3)
    OGM_pon <- round(rep(x[wq_start[which(states_config$state_names == "OGM_pon")-1] + depth_index - 1],2), 3)
    #PHS_frp_ads <- round(rep(x[i-1, m, wq_start[which(state_names == "PHS_frp_ads")-1] + depth_index - 1],2), 3)
    #CAR_dic <- round(rep(x[i-1, m, wq_start[which(state_names == "CAR_dic")-1] + depth_index - 1],2), 3)
    #CAR_ch4 <- round(rep(x[i-1, m, wq_start[which(state_names == "CAR_ch4")-1] + depth_index - 1],2), 3)
    SIL_rsi <- round(rep(x[wq_start[which(states_config$state_names == "SIL_rsi")-1] + depth_index - 1],2), 3)

    sss_inflow <- data.frame(time = time_sss, 
                             FLOW = FLOW, 
                             TEMP = TEMP, 
                             SALT = SALT, 
                             OXY_oxy = OXY_oxy,
                             SIL_rsi = SIL_rsi,
                             NIT_amm = NIT_amm,
                             NIT_nit = NIT_nit,
                             PHS_frp = PHS_frp,
                             OGM_doc = OGM_doc,
                             OGM_docr = OGM_docr,
                             OGM_poc = OGM_poc, 
                             OGM_don = OGM_don,
                             OGM_donr = OGM_donr,
                             OGM_pon = OGM_pon,
                             OGM_dop = OGM_dop,
                             OGM_dopr = OGM_dopr,
                             OGM_pop = OGM_pop
                             #PHS_frp_ads = PHS_frp_ads,
                             #CAR_dic = CAR_dic,
                             #CAR_ch4 = CAR_ch4,
                             )
    
    #sss_inflow <- sss_inflow %>% 
    #  select(vars(c("FLOW", "TEMP", "SALT", all_of(wq_names_tmp))))
  }
  

  
  sss_outflow <- data.frame(time = time_sss, FLOW = FLOW, TEMP = TEMP, SALT = SALT)
  
  write.csv(sss_inflow, paste0(working_directory, "/sss_inflow.csv"), row.names = FALSE, quote = FALSE)
  write.csv(sss_outflow, paste0(working_directory, "/sss_outflow.csv"), row.names = FALSE, quote = FALSE)
}
