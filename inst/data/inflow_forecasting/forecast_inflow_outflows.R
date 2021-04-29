
forecast_inflows_outflows <- function(inflow_obs,
                                      forecast_files,
                                      obs_met_file,
                                      output_dir,
                                      inflow_model,
                                      inflow_process_uncertainty,
                                      forecast_location,
                                      config){

  inflow <- readr::read_csv(inflow_obs, col_types = readr::cols())

  lake_name_code <- config$lake_name_code
  local_tzone <- config$local_tzone

  curr_all_days <- NULL

  noaa_met_nc <- ncdf4::nc_open(forecast_files[1])
  noaa_met_time <- ncdf4::ncvar_get(noaa_met_nc, "time")
  origin <- stringr::str_sub(ncdf4::ncatt_get(noaa_met_nc, "time")$units, 13, 28)
  origin <- lubridate::ymd_hm(origin)
  noaa_met_time <- origin + lubridate::hours(noaa_met_time)

  obs_met_nc <- ncdf4::nc_open(obs_met_file)
  obs_met_time <- ncdf4::ncvar_get(obs_met_nc, "time")
  origin <- stringr::str_sub(ncdf4::ncatt_get(obs_met_nc, "time")$units, 13, 28)
  origin <- lubridate::ymd_hm(origin)
  obs_met_time <- origin + lubridate::hours(obs_met_time)
  AirTemp <- ncdf4::ncvar_get(obs_met_nc, "air_temperature")
  Rain <- ncdf4::ncvar_get(obs_met_nc, "precipitation_flux")

  run_date <- lubridate::as_date(noaa_met_time[1])
  run_cycle <- lubridate::hour(noaa_met_time[1])
  if(run_cycle < 10){run_cycle <- paste0("0",run_cycle)}

  run_dir <- file.path(output_dir, inflow_model, lake_name_code, run_date, run_cycle)

  if(!dir.exists(run_dir)){
    dir.create(run_dir, recursive = TRUE)
  }

  ncdf4::nc_close(obs_met_nc)

  met <- tibble::tibble(time = obs_met_time,
                        AirTemp = AirTemp,
                        Rain = Rain)
  obs_met <- met %>%
    dplyr::filter(time >= noaa_met_time[1] - lubridate::days(1) & time < noaa_met_time[1])

  idx <- which(as.character(inflow$time) == as.character(lubridate::as_date(noaa_met_time[1]) - lubridate::days(1)))
  init_flow_temp <- inflow[idx, ]
  # init_flow_temp <- inflow %>%
  #   filter(time == lubridate::as_date(noaa_met_time[1]) - lubridate::days(1))


  if(length(init_flow_temp$FLOW) == 0){
    previous_run_date <- run_date - lubridate::days(1)
    previous_end_date <- end_date - lubridate::days(1)
    previous_run_dir <- file.path(output_dir, inflow_model, lake_name_code, run_date, run_cycle)

    previous_files <- list.files(previous_run_dir)

    init_flow <- rep(NA, length(forecast_files))
    init_temp <- rep(NA, length(forecast_files))

    for(j in 1:length(previous_files)){
      ens <- dplyr::last(unlist(stringr::str_split(basename(previous_files[j]),"_")))
      ens <- stringr::str_sub(ens,1,5)
      previous_inflow <- readr::read_csv(previous_files[j])
      init_flow[j] <-  previous_inflow$FLOW[1]
      init_temp[j] <-  previous_inflow$TEMP[1]
    }
  }else{
    init_flow <- rep(init_flow_temp$FLOW[1], length(forecast_files))
    init_temp <- rep(init_flow_temp$TEMP[1], length(forecast_files))

  }

  for(j in 1:length(forecast_files)){

    ens <- dplyr::last(unlist(stringr::str_split(basename(forecast_files[j]),"_")))
    ens <- stringr::str_sub(ens,1,5)
    noaa_met_nc <- ncdf4::nc_open(forecast_files[j])
    noaa_met_time <- ncdf4::ncvar_get(noaa_met_nc, "time")
    origin <- stringr::str_sub(ncdf4::ncatt_get(noaa_met_nc, "time")$units, 13, 28)
    origin <- lubridate::ymd_hm(origin)
    noaa_met_time <- origin + lubridate::hours(noaa_met_time)
    AirTemp <- ncdf4::ncvar_get(noaa_met_nc, "air_temperature")
    Rain <- ncdf4::ncvar_get(noaa_met_nc, "precipitation_flux")
    ncdf4::nc_close(noaa_met_nc)
    noaa_met <- tibble::tibble(time = noaa_met_time,
                               AirTemp = AirTemp,
                               Rain = Rain)

    noaa_met <- rbind(obs_met, noaa_met)

    curr_met_daily <- noaa_met %>%
      dplyr::mutate(AirTemp = AirTemp - 273.15,
                    Rain = Rain * (60 * 60 * 24)/1000) %>%
      dplyr::mutate(time = lubridate::with_tz(time, tzone = local_tzone),
                    time = time - lubridate::hours(lubridate::hour(time[1]))) %>%
      dplyr::mutate(time = lubridate::as_date(time)) %>%
      dplyr::group_by(time) %>%
      dplyr::summarize(Rain = mean(Rain),
                       AirTemp = mean(AirTemp),.groups = 'drop') %>%
      dplyr::mutate(ensemble = ens) %>%
      dplyr::mutate(AirTemp_lag1 = dplyr::lag(AirTemp, 1),
                    Rain_lag1 = dplyr::lag(Rain, 1)) %>%
      dplyr::slice(-1) %>%
      dplyr::mutate(FLOW = NA,
                    TEMP = NA)

    curr_met_daily$FLOW[1] <- init_flow[j]
    curr_met_daily$TEMP[1] <- init_temp[j]

    if(inflow_process_uncertainty == TRUE){
      inflow_error <- rnorm(nrow(curr_met_daily), 0, config$future_inflow_flow_error)
      temp_error <- rnorm(nrow(curr_met_daily), 0, config$future_inflow_temp_error)
    }else{
      inflow_error <- rep(0.0, nrow(curr_met_daily))
      temp_error <- rep(0.0, nrow(curr_met_daily))
    }

    for(i in 2:nrow(curr_met_daily)){
      curr_met_daily$FLOW[i] = config$future_inflow_flow_coeff[1] +
        config$future_inflow_flow_coeff[2] * curr_met_daily$FLOW[i - 1] +
        config$future_inflow_flow_coeff[3] * curr_met_daily$Rain_lag1[i] + inflow_error[i]
      curr_met_daily$TEMP[i] = config$future_inflow_temp_coeff[1] +
        config$future_inflow_temp_coeff[2] * curr_met_daily$TEMP[i-1] +
        config$future_inflow_temp_coeff[3] * curr_met_daily$AirTemp_lag1[i] + temp_error[i]
    }

    curr_met_daily <- curr_met_daily %>%
      dplyr::mutate(FLOW = ifelse(FLOW < 0.0, 0.0, FLOW))

    curr_met_daily <- curr_met_daily %>%
      dplyr::mutate(SALT = 0.0) %>%
      dplyr::select(time, FLOW, TEMP, SALT, AirTemp, Rain) %>%
      dplyr::mutate_at(dplyr::vars(c("FLOW", "TEMP", "SALT")), list(~round(., 4))) %>%
      dplyr::mutate(type = "inflow",
                    inflow_num = 1) %>%
      dplyr::slice(-1)

    curr_met_daily_output <- curr_met_daily %>%
      dplyr::select(time, FLOW, TEMP) %>%
      dplyr::mutate(type = "outflow",
                    outflow_num = 1)

    forecast_date <- run_date
    end_date <- dplyr::last(curr_met_daily$time)


    identifier_inflow <- paste0("INFLOW-", inflow_model,"_", lake_name_code, "_", format(run_date, "%Y-%m-%d"),"_",
                                format(end_date, "%Y-%m-%d"))

    identifier_outflow <- paste0("OUTFLOW-", inflow_model, "_", lake_name_code, "_", format(run_date, "%Y-%m-%d"), "_",
                                 format(end_date, "%Y-%m-%d"))

    inflow_file_name <- file.path(run_dir, paste0(identifier_inflow,"_", ens, ".csv"))
    outflow_file_name <- file.path(run_dir, paste0(identifier_outflow,"_", ens, ".csv"))

    readr::write_csv(x = curr_met_daily,
                     file = inflow_file_name)

    readr::write_csv(x = curr_met_daily_output,
                     file = outflow_file_name)

  }
  return(run_dir)
}
