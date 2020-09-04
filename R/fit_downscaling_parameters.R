# --------------------------------------
# purpose: save coefficients from linear debiasing and temporal downscaling
# Creator: Laura Puckett, December 20 2018
# contact: plaura1@vt.edu
# --------------------------------------
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

## setup

fit_downscaling_parameters <- function(observations,
                                       for.file.path,
                                       working_directory,
                                       VarNames,
                                       VarNamesStates,
                                       replaceObsNames,
                                       PLOT,
                                       local_tzone,
                                       VarInfo,
                                       first_obs_date,
                                       last_obs_date,
                                       lake_latitude,
                                       lake_longitude){

  # process and read in saved forecast data
  flare::process_saved_forecasts(data.path = for.file.path,
                          working_directory,
                          local_tzone) # geneartes flux.forecasts and state.forecasts dataframes
  NOAA.flux <- readRDS(paste(working_directory,"/NOAA.flux.forecasts", sep = ""))
  NOAA.state <- readRDS(paste(working_directory,"/NOAA.state.forecasts", sep = ""))
  NOAA.data <- dplyr::inner_join(NOAA.flux, NOAA.state, by = c("forecast.date","ensembles"))
  NOAA_input_tz <- attributes(NOAA.data$forecast.date)$tzone

  forecasts <- flare::prep_for(NOAA.data, input_tz = NOAA_input_tz, local_tzone, weather_uncertainty = FALSE) %>%
    dplyr::mutate(date = lubridate::as_date(timestamp)) %>%
    dplyr::group_by(NOAA.member, date)  %>%
    dplyr::mutate(n = n()) %>%
    # force NA for days without 4 NOAA entries (because having less than 4 entries would introduce error in daily comparisons)
    dplyr::mutate_at(vars(VarInfo$VarNames), ~ifelse(n == 4, ., NA)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-date, -n) %>%
    dplyr::filter(timestamp >= first_obs_date & timestamp <= last_obs_date)

  # -----------------------------------
  # 3. aggregate forecasts and observations to daily resolution and join datasets
  # -----------------------------------

  daily.forecast <- flare::aggregate_to_daily(data = forecasts)

  daily.obs <- flare::aggregate_to_daily(data = observations)
  # might eventually alter this so days with at least a certain percentage of data remain in dataset (instead of becoming NA if a single minute of data is missing)

  joined.data.daily <- dplyr::inner_join(daily.forecast, daily.obs, by = "date", suffix = c(".for",".obs")) %>%
    dplyr::ungroup() %>%
    dplyr::filter_all(all_vars(is.na(.) == FALSE))

  rm(NOAA.flux, NOAA.state, NOAA.data, daily.obs)

  # -----------------------------------
  # 4. save linearly debias coefficients and do linear debiasing at daily resolution
  # -----------------------------------
  out <- flare::get_daily_debias_coeff(joined.data = joined.data.daily, VarInfo = VarInfo, PLOT, working_directory)
  debiased.coefficients <-  out[[1]]
  debiased.covar <-  out[[2]]

  debiased <- flare::daily_debias_from_coeff(daily.forecast, debiased.coefficients, VarInfo)

  # -----------------------------------
  # 5.a. temporal downscaling step (a): redistribute to 6-hourly resolution
  # -----------------------------------
  redistributed <- flare::daily_to_6hr(forecasts, daily.forecast, debiased, VarNames = VarInfo$VarNames)

  # -----------------------------------
  # 5.b. temporal downscaling step (b): temporally downscale from 6-hourly to hourly
  # -----------------------------------

  ## downscale states to hourly resolution (air temperature, relative humidity, average wind speed)
  VarNamesStates <- VarInfo %>%
    filter(VarType == "State")
  VarNamesStates <- VarNamesStates$VarNames
  states.ds.hrly <- spline_to_hourly(redistributed,
                                    VarNamesStates = VarNamesStates)
  # if filtering out incomplete days, that would need to happen here

  VarNames_6hr <- VarInfo %>%
    filter(ds_res == "6hr")
  VarNames_6hr <- VarNames_6hr$VarNames

  ## convert longwave to hourly (just copy 6 hourly values over past 6-hour time period)
  nonSW.flux.hrly <- redistributed %>%
    dplyr::select(timestamp, NOAA.member, all_of(VarNames_6hr)) %>%
    repeat_6hr_to_hrly()


  ## downscale shortwave to hourly
  ShortWave.ds <- flare::ShortWave_to_hrly(debiased, time0 = NA, lat = lake_latitude, lon = 360 - lake_longitude, local_tzone)

  # -----------------------------------
  # 6. join debiased forecasts of different variables into one dataframe
  # -----------------------------------

  joined.ds <- dplyr::full_join(states.ds.hrly, ShortWave.ds, by = c("timestamp","NOAA.member"), suffix = c(".obs",".ds")) %>%
    dplyr::full_join(nonSW.flux.hrly, by = c("timestamp","NOAA.member"), suffix = c(".obs",".ds")) %>%
    dplyr::filter(timestamp >= min(forecasts$timestamp) & timestamp <= max(forecasts$timestamp))

  # -----------------------------------
  # 7. prepare dataframes of hourly observational data for comparison with forecasts
  # -----------------------------------

  # get hourly dataframe of observations
  hrly.obs <- flare::aggregate_obs_to_hrly(observations)

  # -----------------------------------
  # 8. join hourly observations and hourly debiased forecasts
  # -----------------------------------

  joined.hrly.obs.and.ds <- dplyr::inner_join(hrly.obs,joined.ds, by = "timestamp", suffix = c(".obs",".ds")) %>%
    dplyr::mutate(Rain.ds = ifelse(Rain.ds<0 , 0, Rain.ds),
           ShortWave.ds = ifelse(ShortWave.ds<0, 0, ShortWave.ds),
           LongWave.ds = ifelse(LongWave.ds<0, 0, LongWave.ds),
           RelHum.ds = ifelse(RelHum.ds<0, 0, RelHum.ds),
           RelHum.ds = ifelse(RelHum.ds>100, 100, RelHum.ds))

  # -----------------------------------
  # 9. Calculate and save coefficients from hourly downscaling (R2 and standard deviation of residuals)
  # -----------------------------------

  model <- lm(joined.hrly.obs.and.ds$AirTemp.obs ~ joined.hrly.obs.and.ds$AirTemp.ds)
  debiased.coefficients[5,1] <- sd(residuals(model))
  debiased.coefficients[6,1] <- summary(model)$r.squared

  model <- lm(joined.hrly.obs.and.ds$WindSpeed.obs ~ joined.hrly.obs.and.ds$WindSpeed.ds)
  debiased.coefficients[5,2] <- sd(residuals(model))
  debiased.coefficients[6,2] <- summary(model)$r.squared

  model <- lm(joined.hrly.obs.and.ds$RelHum.obs ~ joined.hrly.obs.and.ds$RelHum.ds)
  debiased.coefficients[5,3] <- sd(residuals(model))
  debiased.coefficients[6,3] <- summary(model)$r.squared

  model <- lm(joined.hrly.obs.and.ds$ShortWave.obs ~ joined.hrly.obs.and.ds$ShortWave.ds)
  debiased.coefficients[5,4] <- sd(residuals(model))
  debiased.coefficients[6,4] <- summary(model)$r.squared

  model <- lm(joined.hrly.obs.and.ds$LongWave.obs ~ joined.hrly.obs.and.ds$LongWave.ds)
  debiased.coefficients[5,5] <- sd(residuals(model))
  debiased.coefficients[6,5] <- summary(model)$r.squared
  save(debiased.coefficients,debiased.covar, file = paste(working_directory,"/debiased.coefficients.RData", sep = ""))

  print(debiased.coefficients)
  # -----------------------------------
  # 10. Visual check (comparing observations and downscaled forecast ensemble mean)
  # -----------------------------------
  if(PLOT == TRUE){
    ggplot2::ggplot(data = joined.hrly.obs.and.ds[1:50000,], aes(x = timestamp)) +
      ggplot2::geom_line(aes(y = AirTemp.obs, color = "observations"))+
      ggplot2::geom_line(aes(y = AirTemp.ds, color = "downscaled forecast average", group = NOAA.member))

    ggplot2::ggplot(data = joined.hrly.obs.and.ds[1:50000,], aes(x = timestamp)) +
      ggplot2::geom_line(aes(y = WindSpeed.obs, color = "observations"))+
      ggplot2::geom_line(aes(y = WindSpeed.ds, color = "downscaled forecast average", group = NOAA.member))

    ggplot2::ggplot(data = joined.hrly.obs.and.ds[1:50000,], aes(x = timestamp)) +
      ggplot2::geom_line(aes(y = RelHum.obs, color = "observations"))+
      ggplot2::geom_line(aes(y = RelHum.ds, color = "downscaled forecast average", group = NOAA.member))

    ggplot2::ggplot(data = joined.hrly.obs.and.ds[1:50000,], aes(x = timestamp)) +
      ggplot2::geom_line(aes(y = ShortWave.obs, color = "observations"))+
      ggplot2::geom_line(aes(y = ShortWave.ds, color = "downscaled forecast average", group = NOAA.member))

    ggplot2::ggplot(data = joined.hrly.obs.and.ds[1:5000,], aes(x = timestamp)) +
      ggplot2::geom_line(aes(y = LongWave.obs, color = "observations"))+
      ggplot2::geom_line(aes(y = LongWave.ds, color = "downscaled forecast average", group = NOAA.member))
    ggplot2::ggplot(data = joined.hrly.obs.and.ds[1:5000,], aes(x = timestamp)) +
      ggplot2::geom_line(aes(y = Rain.obs, color = "observations"))+
      ggplot2::geom_line(aes(y = Rain.ds, color = "downscaled forecast average", group = NOAA.member))
  }
}

