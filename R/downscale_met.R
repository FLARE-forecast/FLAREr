# --------------------------------------
# purpose: downscale GEFS forecast to specific site & hr-resolution
# Creator: Laura Puckett, December 21 2018
# contact: plaura1@vt.edu
# --------------------------------------
# summary: this function spatially downscaled forecasts from GEFS cell size to the specific site location and temporally downscaled from 6-hr resolution to hr-resolution using saved parameters from earlier fitting process (fit_downscaling_parameters.R)
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

downscale_met <- function(forecasts,
                          debiased.coefficients,
                          VarInfo,
                          PLOT,
                          local_tzone,
                          debiased.covar,
                          n_ds_members,
                          n_met_members,
                          met_downscale_uncertainty,
                          lake_latitude,
                          lake_longitude){
  # -----------------------------------
  # 0. summarize forecasts to ensemble mean if USE_ENSEMBLE_MEAN is TRUE
  # -----------------------------------


  time0 <- min(forecasts$timestamp)
  tzone <- attributes(forecasts$timestamp)$tzone
  forecasts <- forecasts %>%
    dplyr::mutate(fday = as.numeric(difftime(timestamp, time0))/(24*60*60),
                  fday.group = as.integer(fday + 0.75),
                  fday.group = ifelse(timestamp == time0, 0, fday.group))
  # -----------------------------------
  # 1. aggregate forecasts and observations to daily resolution
  # -----------------------------------

  daily.forecast <- flare::aggregate_to_daily(forecasts) %>%
    dplyr::select(-date) # %>% filter(fday.group > 0))

  # -----------------------------------
  # 2. load saved parameters and spatially debias at daily scale
  # -----------------------------------


  debiased <- flare::daily_debias_from_coeff(daily.forecast, debiased.coefficients, VarInfo)

  #for(i in 1:nrow(debiased)){
  #  if(!is.na(debiased[i, 6])){
  #  debiased[i,3:7] <- rmvnorm(1, mean = as.numeric(debiased[i,3:7]), sigma = as.matrix(debiased.covar[1:5,1:5]))
  #  }
  #}


  # -----------------------------------
  # 3. create ensembles, add noise
  # ----------------------------------

  if(met_downscale_uncertainty == TRUE){
    print("with downscaling noise")

    debiased <- flare::add_noise(debiased = debiased,
                         cov = debiased.covar,
                         n_ds_members,
                         n_met_members,
                         VarNames = VarInfo$VarNames) %>%
      dplyr::mutate(ShortWave = ifelse(ShortWaveOld == 0, 0, ShortWave),
             ShortWave = ifelse(ShortWave < 0, 0, ShortWave),
             RelHum = ifelse(RelHum <0, 0, RelHum),
             RelHum = ifelse(RelHum > 100, 100, RelHum),
             WindSpeed = ifelse(WindSpeed <0, 0, WindSpeed),
             Rain = ifelse(Rain <0, 0, Rain)) %>%
      dplyr::arrange(NOAA.member, dscale.member)
    print("noise added")

  }else{
    print("without downscaling noise")
    debiased <- debiased %>%
      dplyr::mutate(dscale.member = 1) %>%
      dplyr::mutate(ShortWave = ifelse(ShortWave < 0, 0, ShortWave),
             RelHum = ifelse(RelHum < 0, 0, RelHum),
             RelHum = ifelse(RelHum > 100, 100, RelHum),
             Rain = ifelse(Rain < 0, 0, Rain)) %>%
      dplyr::arrange(NOAA.member, dscale.member)
  }

  # -----------------------------------
  # 4.a. temporal downscaling step (a): redistribute to 6-hourly resolution
  # -----------------------------------
  redistributed <- flare::daily_to_6hr(forecasts, daily.forecast, debiased, VarNames = VarInfo$VarNames)

  # -----------------------------------
  # 4.b. temporal downscaling step (b): temporally downscale from 6-hourly to hourly
  # -----------------------------------

  ## downscale states to hourly resolution (air temperature, relative humidity, average wind speed)
  VarNamesStates <- VarInfo %>%
    dplyr::filter(VarType == "State")
  VarNamesStates = VarNamesStates$VarNames
  states.ds.hrly = spline_to_hourly(redistributed,
                                    VarNamesStates = VarNamesStates)
  # if filtering out incomplete days, that would need to happen here

  VarNames_6hr <- VarInfo %>%
    dplyr::filter(ds_res == "6hr")
  VarNames_6hr = VarNames_6hr$VarNames

  ## convert longwave to hourly (just copy 6 hourly values over past 6-hour time period)
  nonSW.flux.hrly <- redistributed %>%
    dplyr::select(timestamp, NOAA.member, all_of(VarNames_6hr), dscale.member) %>%
    repeat_6hr_to_hrly()

  ## downscale shortwave to hourly
  #lake_latitude <- 37.307
  #lake_longitude <- 79.837

  ShortWave.ds <- flare::ShortWave_to_hrly(debiased, time0, lat = lake_latitude, lon = 360 - lake_longitude, local_tzone)

  # -----------------------------------
  # 5. join debiased forecasts of different variables into one dataframe
  # -----------------------------------

  joined.ds <- dplyr::full_join(states.ds.hrly, ShortWave.ds, by = c("timestamp","NOAA.member","dscale.member"), suffix = c(".obs",".ds")) %>%
    dplyr::full_join(nonSW.flux.hrly, by = c("timestamp","NOAA.member","dscale.member"), suffix = c(".obs",".ds")) %>%
    dplyr::filter(timestamp >= min(forecasts$timestamp) & timestamp <= max(forecasts$timestamp))

  return(joined.ds)

}


