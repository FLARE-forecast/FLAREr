# -----------------------------------
# Information
# -----------------------------------
# Purpose: Combine saved daily NOAA forecasts into dataframes
# Creator: Laura Puckett, December 14 2018
# Contact: plaura1@vt.edu
# -----------------------------------
# Description
# -----------------------------------
# Take the first day of each 16-day forecast and combine into single flux (longwave, shortwave, precipitation) and state (temperature, wind speed, relative humidity) dataframes
# -----------------------------------
# Input
# -----------------------------------
# @param data.path: path to SCCData-noaa-data folder, which contains 16-day NOAA forecasts (.csv) saved on many days
# -----------------------------------

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

process_saved_forecasts <- function(data.path,working_directory, local_tzone){

  # -----------------------------------
  # 0. Load data, initialize variables
  # -----------------------------------
  forecast.files.list <- list.files(data.path, "*00z.csv")
  flux.forecasts <- NULL
  state.forecasts <- NULL
  for(i in 1:length(forecast.files.list)){
    tmp.data <- read.csv(paste0(data.path, "/", forecast.files.list[i]))
    if(lubridate::as_datetime(tmp.data$forecast.date[1]) < lubridate::as_datetime('2018-12-07 00:00:00')){
      input_tz = "EST5EDT"
    }else{input_tz = "GMT"}
    #input_tz = "GMT"
    tmp.data <- tmp.data %>%
      dplyr::mutate(forecast.date = as_datetime(forecast.date, tz = input_tz))
    tmp.data$forecast.date = with_tz(tmp.data$forecast.date, local_tzone)
    tmp.min.time = min(tmp.data$forecast.date)
    tmp.state <- tmp.data %>%
      dplyr::filter(forecast.date <= tmp.min.time + 18*60*60) %>%
      dplyr::select(ensembles, tmp2m, rh2m, vgrd10m, ugrd10m, forecast.date)
    tmp.flux <- tmp.data %>%
      dplyr::filter(forecast.date <= tmp.min.time + 24*60*60 & forecast.date > tmp.min.time) %>%
      dplyr::select(ensembles, forecast.date,pratesfc, dlwrfsfc, dswrfsfc)
    flux.forecasts = rbind(flux.forecasts, tmp.flux)
    state.forecasts = rbind(state.forecasts, tmp.state)
  }
  saveRDS(flux.forecasts, file = paste(working_directory,"/NOAA.flux.forecasts", sep = ""))
  saveRDS(state.forecasts, file = paste(working_directory,"/NOAA.state.forecasts", sep = ""))
}

