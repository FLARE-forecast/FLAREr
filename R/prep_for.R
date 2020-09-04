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


prep_for <- function(NOAA.data, input_tz, local_tzone, weather_uncertainty){
  # --------------------------------------
  # purpose: convert forecasts dataframe to units/names for comparison with observations
  # Creator: Laura Puckett, December 14 2018
  # --------------------------------------
  NOAA.na.value = 999900000000000000000
  forecast.data <- NOAA.data %>%
    dplyr::mutate(timestamp = as.character(forecast.date)) %>%
    dplyr::mutate(timestamp = lubridate::as_datetime(timestamp,
                                         tz = input_tz)) %>%
    plyr::rename(c("ensembles" = "NOAA.member")) %>%
    dplyr::mutate(AirTemp = tmp2m,
                  WindSpeed = sqrt(vgrd10m^2 + ugrd10m^2),
                  LongWave = ifelse(dlwrfsfc==NOAA.na.value, NA, dlwrfsfc),
                  ShortWave = ifelse(dswrfsfc==NOAA.na.value, NA, dswrfsfc),
                  RelHum = rh2m,
                  Rain = pratesfc*60*60*24/1000) %>% # (convert from mm/s to total m over 6 hrs)
    dplyr::select(NOAA.member, timestamp, AirTemp, LongWave, ShortWave, RelHum, WindSpeed, Rain)
  forecast.data$timestamp <- lubridate::with_tz(forecast.data$timestamp, local_tzone)

  if(weather_uncertainty == FALSE){
    forecast.data <- forecast.data %>%
      dplyr::group_by(timestamp) %>%
      dplyr::mutate(AirTemp = mean(AirTemp),
             LongWave = mean(LongWave),
             ShortWave = mean(ShortWave),
             RelHum = mean(RelHum),
             WindSpeed = mean(WindSpeed),
             Rain = mean(Rain)) %>%
      dplyr::ungroup(timestamp)
  }
  return(forecast.data)
}
