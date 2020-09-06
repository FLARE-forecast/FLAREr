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


aggregate_obs_to_hrly <- function(observations){
  obs.tz <- attributes(observations$timestamp)$tzone
  hrly.flux.obs <- observations %>%
    dplyr::select(-AirTemp, -RelHum, -WindSpeed) %>%
    dplyr::mutate(date = lubridate::date(timestamp)) %>%
    dplyr::mutate(hour = lubridate::hour(timestamp)) %>%
    dplyr::select(-timestamp) %>%
    dplyr::group_by(date, hour) %>%
    dplyr::summarize_all("mean", na.rm = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(timestamp = lubridate::as_datetime(paste(date, " ", hour, ":","00:00", sep = ""), tz = obs.tz)) %>% # add one hour so that timestamp represents average over past hour
    dplyr::select(-date, -hour)


  hrly.state.obs <- observations %>% dplyr::group_by(timestamp) %>%
    dplyr::summarize(AirTemp = mean(AirTemp, na.rm = FALSE),
                     RelHum = mean(RelHum, na.rm = FALSE),
                     WindSpeed = mean(WindSpeed, na.rm = FALSE)) %>%
    dplyr::ungroup()
  hrly.obs <- dplyr::inner_join(hrly.flux.obs, hrly.state.obs, by = "timestamp")
  return(hrly.obs)
}
