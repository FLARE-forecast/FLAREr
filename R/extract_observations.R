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



extract_observations <- function(fname,
                               start_datetime,
                               end_datetime,
                               modeled_depths,
                               target_variable,
                               distance_threshold_meter,
                               time_threshold_seconds,
                               methods){

  full_time_local <- seq(start_datetime_local, end_datetime_local, by = "1 day")

  d <- readr::read_csv(fname,
                col_types = readr::cols())

  d$timestamp <- lubridate::with_tz(d$timestamp, tzone = "UTC")

  obs <- array(NA,dim = c(length(full_time_local),length(modeled_depths)))

  for(i in 1:length(full_time_local)){
    for(j in 1:length(modeled_depths)){
      d1 <- d %>%
        dplyr::filter(timestamp == full_time_local[i] &
                 abs(depth-modeled_depths[j]) < distance_threshold_meter)

      obs[i,j] <- d1$value
    }
  }

  return(obs)
}
