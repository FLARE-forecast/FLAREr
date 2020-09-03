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



extract_observations <- function(fname,
                               start_datetime_local,
                               end_datetime_local,
                               modeled_depths,
                               local_tzone,
                               target_variable,
                               distance_threshold_meter,
                               time_threshold_seconds,
                               methods){

  full_time_local <- seq(start_datetime_local, end_datetime_local, by = "1 day")

  col_types <- readr::cols(
    timestamp = readr::col_datetime(format = ""),
    depth = readr::col_double(),
    value = readr::col_double(),
    variable = readr::col_character(),
    method = readr::col_character())

  d <- readr::read_csv(fname,
                col_types = col_types)

  d$timestamp <- lubridate::with_tz(d$timestamp, tzone = local_tzone)

  obs <- array(NA,dim = c(length(full_time_local),length(modeled_depths)))

  d <- d %>%
    dplyr::filter(variable == target_variable,
           method %in% methods,
           (timestamp >= dplyr::first(full_time_local) - time_threshold_seconds),
           (timestamp <= dplyr::last(full_time_local) + time_threshold_seconds))

  for(i in 1:length(full_time_local)){
    for(j in 1:length(modeled_depths)){
      d1 <- d %>%
        dplyr::filter(abs(difftime(timestamp, full_time_local[i], units = "secs")) < time_threshold_seconds &
                 abs(depth-modeled_depths[j]) < distance_threshold_meter) %>%
        dplyr::summarize(value = mean(value, na.rm = TRUE)) %>%
        dplyr:: mutate(value = ifelse(is.na(value),NA, value),
               value = ifelse(is.nan(value),NA, value))
      obs[i,j] <- d1$value
    }
  }

  return(obs)
}
