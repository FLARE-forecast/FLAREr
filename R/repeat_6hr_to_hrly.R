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


repeat_6hr_to_hrly <- function(data.6hr){
  data.hrly <- data.6hr %>%
  dplyr::group_by_all() %>%
  tidyr::expand(timestamp = c(timestamp  - 1*60*60,
                       timestamp  - 2*60*60,
                       timestamp  - 3*60*60,
                       timestamp  - 4*60*60,
                       timestamp  - 5*60*60,
                       timestamp  - 6*60*60)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(timestamp >= min(data.6hr$timestamp)) %>%
    dplyr::arrange(timestamp)
return(data.hrly)
}
