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


aggregate_to_daily <- function(data){
  grouping <- NULL
  if("fday.group" %in% colnames(data)){
    grouping <- append(grouping, "fday.group")
  }else{
    grouping <- append(grouping, "date")
  }
  if("NOAA.member" %in% colnames(data)){
    grouping <- append(grouping, "NOAA.member")
  }

  daily.data <- data %>%
    dplyr::mutate(date = lubridate::as_date(timestamp)) %>%
    dplyr::select(-timestamp) %>%
    dplyr::group_by_at(grouping) %>%
    dplyr::summarize_all(~mean(. , na.rm = FALSE), .groups = "drop") %>%
    dplyr::ungroup()
  if("fday" %in% colnames(daily.data)){
    daily.data <- daily.data %>% dplyr::select(-fday)
  }
  return(daily.data)
}
