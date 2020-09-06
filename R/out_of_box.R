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

out_of_box <- function(forecasts,VarNames){
  forecasts.hrly <- forecasts %>%
    dplyr::mutate(Snow = 0) %>%
    dplyr::select(NOAA.member, timestamp, Rain, Snow, ShortWave, LongWave) %>%
    repeat_6hr_to_hrly() %>%
    dplyr::full_join(forecasts %>% dplyr::select(-Rain, -ShortWave, -LongWave), by = c("timestamp","NOAA.member")) %>%
    dplyr::arrange(NOAA.member, timestamp) %>%
    dplyr::group_by(NOAA.member) %>%
    dplyr::mutate(AirTemp = imputeTS::na.interpolation(AirTemp, option = "linear"),
           RelHum = imputeTS::na.interpolation(RelHum, option = "linear"),
           WindSpeed = imputeTS::na.interpolation(WindSpeed, option = "linear")) %>%
    dplyr::ungroup() %>%
    dplyr::select(timestamp, NOAA.member, VarNames, Rain, Snow)

  return(forecasts.hrly)

}

