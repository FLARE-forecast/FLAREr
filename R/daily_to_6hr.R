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


daily_to_6hr <- function(forecasts, daily.forecast, debiased, VarNames){
  grouping <- c("NOAA.member")
  if("fday.group" %in% colnames(forecasts)){
    grouping <- append(grouping, "fday.group")
  }else{
    grouping <- append(grouping, "date")
    forecasts <- forecasts %>%
      dplyr::mutate(date = lubridate::as_date(timestamp))
  }



  deviations <- dplyr::full_join(daily.forecast, forecasts, by = grouping, suffix = c(".daily",".6hr"))
  devNames = NULL
  for(Var in 1:length(VarNames)){
    deviations[,paste0(VarNames[Var],".prop")] = deviations[,paste0(VarNames[Var], ".6hr")] / deviations[,paste0(VarNames[Var], ".daily")]
    devNames <- append(devNames, paste0(VarNames[Var],".prop"))
    deviations[which(is.nan(unlist(deviations[,paste0(VarNames[Var],".prop")]))),paste0(VarNames[Var],".prop")] <- 0.0
  }
  deviations <- deviations %>%
    dplyr::select(all_of(grouping), timestamp, all_of(devNames))

  redistributed <- dplyr::inner_join(debiased, deviations, by = grouping)
  for(Var in 1:length(VarNames)){
    redistributed[,VarNames[Var]] = redistributed[,VarNames[Var]] * redistributed[,paste0(VarNames[Var], ".prop")]
  }

  redistributed <- redistributed %>%
    dplyr::select(NOAA.member, timestamp, all_of(VarNames), dscale.member)

  return(redistributed)
}

