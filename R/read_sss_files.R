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
##'

read_sss_files <-  function(full_time_local,
                            sss_file){

  full_time_day_local <- lubridate::as_date(full_time_local)

  d <- readr::read_csv(sss_file, col_type = readr::cols(
    time = readr::col_date(format = ""),
    FLOW = readr::col_double(),
    OXY_oxy = readr::col_double())
  )

  sss_flow <- rep(0, length(full_time_day_local))
  sss_OXY_oxy <- rep(0, length(full_time_day_local))

  if(length(which(d$time == full_time_day_local[1])) > 0){

    for(i in 1:(length(full_time_day_local))){
      index <- which(d$time == full_time_day_local[i])
      if(length(index) > 0){
        sss_flow[i] <- unlist(d[index, "FLOW"])
        sss_OXY_oxy[i] <- unlist(d[index, "OXY_oxy"])
      }
    }
  }

  management_input <- data.frame(sss_flow = sss_flow, sss_OXY_oxy = sss_OXY_oxy)

  return(management_input)
}
