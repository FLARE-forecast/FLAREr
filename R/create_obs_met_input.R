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
#' @export
#'
#' @author Quinn Thomas
#'



create_obs_met_input <- function(fname,
                                 outfile,
                                 full_time_local,
                                 local_tzone,
                                 working_directory,
                                 hist_days,
                                 missing_met_data_threshold) {

  full_time_hour_local <- seq(full_time_local[1] - lubridate::days(5),
                              full_time_local[length(full_time_local)],
                              by = "1 hour")

  d <- readr::read_csv(fname, col_types = readr::cols())

  d$time <- lubridate::force_tz(d$time, tz = local_tzone)

  ShortWave <- rep(NA, length(full_time_hour_local))
  LongWave <- rep(NA, length(full_time_hour_local))
  AirTemp <- rep(NA, length(full_time_hour_local))
  RelHum <- rep(NA, length(full_time_hour_local))
  WindSpeed <- rep(NA, length(full_time_hour_local))
  Rain <- rep(NA, length(full_time_hour_local))
  Snow <- rep(NA, length(full_time_hour_local))

  if(length(which(d$time %in% full_time_hour_local)) > 0){

    for(i in 1:(length(full_time_hour_local))){
      index <- which(d$time == full_time_hour_local[i])
      if(length(index) > 0){
        ShortWave[i] <- d$ShortWave[index]
        LongWave[i] <- d$LongWave[index]
        AirTemp[i] <- d$AirTemp[index]
        RelHum[i] <- d$RelHum[index]
        WindSpeed[i] <- d$WindSpeed[index]
        Rain[i] <- (d$Rain[index] * 24) / 1000
        Snow[i] <- 0
      }
    }

    observed_hours <- which(full_time_hour_local %in% d$time)

    ShortWave <- ShortWave[observed_hours]
    LongWave <- LongWave[observed_hours]
    AirTemp <- AirTemp[observed_hours]
    RelHum <- RelHum[observed_hours]
    WindSpeed <- WindSpeed[observed_hours]
    Rain <- Rain[observed_hours]
    Snow <- Snow[observed_hours]
    full_time_hour_local <- full_time_hour_local[observed_hours]

    if(hist_days > 0){
      last_observed_day <- seq(full_time_local[hist_days],
                               full_time_local[hist_days + 1],
                               by = "1 hour")
      if(length(which(!(last_observed_day %in% d$time))) > 0){
        missing_met <- TRUE
      }
    }

    na_hours <- length(which(is.na(AirTemp)))
    if(na_hours < missing_met_data_threshold){
      missing_met <- FALSE
    }else{
      missing_met <- TRUE
    }

    ShortWave <- imputeTS::na_interpolation(ShortWave, option = "linear")
    LongWave <- imputeTS::na_interpolation(LongWave, option = "linear")
    AirTemp <- imputeTS::na_interpolation(AirTemp, option = "linear")
    RelHum <- imputeTS::na_interpolation(RelHum, option = "linear")
    WindSpeed <- imputeTS::na_interpolation(WindSpeed, option = "linear")
    Rain <- imputeTS::na_interpolation(Rain, option = "linear")
    Snow <- imputeTS::na_interpolation(Snow, option = "linear")

    historical_met <- data.frame(full_time_hour_local,
                                 ShortWave,
                                 LongWave,
                                 AirTemp,
                                 RelHum,
                                 WindSpeed,
                                 Rain,
                                 Snow)




    n <- noquote(c("time",
                   "ShortWave",
                   "LongWave",
                   "AirTemp",
                   "RelHum",
                   "WindSpeed",
                   "Rain",
                   "Snow"))

    colnames(historical_met) <- noquote(c("time",
                                          "ShortWave",
                                          "LongWave",
                                          "AirTemp",
                                          "RelHum",
                                          "WindSpeed",
                                          "Rain",
                                          "Snow"))

    write.csv(historical_met, file = paste0(working_directory, "/", outfile), row.names = FALSE, quote = FALSE)
  }else{
    missing_met <- TRUE
  }
  return(missing_met)
}
