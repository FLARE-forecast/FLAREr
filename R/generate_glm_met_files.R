#' Convert NOAA forecasts to GLM format
#'
#' @param obs_met_file
#' @param out_dir
#' @param forecast_dir
#' @param start_datetime
#' @param end_datetime
#' @param forecast_start_datetime
#' @param local_tzone
#'
#' @return
#' @export
#'
#' @examples
generate_glm_met_files <- function(obs_met_file = NULL,
                                   out_dir,
                                   forecast_dir = NULL,
                                   local_tzone,
                                   start_datetime_local,
                                   end_datetime_local,
                                   forecast_start_datetime_local){

  if(is.null(obs_met_file) & is.null(forecast_dir)){
    stop("missing files to convert")
  }

  start_datetime_UTC <- lubridate::with_tz(start_datetime_local, tzone = "UTC")
  end_datetime_UTC <- lubridate::with_tz(end_datetime_local, tzone = "UTC")
  forecast_start_datetime_UTC <- lubridate::with_tz(forecast_start_datetime_local, tzone = "UTC")

  full_time_UTC <- seq(start_datetime_UTC, end_datetime_UTC, by = "1 hour")
  full_time_UTC_hist <- seq(start_datetime_UTC, forecast_start_datetime_UTC, by = "1 hour")
  cf_met_vars <- c("air_temperature",
                   "surface_downwelling_shortwave_flux_in_air",
                   "surface_downwelling_longwave_flux_in_air",
                   "relative_humidity",
                   "wind_speed",
                   "precipitation_flux")
  glm_met_vars <- c("AirTemp",
                    "ShortWave",
                    "LongWave",
                    "RelHum",
                    "WindSpeed",
                    "Rain")

  if(!is.null(obs_met_file)){
    obs_met_nc <- ncdf4::nc_open(observed_met_file)
    obs_met_time <- ncdf4::ncvar_get(obs_met_nc, "time")
    origin <- stringr::str_sub(ncdf4::ncatt_get(obs_met_nc, "time")$units, 13, 28)
    origin <- lubridate::ymd_hm(origin)
    obs_met_time <- origin + lubridate::hours(obs_met_time)
    met <- tibble::tibble(time = obs_met_time)
    for(i in 1:length(cf_met_vars)){
      met <- cbind(met, ncdf4::ncvar_get(obs_met_nc, cf_met_vars[i]))
    }
    names(met) <- c("time", glm_met_vars)
    met <- met %>%
      dplyr::filter(time %in% full_time_UTC_hist[1:(length(full_time_UTC_hist)-1)])
  }else{
    met <- NULL
  }

  if(!is.null(forecast_dir)){
    forecast_files <- list.files(forecast_dir, full.names = TRUE)
    nfiles <-   length(forecast_files)
  }else if(!is.null(met)){
    nfiles <-   1
  }

  filenames <- rep(NA, nfiles)

  for(j in 1:nfiles){

    if(!is.null(forecast_dir)){

      ens <- dplyr::last(unlist(stringr::str_split(basename(forecast_files[j]),"_")))
      ens <- stringr::str_sub(ens,1,5)
      noaa_met_nc <- ncdf4::nc_open(forecast_files[j])
      noaa_met_time <- ncdf4::ncvar_get(noaa_met_nc, "time")
      origin <- stringr::str_sub(ncdf4::ncatt_get(noaa_met_nc, "time")$units, 13, 28)
      origin <- lubridate::ymd_hm(origin)
      noaa_met_time <- origin + lubridate::hours(noaa_met_time)
      noaa_met <- tibble::tibble(time = noaa_met_time)
      for(i in 1:length(cf_met_vars)){
        noaa_met <- cbind(noaa_met, ncdf4::ncvar_get(noaa_met_nc, cf_met_vars[i]))
      }

      names(noaa_met) <- c("time", glm_met_vars)
      noaa_met <- noaa_met %>%
        dplyr::filter(time %in% full_time_UTC)

      combined_met <- rbind(met, noaa_met)

      current_filename <- paste0('met_',ens,'.csv')
    }else{
      combined_met <- met
      current_filename <- paste0('met.csv')
    }

    #convert units to GLM
    combined_met$AirTemp <- combined_met$AirTemp - 273.15
    combined_met$RelHum <- combined_met$RelHum * 100
    combined_met$Rain <- combined_met$Rain * (60 * 60 * 24)/1000

    combined_met <- combined_met %>%
      dplyr::mutate(Snow = 0.0)

    combined_met <- combined_met %>%
      dplyr::mutate_at(dplyr::vars(all_of(glm_met_vars)), list(~round(., 4)))

    combined_met$time <- lubridate::with_tz(combined_met$time, tzone = local_tzone)
    combined_met$time <- strftime(combined_met$time, format="%Y-%m-%d %H:%M", tz = local_tzone)

    readr::write_csv(combined_met,path = paste0(out_dir, "/", current_filename), quote_escape = "none")

    filenames[j] <- paste0(out_dir, "/", current_filename)
  }

  return(filenames)
}