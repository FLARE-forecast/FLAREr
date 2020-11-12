#' Convert NOAA forecasts to GLM format
#'
#' @param obs_met_file
#' @param out_dir
#' @param forecast_dir
#' @param start_datetime
#' @param end_datetime
#' @param forecast_start_datetime
#' @param local_tzone
#' @param use_forecasted_met
#' @param spatial_downscale
#' @param spatial_downscale_coeff
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
                                   forecast_start_datetime_local,
                                   use_forecasted_met,
                                   spatial_downscale = FALSE,
                                   spatial_downscale_coeff = NULL){

  if(is.null(obs_met_file) & is.null(forecast_dir)){
    stop("missing files to convert")
  }

  start_datetime_UTC <- lubridate::with_tz(start_datetime_local, tzone = "UTC")
  end_datetime_UTC <- lubridate::with_tz(end_datetime_local, tzone = "UTC") - lubridate::hours(1)
  forecast_start_datetime_UTC <- lubridate::with_tz(forecast_start_datetime_local, tzone = "UTC")

  full_time_UTC <- seq(start_datetime_UTC, end_datetime_UTC, by = "1 hour")
  if(use_forecasted_met){
    if(forecast_start_datetime_UTC > start_datetime_UTC){
    full_time_UTC_hist <- seq(start_datetime_UTC, forecast_start_datetime_UTC - lubridate::hours(1), by = "1 hour")
    }else{
      full_time_UTC_hist <- NULL
    }
  }else{
    full_time_UTC_hist <- seq(start_datetime_UTC, end_datetime_UTC, by = "1 hour")
  }
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

  if(!is.null(obs_met_file) & !is.null(full_time_UTC_hist)){

    obs_met_nc <- ncdf4::nc_open(obs_met_file)

    obs_met_time <- ncdf4::ncvar_get(obs_met_nc, "time")

    origin <- stringr::str_sub(ncdf4::ncatt_get(obs_met_nc, "time")$units, 13, 28)

    origin <- lubridate::ymd_hm(origin)

    obs_met_time <- origin + lubridate::hours(obs_met_time)

    met <- tibble::tibble(time = obs_met_time)

    for(i in 1:length(cf_met_vars)){

      met <- cbind(met, ncdf4::ncvar_get(obs_met_nc, cf_met_vars[i]))
    }

    ncdf4::nc_close(obs_met_nc)

    names(met) <- c("time", glm_met_vars)

    met <- met %>%
      dplyr::filter(time %in% full_time_UTC_hist)

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

    if(!is.null(forecast_dir) & use_forecasted_met){


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

      ncdf4::nc_close(noaa_met_nc)
      names(noaa_met) <- c("time", glm_met_vars)

      if(spatial_downscale){

        noaa_met_daily <- noaa_met %>%
          mutate(date = lubridate::as_date(time)) %>%
          select(-time) %>%
          group_by(date) %>%
          summarise_all(mean, na.rm = TRUE) %>%
          mutate(AirTemp_debias = spatial_downscale_coeff$AirTemp[1] + AirTemp * spatial_downscale_coeff$AirTemp[2],
                 AirTemp_orig = AirTemp,
                 ShortWave_debias = spatial_downscale_coeff$ShortWave[1] + ShortWave * spatial_downscale_coeff$ShortWave[2],
                 ShortWave_debias = ifelse(ShortWave_debias < 0, 0, ShortWave_debias),
                 ShortWave_orig = ShortWave,
                 LongWave_debias = spatial_downscale_coeff$LongWave[1] + LongWave * spatial_downscale_coeff$LongWave[2],
                 LongWave_debias = ifelse(LongWave_debias < 0, 0, LongWave_debias),
                 LongWave_orig = LongWave) %>%
          select(date, AirTemp_debias, AirTemp_orig, ShortWave_debias, ShortWave_orig, LongWave_debias, LongWave_orig)

        noaa_met <- noaa_met %>%
          mutate(date = lubridate::as_date(time)) %>%
          left_join(noaa_met_daily, by = "date") %>%
          mutate(AirTemp = (AirTemp/AirTemp_orig) * AirTemp_debias,
                 ShortWave = (ShortWave/ShortWave_orig) * ShortWave_debias,
                 LongWave = (LongWave/LongWave_orig) * LongWave_debias) %>%
          mutate(ShortWave = ifelse(ShortWave < 0, 0, ShortWave),
                  LongWave = ifelse(LongWave < 0, 0, LongWave)) %>%
          select(time, AirTemp, ShortWave, LongWave, RelHum, WindSpeed, Rain)
      }

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

    readr::write_csv(combined_met,file = paste0(out_dir, "/", current_filename), quote_escape = "none")

    filenames[j] <- paste0(out_dir, "/", current_filename)

  }

  return(filenames)
}
