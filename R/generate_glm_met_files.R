#' Convert NOAA forecasts to GLM format
#'
#' @param obs_met_file
#' @param out_dir
#' @param forecast_dir
#' @param config
#'
#' @return
#' @export
#'
#' @examples
generate_glm_met_files <- function(obs_met_file = NULL,
                                   out_dir,
                                   forecast_dir = NULL,
                                   config,
                                   use_forecasted_met){

  if(is.null(obs_met_file) & is.null(forecast_dir)){
    stop("missing files to convert")
  }

  start_datetime <- lubridate::as_datetime(config$run_config$start_datetime)
  if(is.na(config$run_config$forecast_start_datetime)){
    end_datetime <- lubridate::as_datetime(config$run_config$end_datetime)
    forecast_start_datetime <- end_datetime
  }else{
    forecast_start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime)
    end_datetime <- forecast_start_datetime + lubridate::days(config$run_config$forecast_horizon)
  }

  full_time <- seq(start_datetime, end_datetime, by = "1 hour")
  if(config$met$use_forecasted_met){
    if(forecast_start_datetime > start_datetime){
      full_time_hist <- seq(start_datetime, forecast_start_datetime - lubridate::hours(1), by = "1 hour")
    }else{
      full_time_hist <- NULL
    }
  }else{
    full_time_hist <- seq(start_datetime, end_datetime, by = "1 hour")
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

  if(!is.null(obs_met_file) & !is.null(full_time_hist)){

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
      dplyr::filter(time %in% full_time_hist)

    if(!(dplyr::last(full_time_hist) %in% met$time)){
      historical_met_error <- TRUE
    }else{
      historical_met_error <- FALSE
    }

  }else{
    met <- NULL
    historical_met_error <- FALSE

  }


  if(!is.null(forecast_dir)){

    forecast_files <- list.files(forecast_dir, pattern = ".nc", full.names = TRUE)

    forecast_files <- forecast_files[!stringr::str_detect(string = forecast_files, pattern = basename(obs_met_file))]

    nfiles <-   length(forecast_files)

  }else if(!is.null(met)){

    nfiles <-   1
  }

  filenames <- rep(NA, nfiles)

  for(j in 1:nfiles){

    if(!is.null(forecast_dir) & config$met$use_forecasted_met){


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

      noaa_met <- noaa_met %>%
        dplyr::filter(time %in% full_time)

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

    combined_met$time <- strftime(combined_met$time, format="%Y-%m-%d %H:%M", tz = "UTC")

    readr::write_csv(combined_met,file = paste0(out_dir, "/", current_filename), quote_escape = "none")

    filenames[j] <- paste0(out_dir, "/", current_filename)
  }

  return(list(filenames = gsub("\\\\", "/", filenames),
              historical_met_error = historical_met_error))
}
