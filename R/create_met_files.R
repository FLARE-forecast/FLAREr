##' @title Convert historical meteorology and NOAA forecasts to GLM format
##' @details Function combines historical meteorology and NOAA forecasts to create meteorology input files in the GLM format.  A file is generated for each ensemble member.
##' @param  config list of FLARE configurations
##' @param lake_directory directory of lake configurations
##' @param met_forecast_start_datetime start datetime of met forecasts
##' @param met_start_datetime start datetime of met simulation
##' @return list; vector of full path for the converted files and boolean flag if issues with historical meteorology files
##' @import dplyr
##' @keywords internal
##' @importFrom stringr str_sub str_split str_detect
##' @importFrom tibble tibble
##' @importFrom lubridate as_datetime days hours ymd_hm
##' @author Quinn Thomas
##'
create_met_files <- function(config, lake_directory, met_forecast_start_datetime, met_start_datetime){

  out_dir <- config$file_path$execute_directory
  forecast_horizon <-  config$run_config$forecast_horizon
  use_met_s3 <- config$met$use_met_s3
  bucket <- config$s3$drivers$bucket
  endpoint <- config$s3$drivers$endpoint
  local_directory <- config$met$local_met_directory
  use_ler_vars <- config$met$use_ler_vars
  site_id <- config$location$site_id

  start_datetime <- lubridate::as_datetime(met_start_datetime)

  if(is.na(met_forecast_start_datetime)){
    end_datetime <- lubridate::as_datetime(config$run_config$end_datetime) #- lubridate::hours(1)
    forecast_start_datetime <- end_datetime
  }else{
    forecast_start_datetime <- lubridate::as_datetime(met_forecast_start_datetime)
    end_datetime <- forecast_start_datetime + lubridate::days(forecast_horizon) #- lubridate::hours(1)
  }

  if(!is.na(forecast_start_datetime) & forecast_horizon > 0){

    forecast_date <- lubridate::as_date(forecast_start_datetime)
    forecast_hour <- lubridate::hour(forecast_start_datetime)

    reference_date <- forecast_date

    if(forecast_hour != 0){
      stop("Only forecasts that start at 00:00:00 UTC are currently supported")
    }

    if(config$met$future_met_use_s3){

      if(is.null(bucket) | is.null(endpoint)){
        stop("inflow forecast function needs bucket and endpoint if use_s3=TRUE")
      }
      vars <- arrow_env_vars()

      reference_date <- forecast_date

      forecast_dir <- arrow::s3_bucket(bucket = glue::glue(bucket, "/", config$met$future_met_model),
                                       endpoint_override =  endpoint, anonymous = TRUE)

      unset_arrow_vars(vars)
    }else{
      if(is.null(local_directory)){
        stop("met forecast function needs local_directory if use_s3=FALSE")
      }



      forecast_dir <- arrow::SubTreeFileSystem$create(glue::glue(lake_directory, "/",
                                                                 local_directory, "/",
                                                                 config$met$future_met_model))
    }
  }

  if(forecast_horizon == 0){
    forecast_dir <- NULL
  }

  if(start_datetime < forecast_start_datetime){
    if(config$met$historical_met_use_s3){

      past_dir <- arrow::s3_bucket(bucket =  glue::glue(bucket, "/",
                                                        config$met$historical_met_model),
                                   endpoint_override =  endpoint,
                                   anonymous = TRUE)

    }else{
      past_dir <-  arrow::SubTreeFileSystem$create(glue::glue(lake_directory, "/",
                                                              local_directory, "/",
                                                              config$met$historical_met_model))
    }
  }else{
    past_dir <- NULL
  }


  #if(is.null(obs_met_file) & is.null(forecast_dir)){
  #  stop("missing files to convert")
  #}

  full_time <- seq(start_datetime, end_datetime, by = "1 hour")
  if(forecast_start_datetime > start_datetime){
    full_time_hist <- seq(start_datetime, forecast_start_datetime, by = "1 hour")
  }else{
    full_time_hist <- seq(start_datetime, end_datetime, by = "1 hour")
  }

  if(!is.null(past_dir)){

    hist_met <- arrow::open_dataset(past_dir) |>
      dplyr::select(datetime, parameter,variable,prediction) |>
      dplyr::collect() |>
      dplyr::filter(datetime %in% lubridate::as_datetime(full_time_hist)) |>
      dplyr::distinct() |>
      tidyr::pivot_wider(names_from = variable, values_from = prediction) |>
      dplyr::arrange(parameter, datetime)

    if(!("wind_speed" %in% colnames(hist_met))){
      hist_met <- hist_met |>
        dplyr::mutate(WindSpeed = sqrt(eastward_wind^2 + northward_wind^2))
    }else{
      hist_met <- hist_met |>
        dplyr::mutate(WindSpeed = wind_speed)
    }

    hist_met <- hist_met |>
      dplyr::mutate(use_ler_vars = use_ler_vars) |>
      dplyr::rename(AirTemp = air_temperature,
                    ShortWave = surface_downwelling_shortwave_flux_in_air,
                    LongWave = surface_downwelling_longwave_flux_in_air,
                    RelHum = relative_humidity,
                    Rain = precipitation_flux,
                    ensemble = parameter,
                    time = datetime) |>
      dplyr::mutate(AirTemp = AirTemp - 273.15,
                    RelHum = RelHum * 100,
                    RelHum = ifelse(RelHum > 100, 100, RelHum),
                    Rain = ifelse(use_ler_vars, Rain * (60 * 60), Rain * (60 * 60 * 24)/1000),
                    Snow = 0.0) |>
      dplyr::mutate_at(dplyr::vars(all_of(c("AirTemp", "ShortWave","LongWave","RelHum","WindSpeed"))),
                       list(~round(., 2))) |>
      dplyr::mutate(Rain = round(Rain, 5),
                    time = format(time, format="%Y-%m-%d %H:%M", tz = "UTC")) |>
      dplyr::select(ensemble, time, AirTemp,ShortWave, LongWave, RelHum, WindSpeed,Rain, Snow) |>
      dplyr::group_by(ensemble) |>
      dplyr::slice(-dplyr::n()) |>
      dplyr::ungroup()

    n_gaps <- hist_met |>
      dplyr::mutate(time = lubridate::ymd_hm(time)) |>
      tsibble::as_tsibble(index = time, key = ensemble) |>
      tsibble::count_gaps()

    if( any(hist_met$RelHum <= 0.0)) {
      idx <- which(hist_met$RelHum <= 0.0)
      hist_met$RelHum[idx] <- NA
      hist_met$RelHum <- zoo::na.approx(hist_met$RelHum, rule = 2)
    }


    if (nrow(n_gaps) > 0) {
      n_gaps <- n_gaps |>
        dplyr::summarise(n_gaps = max(.n, na.rm = T)) |> pull()
      message('up to ', n_gaps, ' timesteps of missing data were interpolated per ensemble in stage 3 data')
    }

    # fill in any missed timesteps to ensure a continuous time series
    hist_met <-  hist_met |>
      dplyr::mutate(time = lubridate::ymd_hm(time)) |>
      tsibble::as_tsibble(index = time, key = ensemble) |>
      tsibble::fill_gaps() |>
      dplyr::mutate(across(AirTemp:Snow,imputeTS::na_interpolation)) |>
      dplyr::as_tibble() |>
      dplyr::mutate(time = format(time, format="%Y-%m-%d %H:%M", tz = "UTC"))

  }else{
    hist_met <- NULL
  }



  if(is.null(forecast_dir)){

    ensemble_members <- unique(hist_met$ensemble)
    current_filename <- purrr::map_chr(ensemble_members, function(ens, out_dir, hist_met){
      df <- hist_met |>
        dplyr::filter(ensemble == ens) |>
        dplyr::select(-ensemble) |>
        dplyr::arrange(time)

      if(use_ler_vars){

        df <- df |>
          dplyr::rename(datetime = time,
                        Air_Temperature_celsius = AirTemp,
                        Shortwave_Radiation_Downwelling_wattPerMeterSquared = ShortWave,
                        Longwave_Radiation_Downwelling_wattPerMeterSquared = LongWave,
                        Relative_Humidity_percent = RelHum,
                        Ten_Meter_Elevation_Wind_Speed_meterPerSecond = WindSpeed,
                        Precipitation_millimeterPerHour = Rain,
                        Snowfall_millimeterPerHour = Snow)
      }

      # check for bad data
      missing_data_check(df)

      fn <- paste0("met_",stringr::str_pad(ens, width = 2, side = "left", pad = "0"),".csv")
      fn <- file.path(out_dir, fn)
      write.csv(df, file = fn, quote = FALSE, row.names = FALSE)
      return(fn)
    },
    out_dir,
    hist_met)
  }else{

    #### test if the forecast directory exists, stop with helpful error if it does not ####
    tryCatch({
      forecast <- arrow::open_dataset(forecast_dir) |>
        dplyr::select(datetime, parameter,variable,prediction) |>
        dplyr::collect()
    }, error = function(e) {
      stop(paste0('NOAA Forecasts were not found for reference_datetime = "',forecast_start_datetime,'"...stopping workflow'))
    })

    forecast <- forecast |>
      dplyr::distinct() |>
      tidyr::pivot_wider(names_from = variable, values_from = prediction) |>
      dplyr::arrange(parameter, datetime)

    if(!("wind_speed" %in% colnames(forecast))){
      forecast <- forecast |>
        dplyr::mutate(WindSpeed = sqrt(eastward_wind^2 + northward_wind^2))
    }else{
      forecast <- forecast |>
        dplyr::mutate(WindSpeed = wind_speed)
    }
    forecast <- forecast |>
      dplyr::mutate(use_ler_vars = use_ler_vars) |>
      dplyr::rename(AirTemp = air_temperature,
                    ShortWave = surface_downwelling_shortwave_flux_in_air,
                    LongWave = surface_downwelling_longwave_flux_in_air,
                    RelHum = relative_humidity,
                    Rain = precipitation_flux,
                    ensemble = parameter,
                    time = datetime) |>
      dplyr::mutate(AirTemp = AirTemp - 273.15,
                    RelHum = RelHum * 100,
                    RelHum = ifelse(RelHum > 100, 100, RelHum),
                    Rain = ifelse(use_ler_vars, Rain * (60 * 60), Rain * (60 * 60 * 24)/1000),
                    Snow = 0.0) |>
      dplyr::mutate_at(dplyr::vars(all_of(c("AirTemp", "ShortWave","LongWave","RelHum","WindSpeed"))), list(~round(., 2))) |>
      dplyr::mutate(Rain = round(Rain, 5),
                    time = strftime(time, format="%Y-%m-%d %H:%M", tz = "UTC")) |>
      dplyr::select(ensemble, time, AirTemp,ShortWave, LongWave, RelHum, WindSpeed,Rain, Snow) |>
      dplyr::group_by(ensemble) |>
      dplyr::slice(-dplyr::n()) |>
      dplyr::ungroup()

    ensemble_members <- unique(forecast$ensemble)

    current_filename <- purrr::map_chr(ensemble_members, function(ens, out_dir, forecast, hist_met){

      if("ensemble" %in% names(hist_met)){
        hist_met <- hist_met |>
          dplyr::filter(ensemble == ens) |>
          dplyr::select(-ensemble)
      }

      df <- forecast |>
        dplyr::filter(ensemble == ens) |>
        dplyr::select(-ensemble) |>
        dplyr::bind_rows(hist_met) |>
        dplyr::arrange(time)

      if(max(forecast$time) < strftime(end_datetime - lubridate::hours(1), format="%Y-%m-%d %H:%M", tz = "UTC")){
        stop(paste0("Weather forecasts do not cover full forecast horizon. Current max time: ", max(forecast$time), " ; Requested max time: ", strftime(end_datetime - lubridate::hours(1), format="%Y-%m-%d %H:%M", tz = "UTC")))
      }


      if(use_ler_vars){

        df <- df |>
          dplyr::rename(datetime = time,
                        Air_Temperature_celsius = AirTemp,
                        Shortwave_Radiation_Downwelling_wattPerMeterSquared = ShortWave,
                        Longwave_Radiation_Downwelling_wattPerMeterSquared = LongWave,
                        Relative_Humidity_percent = RelHum,
                        Ten_Meter_Elevation_Wind_Speed_meterPerSecond = WindSpeed,
                        Precipitation_millimeterPerHour = Rain,
                        Snowfall_millimeterPerHour = Snow)
      }

      # check for bad data
      missing_data_check(df)

      fn <- paste0("met_",stringr::str_pad(ens, width = 2, side = "left", pad = "0"),".csv")
      fn <- file.path(out_dir, fn)
      write.csv(df, file = fn, quote = FALSE, row.names = FALSE)
      return(fn)
    },
    out_dir = out_dir,
    forecast,
    hist_met)
  }

  return(list(filenames = current_filename))
}
