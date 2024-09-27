#' @title Generate GLM ready met files from open meteo
#' @details Generate GLM ready met files from open meteo
#' @param out_dir path to where the GLM formatted met file will be written
#' @param start_datetime start date of weather data
#' @param end_datetime end date of weather data
#' @param forecast_start_datetime start datetime of forecast
#' @param forecast_horizon number of days in the future
#' @param latitude latitude
#' @param longitude longitude (west is negative)
#' @param site_id site code
#' @param openmeteo_api type of weather data or forecast (ensemble_forecast, season, climate, historical)
#' @param model forecast model
#' @param use_archive Boolen (default = FALSE); TRUE = use forecasts stored on s3 bucket, FALSE = use open-meteo download directly
#' @param bucket s3 bucket for archive
#' @param endpoint s3 endpoint for archive
#' @keywords internal
#'
#' @return list of meteorology file names
create_met_files_openmet <- function(out_dir,
                                     start_datetime,
                                     end_datetime = NA,
                                     forecast_start_datetime = NA,
                                     forecast_horizon = 0,
                                     latitude,
                                     longitude,
                                     site_id,
                                     openmeteo_api = "ensemble",
                                     model = NULL,
                                     use_archive = FALSE,
                                     bucket = NULL,
                                     endpoint = NULL){

  if (!requireNamespace("ropenmeteo", quietly = TRUE)) {
    stop("Package ropenmeteo needed.")
  }


  if(lubridate::as_date(forecast_start_datetime) != Sys.Date() & !use_archive){
    warning(paste0("forecast_start_datetime needs to be current day (", Sys.Date(),") when using the real-time openmeteo"))
  }

  if(openmeteo_api == "seasonal"){

    if(use_archive){

      if(is.null(bucket)) warning("missing s3 bucket for config$s3$drivers")
      if(is.null(endpoint)) warning("missing s3 endpoint for config$s3$drivers")

      bucket <- file.path(bucket,
                          "seasonal_forecast",
                          "model_id=cfs",
                          paste0("reference_date=", lubridate::as_date(forecast_start_datetime)),
                          paste0("site_id=", site_id))

      s3 <- arrow::s3_bucket(bucket = bucket, endpoint_override = endpoint, anonymous = TRUE)

      df <- arrow::open_dataset(s3) |>
        dplyr::collect() |>
        mutate(model_id = "cfs",
               site_id = site_id)

    }else{

      df <- ropenmeteo::get_seasonal_forecast(
        latitude = latitude,
        longitude = longitude,
        site_id = site_id,
        forecast_days = forecast_horizon,
        past_days = as.numeric(forecast_start_datetime - start_datetime),
        variables = ropenmeteo::glm_variables(product = "seasonal_forecast",
                                              time_step = "6hourly"))
    }


    max_datetime_table <- df |>
      na.omit() |>
      summarize(max_datetime = max(datetime), .by = ensemble) |>
      filter(ensemble == "02") |>
      pull(max_datetime)


    if(forecast_start_datetime + lubridate::days(forecast_horizon) > max_datetime_table){
      df <- df |>
        dplyr::filter(ensemble == "01")
    }

    df |>
      ropenmeteo::six_hourly_to_hourly(latitude = latitude, longitude = longitude, use_solar_geom = TRUE) |>
      ropenmeteo::add_longwave() |>
      ropenmeteo::write_glm_format(path = out_dir)

  }else if(openmeteo_api == "ensemble_forecast"){

    if(is.null(model)) model <- "gfs_seamless"

    if(use_archive){

      bucket <- file.path(bucket,
                          "ensemble_forecast",
                          paste0("model_id=",model),
                          paste0("reference_date=", lubridate::as_date(forecast_start_datetime)),
                          paste0("site_id=", site_id))

      s3 <- arrow::s3_bucket(bucket = bucket, endpoint_override = endpoint, anonymous = TRUE)

      df <- arrow::open_dataset(s3) |>
        dplyr::collect() |>
        mutate(model_id = model,
               site_id = site_id)

    }else{

      df <- ropenmeteo::get_ensemble_forecast(
        latitude = latitude,
        longitude = longitude,
        site_id = site_id,
        forecast_days = forecast_horizon,
        past_days = as.numeric(forecast_start_datetime - start_datetime),
        model = model,
        variables = ropenmeteo::glm_variables(product = "ensemble_forecast",
                                              time_step = "hourly"))
    }

    if(model == "ecmwf_ifs04"){

      max_datetime_table <- df |>
        na.omit() |>
        summarize(max_datetime = max(datetime), .by = ensemble) |>
        filter(ensemble == "00") |>
        pull("max_datetime")

      if(use_archive){

        bucket <- file.path(bucket,
                            "ensemble_forecast",
                            paste0("model_id=gfs_seamless"),
                            paste0("reference_date=", lubridate::as_date(forecast_start_datetime)),
                            paste0("site_id=", site_id))

        s3 <- arrow::s3_bucket(bucket = bucket, endpoint_override = endpoint, anonymous = TRUE)
        shortwave_df <- arrow::open_dataset(s3) |>
          dplyr::filter(variable == "shortwave_radiation") |>
          dplyr::collect() |>
          mutate(model_id = "ecmwf_ifs04",
                 site_id = site_id)

      }else{

        shortwave_df <-  ropenmeteo::get_ensemble_forecast(
          latitude = latitude,
          longitude = longitude,
          site_id = site_id,
          forecast_days = forecast_horizon,
          past_days = as.numeric(forecast_start_datetime - start_datetime),
          model = "gfs_seamless",
          variables = "shortwave_radiation") |>
          mutate(model_id = "ecmwf_ifs04")

      }

      shortwave_df1 <- shortwave_df |>
        mutate(ensemble = as.numeric(ensemble) + 31,
               ensemble = as.character(ensemble))

      shortwave_df <- dplyr::bind_rows(shortwave_df, shortwave_df1) |>
        filter(as.numeric(ensemble) <= max(as.numeric(df$ensemble)))

      df <- dplyr::bind_rows(df, shortwave_df) |>
        filter(datetime <= max_datetime_table)
    }

    df |>
      ropenmeteo::add_longwave() |>
      ropenmeteo::write_glm_format(path = out_dir)

  }else if(openmeteo_api == "historical"){

    if(is.na(end_datetime)){
      end_datetime <- lubridate::as_date(lubridate::as_datetime(forecast_start_datetime) + lubridate::days(forecast_horizon + 1))
    }

    #warning("Not tested")

    ropenmeteo::get_historical_weather(
      latitude = latitude,
      longitude = longitude,
      site_id = site_id,
      start_date = start_datetime,
      end_date = end_datetime,
      variables = ropenmeteo::glm_variables(product = "historical",
                                            time_step = "hourly")) |>
      ropenmeteo::add_longwave() |>
      ropenmeteo::write_glm_format(path = out_dir)

  }else if(openmeteo_api == "climate"){

    if(is.null(model)) warning("no model provided for climate projection and no default is assumed")

    ropenmeteo::get_climate_projections(
      latitude = latitude,
      longitude = longitude,
      site_id = site_id,
      start_date = start_datetime,
      end_date = forecast_start_datetime + lubridate::days(forecast_horizon),
      model = model,
      variables = ropenmeteo::glm_variables(product = "climate_projection",
                                            time_step = "daily")) |>
      ropenmeteo::daily_to_hourly(latitude = latitude, longitude = longitude) |>
      ropenmeteo::add_longwave() |>
      ropenmeteo::write_glm_format(path = out_dir)
  }

  current_filenames <- list.files(path = out_dir, pattern = paste0("met_",model),full.names = TRUE)

  return(list(filenames = current_filenames))
}
