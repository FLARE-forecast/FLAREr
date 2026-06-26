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
#' @param openmeteo_api type of weather data or forecast (ensemble_forecast, seasonal, climate, historical)
#' @param model forecast model
#' @param use_archive Boolen (default = FALSE); TRUE = use forecasts stored on s3 bucket, FALSE = use open-meteo download directly
#' @param bucket s3 bucket for archive
#' @param endpoint s3 endpoint for archive
#' @param include_wind_direction Logical (default \code{FALSE}); when \code{TRUE},
#'   adds a \code{WindDir} column to each output CSV. Only supported when
#'   \code{openmeteo_api = "ensemble"} and wind direction data is available from
#'   the OpenMeteo API; ignored for other pathways.
#' @param out_dir_fn Optional function of the ensemble positional index (integer,
#'   1-based) returning the directory path for that member's met file. When
#'   non-NULL each file is moved to \code{out_dir_fn(i)} after writing; the
#'   returned filenames reflect the new locations. When NULL (default) files
#'   remain in \code{out_dir}.
#' @export
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
                                     endpoint = NULL,
                                     config = NULL,
                                     include_wind_direction = FALSE,
                                     out_dir_fn = NULL){

  if (!requireNamespace("ropenmeteo", quietly = TRUE)) {
    stop("Package ropenmeteo needed.")
  }


  if(lubridate::as_date(forecast_start_datetime) != Sys.Date() & !use_archive & openmeteo_api %in% c("seasonal","ensemble")){
    warning(paste0("forecast_start_datetime needs to be current day (", Sys.Date(),") when using the real-time openmeteo"))
  }

  if(openmeteo_api == "seasonal"){

    if(use_archive){

      if(is.null(bucket)) warning("missing s3 bucket for config$s3$drivers")
      if(is.null(endpoint)) warning("missing s3 endpoint for config$s3$drivers")

      prefix <- file.path(stringr::str_split_fixed(bucket, "/", n = 2)[2],"seasonal_forecast","model_id=ecmwf_seasonal_seamless",
                          paste0("reference_date=", lubridate::as_date(forecast_start_datetime)),
                          paste0("site_id=", site_id))

      config$s3$drivers$anonymous <- TRUE
      s3 <- flare_arrow_s3_bucket(server_name = "drivers", faasr_prefix = prefix, config = config)

      # bucket <- file.path(bucket,
      #                     "seasonal_forecast",
      #                     "model_id=cfs",
      #                     paste0("reference_date=", lubridate::as_date(forecast_start_datetime)),
      #                     paste0("site_id=", site_id))

      # s3 <- arrow::s3_bucket(bucket = bucket, endpoint_override = endpoint, anonymous = TRUE)
      df <- arrow::open_dataset(s3) |>
        dplyr::collect() |>
        mutate(model_id = "ecmwf_seasonal_seamless",
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

    df |>
      ropenmeteo::six_hourly_to_hourly(latitude = latitude, longitude = longitude, use_solar_geom = TRUE) |>
      ropenmeteo::add_longwave() |>
      ropenmeteo::write_glm_format(path = out_dir)

  }else if(openmeteo_api == "ensemble"){

    if(is.null(model)) model <- "ncep_gefs_seamless"

    if(use_archive){

      prefix <- file.path(stringr::str_split_fixed(bucket, "/", n = 2)[2],"ensemble_forecast",paste0("model_id=",model),
                          paste0("reference_date=", lubridate::as_date(forecast_start_datetime)),
                          paste0("site_id=", site_id))


      # bucket <- file.path(bucket,
      #                     "ensemble_forecast",
      #                     paste0("model_id=",model),
      #                     paste0("reference_date=", lubridate::as_date(forecast_start_datetime)),
      #                     paste0("site_id=", site_id))


      config$s3$drivers$anonymous <- TRUE
      #s3 <- arrow::s3_bucket(bucket = bucket, endpoint_override = endpoint, anonymous = TRUE)

      s3 <- flare_arrow_s3_bucket(server_name = "drivers", faasr_prefix = prefix, config = config)
      df <- arrow::open_dataset(s3) |>
        dplyr::collect() |>
        mutate(model_id = model,
               site_id = site_id)
      #message("opening dataset success in ensemble using archive")

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

    df |>
      ropenmeteo::add_longwave() |>
      ropenmeteo::write_glm_format(path = out_dir)

  }else if(openmeteo_api == "historical"){

    model <- "ERA5"

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

  current_filenames <- list.files(path = out_dir, pattern = paste0("met_"), full.names = TRUE)

  # Distribute files to per-ensemble directories if out_dir_fn is provided
  if (!is.null(out_dir_fn) && length(current_filenames) > 0) {
    current_filenames <- current_filenames[order(current_filenames)]
    new_filenames <- character(length(current_filenames))
    for (i in seq_along(current_filenames)) {
      ens_dir <- out_dir_fn(i)
      dir.create(ens_dir, recursive = TRUE, showWarnings = FALSE)
      new_fn <- file.path(ens_dir, basename(current_filenames[i]))
      file.copy(current_filenames[i], new_fn, overwrite = TRUE)
      file.remove(current_filenames[i])
      new_filenames[i] <- new_fn
    }
    current_filenames <- new_filenames
  }

  # Recycle met files when ensemble_size exceeds available met members,
  # or truncate when ensemble_size is smaller.  Mirrors FLAREr's run-time
  # met_index cycling in run_da_forecast.R but copies files on disk so
  # model backends that need a physical file per run directory can find them.
  if (!is.null(out_dir_fn) && !is.null(config) &&
      !is.null(config$da_setup$ensemble_size)) {
    n_met    <- length(current_filenames)
    nmembers <- config$da_setup$ensemble_size
    if (nmembers > n_met) {
      message(sprintf(
        "ensemble_size (%d) > met members (%d): recycling met files",
        nmembers, n_met))
      recycled <- character(nmembers - n_met)
      for (ens in seq(n_met + 1L, nmembers)) {
        src_ens  <- ((ens - 1L) %% n_met) + 1L
        dest_dir <- out_dir_fn(ens)
        dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
        dest_fn  <- file.path(
          dest_dir,
          paste0("met_", stringr::str_pad(ens - 1L, 2L, "left", "0"), ".csv")
        )
        file.copy(current_filenames[src_ens], dest_fn, overwrite = TRUE)
        recycled[ens - n_met] <- dest_fn
      }
      current_filenames <- c(current_filenames, recycled)
    } else if (nmembers < n_met) {
      current_filenames <- current_filenames[seq_len(nmembers)]
    }
  }

  return(list(filenames = current_filenames))
}
