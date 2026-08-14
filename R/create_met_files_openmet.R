##' @title Generate GLM ready met files from the Open-Meteo ensemble API
##' @details Downloads a live ensemble weather forecast from the Open-Meteo
##'   ensemble API (<https://open-meteo.com/en/docs/ensemble-api>) using the
##'   `ropenmeteo` package and writes one GLM-formatted meteorology CSV per
##'   ensemble member. Only the ensemble-forecast API is supported (no
##'   seasonal/historical/climate modes, no S3-archived alternative to the
##'   live API call). The pivot/rename step intentionally reimplements (rather
##'   than calls) `ropenmeteo::write_glm_format()` so files land directly in
##'   FLAREr's per-ensemble-member directory/filename layout and so
##'   `include_wind_direction` is supported (`write_glm_format()`'s own column
##'   selection always drops wind direction).
##' @param config list of FLARE configurations
##' @param lake_directory directory of lake configurations
##' @param met_forecast_start_datetime start datetime of met forecasts
##' @param met_start_datetime start datetime of met simulation
##' @param include_wind_direction Logical (default `FALSE`); when `TRUE`, adds
##'   a `WindDir` column to each output CSV using Open-Meteo's
##'   `wind_direction_10m` variable.
##' @param out_dir_fn Optional function of the ensemble positional index
##'   (integer, 1-based) returning the directory path for that member's met
##'   file. When non-NULL each file is written to `out_dir_fn(i)`; the
##'   returned filenames reflect those locations. When `NULL` (default) files
##'   are written to `config$file_path$execute_directory`.
##' @return list; vector of full path for the converted files
##' @export
##' @author Quinn Thomas
create_met_files_openmet <- function(config, lake_directory, met_forecast_start_datetime, met_start_datetime,
                                     include_wind_direction = FALSE, out_dir_fn = NULL){

  if (!requireNamespace("ropenmeteo", quietly = TRUE)) {
    stop("the ropenmeteo package is required when config$met$use_openmeteo is TRUE; ",
         "install it with remotes::install_github('flare-forecast/ropenmeteo')")
  }

  out_dir          <- config$file_path$execute_directory
  latitude         <- config$location$latitude
  longitude        <- config$location$longitude
  site_id          <- config$location$site_id
  nmembers         <- config$da_setup$ensemble_size
  forecast_horizon <- config$run_config$forecast_horizon

  model <- config$met$openmeteo_model
  if (is.null(model) || is.na(model)) model <- "ncep_gefs_seamless"

  start_datetime          <- lubridate::as_datetime(met_start_datetime)
  forecast_start_datetime <- lubridate::as_datetime(met_forecast_start_datetime)

  past_days <- max(0, ceiling(as.numeric(
    difftime(forecast_start_datetime, start_datetime, units = "days")
  )))

  # FLARE-specific messages for the Open-Meteo ensemble API's own hard limits
  # (get_ensemble_forecast() enforces these too, but with a less actionable message).
  if (forecast_horizon > 35) {
    stop(paste0("forecast_horizon (", forecast_horizon, " days) exceeds the Open-Meteo ",
                "ensemble API's maximum forecast_days (35). Reduce forecast_horizon or ",
                "set config$met$use_openmeteo = FALSE."))
  }
  if (past_days > 3) {
    stop(paste0("the gap between start_datetime and forecast_start_datetime (", past_days,
                " days) exceeds the Open-Meteo ensemble API's maximum past_days (3)."))
  }

  variables <- ropenmeteo::glm_variables(product = "ensemble_forecast", time_step = "hourly")
  if (include_wind_direction) variables <- c(variables, "wind_direction_10m")

  message("Downloading Open-Meteo ensemble forecast (model = ", model, ")...")

  fc <- ropenmeteo::get_ensemble_forecast(
    latitude      = latitude,
    longitude     = longitude,
    site_id       = site_id,
    forecast_days = forecast_horizon,
    past_days     = past_days,
    model         = model,
    variables     = variables
  ) |> ropenmeteo::add_longwave()

  # Shift accumulated/instantaneous-flux variables from period-end to
  # period-start labeling (mirrors ropenmeteo::write_glm_format()), then pivot
  # to one row per timestep with one column per variable.
  fc_wide <- fc |>
    dplyr::mutate(
      datetime = ifelse(
        variable %in% c("longwave_radiation", "shortwave_radiation", "precipitation"),
        datetime - lubridate::hours(1),
        datetime
      ),
      datetime = lubridate::as_datetime(datetime)
    ) |>
    dplyr::select(-unit) |>
    tidyr::pivot_wider(names_from = variable, values_from = prediction) |>
    dplyr::rename(
      LongWave  = longwave_radiation,
      ShortWave = shortwave_radiation,
      AirTemp   = temperature_2m,
      Rain      = precipitation,
      WindSpeed = wind_speed_10m,
      RelHum    = relative_humidity_2m,
      time      = datetime
    ) |>
    dplyr::mutate(Rain = Rain * 0.024, Snow = 0.0)

  if (include_wind_direction && "wind_direction_10m" %in% names(fc_wide)) {
    fc_wide <- dplyr::rename(fc_wide, WindDir = wind_direction_10m)
  }

  keep_cols <- c("time", "AirTemp", "ShortWave", "LongWave", "RelHum", "WindSpeed", "Rain", "Snow")
  if (include_wind_direction && "WindDir" %in% names(fc_wide)) keep_cols <- c(keep_cols, "WindDir")

  ensemble_ids <- sort(unique(fc_wide$ensemble))
  n_met        <- length(ensemble_ids)

  current_filename <- purrr::map_chr(seq_along(ensemble_ids), function(idx, ensemble_ids, fc_wide, keep_cols) {
    ens <- ensemble_ids[idx]
    df <- fc_wide |>
      dplyr::filter(ensemble == ens) |>
      dplyr::arrange(time) |>
      dplyr::select(dplyr::all_of(keep_cols)) |>
      dplyr::mutate(
        dplyr::across(dplyr::all_of(c("AirTemp", "ShortWave", "LongWave", "RelHum", "WindSpeed")),
                      \(x) round(x, 2)),
        Rain = round(Rain, 5),
        time = strftime(time, format = "%Y-%m-%d %H:%M", tz = "UTC")
      ) |>
      # first row's period-labeled fluxes were shifted from the previous
      # (unavailable) hour; drop it, matching ropenmeteo::write_glm_format()
      dplyr::slice(-1)

    missing_data_check(df)

    ens_out_dir <- if (!is.null(out_dir_fn)) out_dir_fn(idx) else out_dir
    dir.create(ens_out_dir, recursive = TRUE, showWarnings = FALSE)
    fn <- file.path(ens_out_dir,
                    paste0("met_", stringr::str_pad(idx, width = 2, side = "left", pad = "0"), ".csv"))
    write.csv(df, file = fn, quote = FALSE, row.names = FALSE)
    fn
  }, ensemble_ids, fc_wide, keep_cols)

  # Recycle met files when ensemble_size exceeds available met members, or
  # truncate when ensemble_size is smaller (mirrors create_met_files()).
  if (!is.null(nmembers)) {
    if (nmembers > n_met) {
      message(sprintf("ensemble_size (%d) > met members (%d): recycling met files",
                      nmembers, n_met))
      recycled <- character(nmembers - n_met)
      for (ens in seq(n_met + 1L, nmembers)) {
        src_ens  <- ((ens - 1L) %% n_met) + 1L
        dest_dir <- if (!is.null(out_dir_fn)) out_dir_fn(ens) else out_dir
        dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
        dest_fn  <- file.path(
          dest_dir,
          paste0("met_", stringr::str_pad(ens, 2L, "left", "0"), ".csv")
        )
        file.copy(current_filename[src_ens], dest_fn, overwrite = TRUE)
        recycled[ens - n_met] <- dest_fn
      }
      current_filename <- c(current_filename, recycled)
    } else if (nmembers < n_met) {
      current_filename <- current_filename[seq_len(nmembers)]
    }
  }

  return(list(filenames = current_filename))
}
