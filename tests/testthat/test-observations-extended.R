# Extended edge-case tests for observation processing functions.

# Shared setup ----
.obs_setup <- function() {
  dir            <- normalizePath(tempdir(), winslash = "/")
  lake_directory <- file.path(dir, "extdata")
  file.copy(system.file("extdata", package = "FLAREr"), dir, recursive = TRUE)
  config <- FLAREr:::set_up_simulation("configure_run.yml", lake_directory,
                                       config_set_name = "default")
  obs_config <- readr::read_csv(
    file.path(config$file_path$configuration_directory,
              config$model_settings$obs_config_file),
    col_types = readr::cols()
  )
  list(config = config, obs_config = obs_config,
       lake_directory = lake_directory)
}


# create_obs_matrix() ----

test_that("create_obs_matrix returns all-NA array when no data matches configured variables", {
  s <- .obs_setup()

  # One row with an unrecognised variable so the left_join finds no matches,
  # but the datetime column is typed correctly for the join (parsed by readr).
  config   <- s$config
  start_dt <- lubridate::as_datetime(config$run_config$start_datetime)
  no_match_file <- tempfile(fileext = ".csv")
  readr::write_csv(
    data.frame(
      datetime    = format(start_dt, "%Y-%m-%dT%H:%M:%SZ"),
      site_id     = "fcre",
      depth       = 1.0,
      observation = 99.9,
      variable    = "__no_match__"   # not in obs_config$target_variable
    ),
    no_match_file
  )

  result <- FLAREr:::create_obs_matrix(no_match_file, s$obs_config, s$config)
  expect_true(all(is.na(result)))
})

test_that("create_obs_matrix has correct [nobs_types, ndays, ndepths] dimensions", {
  s <- .obs_setup()

  config   <- s$config
  start_dt <- lubridate::as_datetime(config$run_config$start_datetime)
  no_match_file <- tempfile(fileext = ".csv")
  readr::write_csv(
    data.frame(
      datetime    = format(start_dt, "%Y-%m-%dT%H:%M:%SZ"),
      site_id     = "fcre",
      depth       = 1.0,
      observation = 99.9,
      variable    = "__no_match__"
    ),
    no_match_file
  )

  result <- FLAREr:::create_obs_matrix(no_match_file, s$obs_config, s$config)
  config <- s$config

  start_datetime          <- lubridate::as_datetime(config$run_config$start_datetime)
  forecast_start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime)
  end_datetime            <- forecast_start_datetime +
    lubridate::days(config$run_config$forecast_horizon)
  full_time               <- seq(start_datetime, end_datetime, by = "1 day")

  n_obs_types <- sum(s$obs_config$multi_depth == 1, na.rm = TRUE)
  n_days      <- length(full_time)
  n_depths    <- length(config$model_settings$modeled_depths)

  expect_equal(dim(result), c(n_obs_types, n_days, n_depths))
})

test_that("create_obs_matrix masks observations after forecast_start_datetime as NA", {
  s <- .obs_setup()

  config    <- s$config
  start_dt  <- lubridate::as_datetime(config$run_config$start_datetime)
  fcast_dt  <- lubridate::as_datetime(config$run_config$forecast_start_datetime)
  end_dt    <- fcast_dt + lubridate::days(config$run_config$forecast_horizon)
  full_time <- seq(start_dt, end_dt, by = "1 day")

  first_depth <- config$model_settings$modeled_depths[1]
  # An observation at t = fcast_dt + 1 day (strictly inside the forecast window)
  forecast_day <- fcast_dt + lubridate::days(1)

  obs_file <- tempfile(fileext = ".csv")
  readr::write_csv(
    data.frame(
      datetime    = format(forecast_day, "%Y-%m-%dT%H:%M:%SZ"),
      site_id     = "fcre",
      depth       = first_depth,
      observation = 99.9,      # distinctive value that should be masked
      variable    = "temperature"
    ),
    obs_file
  )

  result <- FLAREr:::create_obs_matrix(obs_file, s$obs_config, s$config)

  forecast_time_indices <- which(full_time > fcast_dt)
  # All slots in the forecast window must be NA
  expect_true(all(is.na(result[1, forecast_time_indices, ])))
})


# create_obs_non_vertical() ----

test_that("create_obs_non_vertical returns obs_secchi$obs as all-NA when no secchi data", {
  skip("non-vertical observation assimilation is not included in this build of FLAREr")
  s <- .obs_setup()

  config <- s$config
  start_dt  <- lubridate::as_datetime(config$run_config$start_datetime)
  fcast_dt  <- lubridate::as_datetime(config$run_config$forecast_start_datetime)
  end_dt    <- fcast_dt + lubridate::days(config$run_config$forecast_horizon)

  # File contains only temperature rows — no secchi
  obs_file <- tempfile(fileext = ".csv")
  readr::write_csv(
    data.frame(
      datetime    = format(start_dt, "%Y-%m-%dT%H:%M:%SZ"),
      site_id     = "fcre",
      depth       = 1.0,
      observation = 20.0,
      variable    = "temperature"
    ),
    obs_file
  )

  result <- FLAREr:::create_obs_non_vertical(
    cleaned_observations_file_long = obs_file,
    obs_config                     = s$obs_config,
    start_datetime                 = start_dt,
    end_datetime                   = end_dt,
    forecast_start_datetime        = fcast_dt,
    forecast_horizon               = config$run_config$forecast_horizon
  )

  # Secchi is in obs_config (multi_depth = 0), but no secchi data in file
  # → result$secchi should exist (secchi IS in obs_config) but obs should be all NA
  expect_false(is.null(result$secchi))
  expect_true(all(is.na(result$secchi$obs)))
})

test_that("create_obs_non_vertical returns obs_depth as NULL when depth not in obs_config", {
  skip("non-vertical observation assimilation is not included in this build of FLAREr")
  s <- .obs_setup()

  config <- s$config
  start_dt <- lubridate::as_datetime(config$run_config$start_datetime)
  fcast_dt <- lubridate::as_datetime(config$run_config$forecast_start_datetime)
  end_dt   <- fcast_dt + lubridate::days(config$run_config$forecast_horizon)

  obs_file <- tempfile(fileext = ".csv")
  readr::write_csv(
    data.frame(datetime = character(), site_id = character(),
               depth = numeric(), observation = numeric(),
               variable = character()),
    obs_file
  )

  result <- FLAREr:::create_obs_non_vertical(
    cleaned_observations_file_long = obs_file,
    obs_config                     = s$obs_config,
    start_datetime                 = start_dt,
    end_datetime                   = end_dt,
    forecast_start_datetime        = fcast_dt,
    forecast_horizon               = config$run_config$forecast_horizon
  )

  # The default obs_config does not contain a 'depth' state; obs_depth should be NULL
  expect_null(result$obs_depth)
})

test_that("create_obs_non_vertical defaults assimilate to 1 when column absent", {
  skip("non-vertical observation assimilation is not included in this build of FLAREr")
  s <- .obs_setup()

  config   <- s$config
  start_dt <- lubridate::as_datetime(config$run_config$start_datetime)
  fcast_dt <- lubridate::as_datetime(config$run_config$forecast_start_datetime)
  end_dt   <- fcast_dt + lubridate::days(config$run_config$forecast_horizon)

  obs_config <- s$obs_config
  obs_config$assimilate <- NULL  # simulate a config that predates the column

  obs_file <- tempfile(fileext = ".csv")
  readr::write_csv(
    data.frame(datetime = character(), site_id = character(),
               depth = numeric(), observation = numeric(),
               variable = character()),
    obs_file
  )

  result <- FLAREr:::create_obs_non_vertical(
    cleaned_observations_file_long = obs_file,
    obs_config                     = obs_config,
    start_datetime                 = start_dt,
    end_datetime                   = end_dt,
    forecast_start_datetime        = fcast_dt,
    forecast_horizon               = config$run_config$forecast_horizon
  )

  expect_false(is.null(result$secchi))
  expect_equal(result$secchi$assimilate, 1)
})

test_that("create_obs_non_vertical passes through assimilate = 0", {
  skip("non-vertical observation assimilation is not included in this build of FLAREr")
  s <- .obs_setup()

  config   <- s$config
  start_dt <- lubridate::as_datetime(config$run_config$start_datetime)
  fcast_dt <- lubridate::as_datetime(config$run_config$forecast_start_datetime)
  end_dt   <- fcast_dt + lubridate::days(config$run_config$forecast_horizon)

  obs_config <- s$obs_config
  obs_config$assimilate <- 1
  obs_config$assimilate[trimws(obs_config$state_names_obs) == "secchi"] <- 0

  obs_file <- tempfile(fileext = ".csv")
  readr::write_csv(
    data.frame(datetime = character(), site_id = character(),
               depth = numeric(), observation = numeric(),
               variable = character()),
    obs_file
  )

  result <- FLAREr:::create_obs_non_vertical(
    cleaned_observations_file_long = obs_file,
    obs_config                     = obs_config,
    start_datetime                 = start_dt,
    end_datetime                   = end_dt,
    forecast_start_datetime        = fcast_dt,
    forecast_horizon               = config$run_config$forecast_horizon
  )

  expect_equal(result$secchi$assimilate, 0)
})
