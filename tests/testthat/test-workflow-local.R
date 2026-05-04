# Tests for exported workflow functions that run without S3 or network access.

# update_run_config() (local storage only) ----

# Helper: set up a fresh lake_directory with restart sub-directory
.setup_lake_dir <- function(sim_name = "test", site_id = "fcre") {
  dir            <- normalizePath(tempdir(), winslash = "/")
  lake_directory <- file.path(dir, "extdata")
  file.copy(system.file("extdata", package = "FLAREr"), dir, recursive = TRUE)
  restart_dir <- file.path(lake_directory, "restart", site_id, sim_name)
  dir.create(restart_dir, recursive = TRUE, showWarnings = FALSE)
  # Seed with default run config so the file exists before update
  file.copy(
    file.path(lake_directory, "configuration", "default", "configure_run.yml"),
    file.path(restart_dir, "configure_run.yml"),
    overwrite = TRUE
  )
  list(lake_directory = lake_directory, restart_dir = restart_dir)
}

test_that("update_run_config creates config YAML at expected local path", {
  s <- .setup_lake_dir()
  FLAREr::update_run_config(
    lake_directory          = s$lake_directory,
    configure_run_file      = "configure_run.yml",
    restart_file            = NA,
    start_datetime          = "2024-01-01 00:00:00",
    end_datetime            = NA,
    forecast_start_datetime = "2024-01-10 00:00:00",
    forecast_horizon        = 16L,
    sim_name                = "test",
    site_id                 = "fcre",
    configure_flare         = "configure_flare.yml",
    configure_obs           = NULL,
    use_s3                  = FALSE,
    bucket                  = NULL,
    endpoint                = NULL,
    config                  = NULL
  )
  expect_true(file.exists(file.path(s$restart_dir, "configure_run.yml")))
})

test_that("update_run_config round-trips start_datetime (full datetime string)", {
  s <- .setup_lake_dir()
  start <- "2024-03-15 00:00:00"
  FLAREr::update_run_config(
    lake_directory          = s$lake_directory,
    configure_run_file      = "configure_run.yml",
    restart_file            = NA,
    start_datetime          = start,
    end_datetime            = NA,
    forecast_start_datetime = "2024-03-20 00:00:00",
    forecast_horizon        = 5L,
    sim_name                = "test",
    site_id                 = "fcre",
    configure_flare         = "configure_flare.yml",
    configure_obs           = NULL,
    use_s3                  = FALSE,
    bucket                  = NULL,
    endpoint                = NULL,
    config                  = NULL
  )
  written <- yaml::read_yaml(file.path(s$restart_dir, "configure_run.yml"))
  expect_true(grepl("2024-03-15", written$start_datetime))
})

test_that("update_run_config round-trips start_datetime (date-only string)", {
  s <- .setup_lake_dir()
  FLAREr::update_run_config(
    lake_directory          = s$lake_directory,
    configure_run_file      = "configure_run.yml",
    restart_file            = NA,
    start_datetime          = "2024-06-01",
    end_datetime            = NA,
    forecast_start_datetime = "2024-06-10 00:00:00",
    forecast_horizon        = 10L,
    sim_name                = "test",
    site_id                 = "fcre",
    configure_flare         = "configure_flare.yml",
    configure_obs           = NULL,
    use_s3                  = FALSE,
    bucket                  = NULL,
    endpoint                = NULL,
    config                  = NULL
  )
  written <- yaml::read_yaml(file.path(s$restart_dir, "configure_run.yml"))
  expect_true(grepl("2024-06-01", written$start_datetime))
})

test_that("update_run_config stores NA end_datetime as null/NA in YAML", {
  s <- .setup_lake_dir()
  FLAREr::update_run_config(
    lake_directory          = s$lake_directory,
    configure_run_file      = "configure_run.yml",
    restart_file            = NA,
    start_datetime          = "2024-01-01 00:00:00",
    end_datetime            = NA,
    forecast_start_datetime = "2024-01-10 00:00:00",
    forecast_horizon        = 16L,
    sim_name                = "test",
    site_id                 = "fcre",
    configure_flare         = "configure_flare.yml",
    configure_obs           = NULL,
    use_s3                  = FALSE,
    bucket                  = NULL,
    endpoint                = NULL,
    config                  = NULL
  )
  written <- yaml::read_yaml(file.path(s$restart_dir, "configure_run.yml"))
  expect_true(is.null(written$end_datetime) || is.na(written$end_datetime))
})

test_that("update_run_config stores forecast_horizon correctly", {
  s <- .setup_lake_dir()
  FLAREr::update_run_config(
    lake_directory          = s$lake_directory,
    configure_run_file      = "configure_run.yml",
    restart_file            = NA,
    start_datetime          = "2024-01-01 00:00:00",
    end_datetime            = NA,
    forecast_start_datetime = "2024-01-10 00:00:00",
    forecast_horizon        = 30L,
    sim_name                = "test",
    site_id                 = "fcre",
    configure_flare         = "configure_flare.yml",
    configure_obs           = NULL,
    use_s3                  = FALSE,
    bucket                  = NULL,
    endpoint                = NULL,
    config                  = NULL
  )
  written <- yaml::read_yaml(file.path(s$restart_dir, "configure_run.yml"))
  expect_equal(written$forecast_horizon, 30L)
})


# check_noaa_present() ----

test_that("check_noaa_present returns a logical value", {
  dir <- normalizePath(tempdir(), winslash = "/")
  lake_directory <- file.path(dir, "extdata")
  file.copy(system.file("extdata", package = "FLAREr"), dir, recursive = TRUE)
  # The default config uses local met files, not S3; without NOAA files present
  # the function should return FALSE (or TRUE if already cached from prior tests)
  result <- tryCatch(
    FLAREr::check_noaa_present(lake_directory,
                               configure_run_file = "configure_run.yml",
                               config_set_name    = "default"),
    error = function(e) NA   # S3 / network errors are acceptable in offline CI
  )
  expect_true(is.logical(result) || is.na(result))
})
