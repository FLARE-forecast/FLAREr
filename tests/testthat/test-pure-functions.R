# Tests for stateless utility functions with no external dependencies

# localization() ----

test_that("localization preserves matrix dimensions", {
  mat <- matrix(1, nrow = 4, ncol = 4)
  result <- FLAREr:::localization(mat, nstates = 2,
                                  modeled_depths = c(1, 2),
                                  localization_distance = 1)
  expect_equal(dim(result), c(4L, 4L))
})

test_that("localization with Inf distance returns input unchanged", {
  mat <- matrix(seq_len(16), nrow = 4, ncol = 4) * 1.0
  result <- FLAREr:::localization(mat, nstates = 2,
                                  modeled_depths = c(1, 2),
                                  localization_distance = Inf)
  expect_equal(result, mat)
})

test_that("localization element values are in (0, 1]", {
  mat <- matrix(1, nrow = 6, ncol = 6)
  result <- FLAREr:::localization(mat, nstates = 2,
                                  modeled_depths = c(1, 2, 3),
                                  localization_distance = 5)
  expect_true(all(result > 0))
  expect_true(all(result <= 1))
})

test_that("localization with num_single_states: appended row and col are all ones", {
  # mat must include the extra single-state rows/cols that will be appended.
  # nstates=2, ndepths=2, num_single_states=1 → mat is 5×5
  mat <- matrix(1, nrow = 5, ncol = 5)
  result <- FLAREr:::localization(mat, nstates = 2,
                                  modeled_depths = c(1, 2),
                                  localization_distance = 1,
                                  num_single_states = 1)
  expect_equal(dim(result), c(5L, 5L))
  expect_equal(unname(result[5, ]), rep(1, 5))
  expect_equal(unname(result[, 5]), rep(1, 5))
})


# missing_data_check() ----

test_that("missing_data_check passes clean data without error", {
  df <- data.frame(a = 1:5, b = c(1.0, 2.0, 3.0, 4.0, 5.0))
  expect_no_error(FLAREr:::missing_data_check(df))
})

test_that("missing_data_check stops on Inf with informative message", {
  df <- data.frame(a = c(1.0, Inf, 3.0), b = c(1.0, 2.0, 3.0))
  expect_error(FLAREr:::missing_data_check(df), regexp = "Inf values")
})

test_that("missing_data_check stops on NA", {
  df <- data.frame(a = c(1.0, NA, 3.0), b = c(1.0, 2.0, 3.0))
  expect_error(FLAREr:::missing_data_check(df), regexp = "NA or NaN")
})

test_that("missing_data_check stops on NaN", {
  df <- data.frame(a = c(1.0, NaN, 3.0), b = c(1.0, 2.0, 3.0))
  expect_error(FLAREr:::missing_data_check(df), regexp = "NA or NaN")
})


# build_R_matrix() ----

test_that("build_R_matrix returns a square diagonal matrix", {
  psi     <- c(0.1, 0.2, 0.3, 0.4)
  z_index <- c(1L, 2L, 3L, 4L)
  R <- FLAREr:::build_R_matrix(psi, z_index)
  expect_true(is.matrix(R))
  expect_equal(dim(R), c(4L, 4L))
  expect_equal(diag(R), psi^2)
  expect_true(all(R[upper.tri(R)] == 0))
  expect_true(all(R[lower.tri(R)] == 0))
})

test_that("build_R_matrix selects subset via z_index", {
  psi     <- c(0.1, 0.2, 0.3, 0.4)
  z_index <- c(2L, 4L)
  R <- FLAREr:::build_R_matrix(psi, z_index)
  expect_equal(dim(R), c(2L, 2L))
  expect_equal(diag(R), c(0.2^2, 0.4^2))
})


# create_filenames() ----

test_that("create_filenames returns a named list of 4 elements", {
  full_time <- seq(lubridate::as_datetime("2024-01-01"),
                   lubridate::as_datetime("2024-01-10"), by = "1 day")
  config <- list(run_config = list(sim_name = "test"),
                 location   = list(site_id  = "fcre"))
  result <- FLAREr:::create_filenames(full_time, hist_days = 4,
                                      forecast_days = 5, config = config)
  expect_length(result, 4L)
  expect_named(result,
               c("save_file_name", "save_file_name_short",
                 "forecast_iteration_id", "time_of_forecast"),
               ignore.order = TRUE)
})

test_that("create_filenames short name contains site_id", {
  full_time <- seq(lubridate::as_datetime("2024-01-01"),
                   lubridate::as_datetime("2024-01-10"), by = "1 day")
  config <- list(run_config = list(sim_name = "test"),
                 location   = list(site_id  = "fcre"))
  result <- FLAREr:::create_filenames(full_time, hist_days = 4,
                                      forecast_days = 5, config = config)
  expect_true(grepl("fcre", result$save_file_name_short))
})

test_that("create_filenames normal run does not add _spinup suffix", {
  full_time <- seq(lubridate::as_datetime("2024-01-01"),
                   lubridate::as_datetime("2024-01-10"), by = "1 day")
  config <- list(run_config = list(sim_name = "test"),
                 location   = list(site_id  = "fcre"))
  result <- FLAREr:::create_filenames(full_time, hist_days = 4,
                                      forecast_days = 5, config = config)
  expect_false(grepl("_spinup", result$save_file_name_short))
})

test_that("create_filenames forecast_iteration_id matches YYYYMMDDTHHmmss", {
  full_time <- seq(lubridate::as_datetime("2024-01-01"),
                   lubridate::as_datetime("2024-01-10"), by = "1 day")
  config <- list(run_config = list(sim_name = "test"),
                 location   = list(site_id  = "fcre"))
  result <- FLAREr:::create_filenames(full_time, hist_days = 4,
                                      forecast_days = 5, config = config)
  expect_true(grepl("^[0-9]{8}T[0-9]{6}$", result$forecast_iteration_id))
})
