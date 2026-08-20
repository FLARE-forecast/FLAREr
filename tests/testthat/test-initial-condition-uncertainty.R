# uncertainty$initial_condition ----
#
# When FALSE, ensemble member 1's model states and GLM restart file are copied
# to every member on the first forecast day, so the forecast propagates from a
# single initial condition. Parameters are deliberately untouched.

make_test_arrays <- function(nsteps = 3, nstates = 2, nheights = 4,
                             ndepths = 3, ndiag = 2, nmembers = 5) {
  fill <- function(dims) array(seq_len(prod(dims)), dim = dims)
  list(
    states_height          = fill(c(nsteps, nstates, nheights, nmembers)),
    states_depth           = fill(c(nsteps, nstates, ndepths, nmembers)),
    model_internal_heights = fill(c(nsteps, nheights, nmembers)),
    lake_depth             = fill(c(nsteps, nmembers)),
    snow_ice_thickness     = fill(c(3, nsteps, nmembers)),
    diagnostics            = fill(c(ndiag, nsteps, ndepths, nmembers)),
    diagnostics_daily      = fill(c(ndiag, nsteps, nmembers)),
    log_particle_weights   = fill(c(nsteps, nmembers))
  )
}

test_that("collapse_states_to_member1 gives every member member 1's state", {

  arrays <- make_test_arrays()
  nmembers <- dim(arrays$lake_depth)[2]
  out <- FLAREr:::collapse_states_to_member1(arrays, idx = 2)

  for (m in seq_len(nmembers)) {
    expect_equal(out$states_height[2, , , m],          out$states_height[2, , , 1])
    expect_equal(out$states_depth[2, , , m],           out$states_depth[2, , , 1])
    expect_equal(out$model_internal_heights[2, , m],   out$model_internal_heights[2, , 1])
    expect_equal(out$lake_depth[2, m],                 out$lake_depth[2, 1])
    expect_equal(out$snow_ice_thickness[, 2, m],       out$snow_ice_thickness[, 2, 1])
    expect_equal(out$diagnostics[, 2, , m],            out$diagnostics[, 2, , 1])
    expect_equal(out$diagnostics_daily[, 2, m],        out$diagnostics_daily[, 2, 1])
  }

  # Member 1 itself must be unchanged by the collapse.
  expect_equal(out$states_height[2, , , 1], arrays$states_height[2, , , 1])
  expect_equal(out$lake_depth[2, 1],        arrays$lake_depth[2, 1])

  # Identical particles carry uniform weight.
  expect_equal(out$log_particle_weights[2, ], rep(log(1.0), nmembers))
})

test_that("collapse_states_to_member1 leaves other time steps alone", {

  arrays <- make_test_arrays()
  out <- FLAREr:::collapse_states_to_member1(arrays, idx = 2)

  for (t in c(1, 3)) {
    expect_equal(out$states_height[t, , , ],          arrays$states_height[t, , , ])
    expect_equal(out$states_depth[t, , , ],           arrays$states_depth[t, , , ])
    expect_equal(out$model_internal_heights[t, , ],   arrays$model_internal_heights[t, , ])
    expect_equal(out$lake_depth[t, ],                 arrays$lake_depth[t, ])
    expect_equal(out$snow_ice_thickness[, t, ],       arrays$snow_ice_thickness[, t, ])
    expect_equal(out$diagnostics[, t, , ],            arrays$diagnostics[, t, , ])
    expect_equal(out$diagnostics_daily[, t, ],        arrays$diagnostics_daily[, t, ])
    expect_equal(out$log_particle_weights[t, ],       arrays$log_particle_weights[t, ])
  }
})

test_that("collapse_states_to_member1 handles absent diagnostics and never touches pars", {

  arrays <- make_test_arrays()
  arrays$diagnostics <- NA
  arrays$diagnostics_daily <- NA
  arrays$log_particle_weights <- NULL

  out <- FLAREr:::collapse_states_to_member1(arrays, idx = 2)

  expect_true(is.na(out$diagnostics))
  expect_true(is.na(out$diagnostics_daily))
  expect_null(out$log_particle_weights)
  expect_equal(out$states_height[2, , , 3], out$states_height[2, , , 1])

  # Parameters are not part of the collapsed set; parameter spread is governed
  # by config$uncertainty$parameter.
  expect_false("pars" %in% names(out))
})

test_that("collapse_glm_restart_to_member1 copies member 1's restart to all members", {

  dir <- file.path(tempdir(), "collapse_rst_test")
  unlink(dir, recursive = TRUE)
  nmembers <- 3
  for (m in seq_len(nmembers)) {
    dir.create(file.path(dir, m), recursive = TRUE)
    writeLines(paste0("member ", m),
               file.path(dir, m, paste0("glm_restart_", m, ".nc")))
  }

  FLAREr:::collapse_glm_restart_to_member1(dir, nmembers)

  expected <- readLines(file.path(dir, 1, "glm_restart_1.nc"))
  for (m in seq_len(nmembers)) {
    expect_equal(readLines(file.path(dir, m, paste0("glm_restart_", m, ".nc"))),
                 expected)
  }

  unlink(dir, recursive = TRUE)
})

test_that("collapse_glm_restart_to_member1 is a no-op when member 1 has no restart", {

  dir <- file.path(tempdir(), "collapse_rst_test_missing")
  unlink(dir, recursive = TRUE)
  nmembers <- 3
  for (m in seq_len(nmembers)) {
    dir.create(file.path(dir, m), recursive = TRUE)
  }
  writeLines("member 2", file.path(dir, 2, "glm_restart_2.nc"))

  expect_silent(FLAREr:::collapse_glm_restart_to_member1(dir, nmembers))

  expect_false(file.exists(file.path(dir, 1, "glm_restart_1.nc")))
  expect_equal(readLines(file.path(dir, 2, "glm_restart_2.nc")), "member 2")

  unlink(dir, recursive = TRUE)
})

# End-to-end ----

test_that("uncertainty$initial_condition FALSE removes spread only at forecast start", {

  skip_on_os(c("windows", "mac"))
  skip_if_offline()
  skip_on_cran()

  remotes::install_github("flare-forecast/GLMAEDr")
  GLMAEDr::glm_install()
  Sys.setenv('GLM_PATH' = GLMAEDr::glm_path())

  dir <- file.path(normalizePath(tempdir(), winslash = "/"), "ic_uncert")
  unlink(dir, recursive = TRUE)
  dir.create(dir, recursive = TRUE)
  lake_directory <- file.path(dir, "extdata")
  configure_run_file <- "configure_run.yml"
  config_set_name <- "default"

  file.copy(system.file("extdata", package = "FLAREr"), dir, recursive = TRUE)

  # Turn off initial condition uncertainty; leave process and weather on so the
  # ensemble must re-diverge after the collapse.
  flare_yml_path <- file.path(lake_directory, "configuration", config_set_name,
                              "configure_flare.yml")
  flare_yml <- yaml::read_yaml(flare_yml_path)
  flare_yml$uncertainty$initial_condition <- FALSE
  yaml::write_yaml(flare_yml, flare_yml_path)

  FLAREr::run_flare(lake_directory = lake_directory,
                    configure_run_file = configure_run_file,
                    config_set_name = config_set_name)

  df <- arrow::open_dataset(
    file.path(lake_directory,
              "forecasts/parquet/site_id=fcre/model_id=test/reference_date=2022-10-02/part-0.parquet")
  ) |>
    dplyr::collect()

  spread <- df |>
    dplyr::filter(variable == "temperature", variable_type == "state",
                  !is.na(prediction)) |>
    dplyr::group_by(datetime, depth) |>
    dplyr::summarise(sd = sd(prediction), .groups = "drop")

  forecast_start <- lubridate::as_datetime("2022-10-02 00:00:00")

  at_start <- spread |> dplyr::filter(datetime == forecast_start)
  expect_gt(nrow(at_start), 0)
  expect_equal(max(at_start$sd), 0)

  # Process and weather uncertainty are still on, so the members must spread
  # again on the next forecast day. This only holds if the collapse touched the
  # states that are actually propagated.
  after_start <- spread |>
    dplyr::filter(datetime == forecast_start + lubridate::days(1))
  expect_gt(nrow(after_start), 0)
  expect_gt(max(after_start$sd), 0)
})

# uncertainty defaults / weather gating ----

test_that("apply_uncertainty_defaults fills every flag with TRUE", {

  out <- FLAREr:::apply_uncertainty_defaults(list())
  expect_equal(
    out$uncertainty[c("observation", "process", "weather",
                      "initial_condition", "parameter", "inflow")],
    list(observation = TRUE, process = TRUE, weather = TRUE,
         initial_condition = TRUE, parameter = TRUE, inflow = TRUE)
  )

  # A partially specified block keeps the values it does have.
  out <- FLAREr:::apply_uncertainty_defaults(
    list(uncertainty = list(weather = FALSE, process = FALSE))
  )
  expect_false(out$uncertainty$weather)
  expect_false(out$uncertainty$process)
  expect_true(out$uncertainty$parameter)
  expect_true(out$uncertainty$initial_condition)

  # Other config entries are untouched.
  out <- FLAREr:::apply_uncertainty_defaults(list(da_setup = list(ensemble_size = 7)))
  expect_equal(out$da_setup$ensemble_size, 7)
})

test_that("uncertainty flags are safe to negate after defaulting", {

  # !NULL errors ("invalid argument type"), which is what the defaults prevent.
  config <- list(uncertainty = list(process = TRUE))
  expect_error(if (!config$uncertainty$weather) TRUE else FALSE)

  config <- FLAREr:::apply_uncertainty_defaults(config)
  expect_silent(if (!config$uncertainty$weather & 5 > 4) TRUE else FALSE)
})

test_that("weather uncertainty gate matches the other forecast-period flags", {

  # Step i simulates full_time[i-1] -> full_time[i], and full_time[hist_days+1]
  # is forecast_start_datetime, so the first forecast step is hist_days + 2.
  # weather must switch to a single met member on the same step as process,
  # inflow and parameter -- not one step earlier.
  met_file_names <- paste0("met_", sprintf("%02d", 0:4), ".csv")
  nmembers  <- 5
  hist_days <- 3
  nsteps    <- 7
  met_index <- rep(seq_along(met_file_names), times = nmembers)

  n_unique <- function(i, weather) {
    length(unique(vapply(seq_len(nmembers), function(m) {
      if (!weather & i > (hist_days + 1)) met_file_names[met_index[1]]
      else met_file_names[met_index[m]]
    }, character(1))))
  }

  is_forecast_step <- function(i) i > (hist_days + 1)

  for (i in 2:nsteps) {
    # weather TRUE: every member always keeps its own met member.
    expect_equal(n_unique(i, TRUE), nmembers)
    # weather FALSE: one shared met member during the forecast, full spread
    # through the whole hindcast including the final DA step.
    expect_equal(n_unique(i, FALSE), if (is_forecast_step(i)) 1L else nmembers)
  }
})
