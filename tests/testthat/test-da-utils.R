# Tests for data assimilation support functions using synthetic arrays

# add_process_noise() ----

test_that("add_process_noise returns a list with states_height_ens and states_depth_ens", {
  set.seed(1)
  nstates      <- 2
  ndepths      <- 5
  max_layers   <- 6
  lake_depth   <- 5.0
  modeled_depths    <- seq(1, ndepths)
  vert_decorr_length <- c(2, 2)

  heights <- lake_depth - modeled_depths          # [4, 3, 2, 1, 0]
  model_internal_heights_ens <- c(heights, NA)    # 5 valid + 1 NA

  states_height_ens <- matrix(15, nrow = nstates, ncol = max_layers)
  model_sd          <- matrix(0.5, nrow = nstates, ncol = ndepths)

  result <- FLAREr:::add_process_noise(
    states_height_ens          = states_height_ens,
    model_sd                   = model_sd,
    model_internal_heights_ens = model_internal_heights_ens,
    lake_depth_ens             = lake_depth,
    modeled_depths             = modeled_depths,
    vert_decorr_length         = vert_decorr_length,
    include_uncertainty        = TRUE
  )
  expect_type(result, "list")
  expect_named(result, c("states_height_ens", "states_depth_ens"), ignore.order = FALSE)
})

test_that("add_process_noise states_depth_ens has correct shape [nstates, ndepths]", {
  set.seed(2)
  nstates      <- 2
  ndepths      <- 4
  max_layers   <- 5
  lake_depth   <- 4.0
  modeled_depths     <- seq(1, ndepths)
  vert_decorr_length <- c(2, 2)
  heights            <- lake_depth - modeled_depths

  model_internal_heights_ens <- c(heights, NA)
  states_height_ens  <- matrix(15, nrow = nstates, ncol = max_layers)
  model_sd           <- matrix(0.5, nrow = nstates, ncol = ndepths)

  result <- FLAREr:::add_process_noise(
    states_height_ens          = states_height_ens,
    model_sd                   = model_sd,
    model_internal_heights_ens = model_internal_heights_ens,
    lake_depth_ens             = lake_depth,
    modeled_depths             = modeled_depths,
    vert_decorr_length         = vert_decorr_length
  )
  expect_equal(dim(result$states_depth_ens), c(nstates, ndepths))
})

test_that("add_process_noise with include_uncertainty=FALSE adds zero noise to state heights", {
  nstates      <- 2
  ndepths      <- 3
  max_layers   <- 4
  lake_depth   <- 3.0
  modeled_depths     <- seq(1, ndepths)
  vert_decorr_length <- c(2, 2)
  heights            <- lake_depth - modeled_depths

  model_internal_heights_ens <- c(heights, NA)
  states_height_ens_orig <- matrix(c(20, 19, 18, 17, 0.1, 0.1, 0.1, 0.1),
                                   nrow = nstates, ncol = max_layers, byrow = TRUE)
  model_sd <- matrix(0.5, nrow = nstates, ncol = ndepths)

  result <- FLAREr:::add_process_noise(
    states_height_ens          = states_height_ens_orig,
    model_sd                   = model_sd,
    model_internal_heights_ens = model_internal_heights_ens,
    lake_depth_ens             = lake_depth,
    modeled_depths             = modeled_depths,
    vert_decorr_length         = vert_decorr_length,
    include_uncertainty        = FALSE
  )
  # Heights should be unchanged for valid layers when noise is zero
  n_valid <- sum(!is.na(model_internal_heights_ens))
  expect_equal(result$states_height_ens[, seq_len(n_valid)],
               states_height_ens_orig[, seq_len(n_valid)])
})

test_that("add_process_noise clamps non-temperature states (row >= 2) to non-negative", {
  set.seed(99)
  nstates      <- 2
  ndepths      <- 3
  max_layers   <- 3
  lake_depth   <- 3.0
  modeled_depths     <- seq(1, ndepths)
  vert_decorr_length <- c(2, 2)
  heights            <- lake_depth - modeled_depths

  model_internal_heights_ens <- heights
  # Start state 2 at a large negative value
  states_height_ens <- matrix(c(20, 19, 18, -50, -50, -50),
                              nrow = nstates, ncol = ndepths, byrow = TRUE)
  # Use large SD to ensure state 2 remains negative without clamping
  model_sd <- matrix(c(0.01, 100), nrow = nstates, ncol = ndepths)

  result <- FLAREr:::add_process_noise(
    states_height_ens          = states_height_ens,
    model_sd                   = model_sd,
    model_internal_heights_ens = model_internal_heights_ens,
    lake_depth_ens             = lake_depth,
    modeled_depths             = modeled_depths,
    vert_decorr_length         = vert_decorr_length,
    include_uncertainty        = TRUE
  )
  # State 2 (row index >= 2) must be non-negative after noise
  expect_true(all(result$states_height_ens[2, seq_len(ndepths)] >= 0))
})


# propose_parameters() ----

test_that("propose_parameters returns NULL when npars is 0", {
  result <- FLAREr:::propose_parameters(
    i = 2, m = 1, pars = array(0, dim = c(1, 0, 5)),
    pars_config = data.frame(), npars = 0,
    par_fit_method = "perturb", da_method = "enkf",
    hist_days = 4, include_uncertainty = TRUE
  )
  expect_null(result)
})

test_that("propose_parameters returns par_init_mean for fixed parameters", {
  pars_config <- data.frame(
    fix_par         = 1L,
    par_init_mean   = 0.42,
    par_lowerbound  = 0.0,
    par_upperbound  = 1.0,
    perturb_par     = 0.05
  )
  pars <- array(0.5, dim = c(3, 1, 5))  # [time, npars, members]
  result <- FLAREr:::propose_parameters(
    i = 2, m = 1, pars = pars, pars_config = pars_config,
    npars = 1, par_fit_method = "perturb", da_method = "enkf",
    hist_days = 1, include_uncertainty = TRUE
  )
  expect_equal(result, 0.42)
})

test_that("propose_parameters reflects values below lower bound", {
  set.seed(7)
  pars_config <- data.frame(
    fix_par         = 0L,
    par_init_mean   = 0.5,
    par_lowerbound  = 0.0,
    par_upperbound  = 1.0,
    perturb_par     = 10.0   # large perturbation to push below bound
  )
  # Set current par close to lower bound so perturbation sends it below
  pars <- array(0.001, dim = c(3, 1, 5))
  for (m in seq_len(5)) {
    result <- FLAREr:::propose_parameters(
      i = 2, m = m, pars = pars, pars_config = pars_config,
      npars = 1, par_fit_method = "perturb", da_method = "enkf",
      hist_days = 1, include_uncertainty = TRUE
    )
    expect_gte(result[1], 0.0)
    expect_lte(result[1], 1.0)
  }
})

test_that("propose_parameters reflects values above upper bound", {
  set.seed(8)
  pars_config <- data.frame(
    fix_par         = 0L,
    par_init_mean   = 0.5,
    par_lowerbound  = 0.0,
    par_upperbound  = 1.0,
    perturb_par     = 10.0   # large perturbation to push above bound
  )
  pars <- array(0.999, dim = c(3, 1, 5))
  for (m in seq_len(5)) {
    result <- FLAREr:::propose_parameters(
      i = 2, m = m, pars = pars, pars_config = pars_config,
      npars = 1, par_fit_method = "perturb", da_method = "enkf",
      hist_days = 1, include_uncertainty = TRUE
    )
    expect_gte(result[1], 0.0)
    expect_lte(result[1], 1.0)
  }
})

test_that("propose_parameters output is always within bounds after many draws", {
  set.seed(42)
  pars_config <- data.frame(
    fix_par         = 0L,
    par_init_mean   = 0.5,
    par_lowerbound  = 0.2,
    par_upperbound  = 0.8,
    perturb_par     = 0.3
  )
  pars <- array(0.5, dim = c(3, 1, 100))
  results <- vapply(seq_len(100), function(m) {
    FLAREr:::propose_parameters(
      i = 2, m = m, pars = pars, pars_config = pars_config,
      npars = 1, par_fit_method = "perturb", da_method = "enkf",
      hist_days = 1, include_uncertainty = TRUE
    )[1]
  }, numeric(1))
  expect_true(all(results >= 0.2))
  expect_true(all(results <= 0.8))
})


# generate_states_to_obs_mapping() ----

test_that("generate_states_to_obs_mapping adds states_to_obs and states_to_obs_mapping columns", {
  dir <- file.path(normalizePath(tempdir(), winslash = "/"))
  lake_directory <- file.path(dir, "extdata")
  file.copy(system.file("extdata", package = "FLAREr"), dir, recursive = TRUE)
  config <- FLAREr:::set_up_simulation("configure_run.yml", lake_directory,
                                       config_set_name = "default")
  obs_config    <- readr::read_csv(file.path(config$file_path$configuration_directory,
                                             config$model_settings$obs_config_file),
                                   col_types = readr::cols())
  states_config <- readr::read_csv(file.path(config$file_path$configuration_directory,
                                             config$model_settings$states_config_file),
                                   col_types = readr::cols())
  result <- FLAREr:::generate_states_to_obs_mapping(states_config, obs_config)
  expect_s3_class(result, "data.frame")
  expect_true("states_to_obs_mapping" %in% names(result))
  expect_true("states_to_obs" %in% names(result))
})

test_that("generate_states_to_obs_mapping returns NA entry for a state with no matching obs", {
  obs_config <- data.frame(
    state_names_obs = "temperature",
    obs_sd          = 0.5,
    stringsAsFactors = FALSE
  )
  # Two states: one maps to temperature, one maps to nothing
  states_config <- data.frame(
    state_names         = c("temp", "ghost"),
    model_sd            = c(1.0, 0.5),
    states_to_obs_1     = c("temperature", NA),
    states_to_obs_mapping_1 = c(1L, NA),
    stringsAsFactors    = FALSE
  )
  result <- FLAREr:::generate_states_to_obs_mapping(states_config, obs_config)
  # State 2 (ghost) has no matching obs → entry should be NA
  expect_true(is.na(result$states_to_obs[[2]][1]))
})

test_that("generate_states_to_obs_mapping maps temperature state to index 1", {
  dir <- file.path(normalizePath(tempdir(), winslash = "/"))
  lake_directory <- file.path(dir, "extdata")
  file.copy(system.file("extdata", package = "FLAREr"), dir, recursive = TRUE)
  config <- FLAREr:::set_up_simulation("configure_run.yml", lake_directory,
                                       config_set_name = "default")
  obs_config    <- readr::read_csv(file.path(config$file_path$configuration_directory,
                                             config$model_settings$obs_config_file),
                                   col_types = readr::cols())
  states_config <- readr::read_csv(file.path(config$file_path$configuration_directory,
                                             config$model_settings$states_config_file),
                                   col_types = readr::cols())
  result <- FLAREr:::generate_states_to_obs_mapping(states_config, obs_config)
  expect_equal(result$states_to_obs_mapping[[1]][1], 1)
})


# initiate_model_error() ----

test_that("initiate_model_error returns matrix with correct dimensions", {
  dir <- file.path(normalizePath(tempdir(), winslash = "/"))
  lake_directory <- file.path(dir, "extdata")
  file.copy(system.file("extdata", package = "FLAREr"), dir, recursive = TRUE)
  config <- FLAREr:::set_up_simulation("configure_run.yml", lake_directory,
                                       config_set_name = "default")
  states_config <- readr::read_csv(file.path(config$file_path$configuration_directory,
                                             config$model_settings$states_config_file),
                                   col_types = readr::cols())
  model_sd <- FLAREr:::initiate_model_error(config, states_config)
  expect_equal(dim(model_sd),
               c(nrow(states_config),
                 length(config$model_settings$modeled_depths)))
})

test_that("initiate_model_error returns non-negative values", {
  dir <- file.path(normalizePath(tempdir(), winslash = "/"))
  lake_directory <- file.path(dir, "extdata")
  file.copy(system.file("extdata", package = "FLAREr"), dir, recursive = TRUE)
  config <- FLAREr:::set_up_simulation("configure_run.yml", lake_directory,
                                       config_set_name = "default")
  states_config <- readr::read_csv(file.path(config$file_path$configuration_directory,
                                             config$model_settings$states_config_file),
                                   col_types = readr::cols())
  model_sd <- FLAREr:::initiate_model_error(config, states_config)
  expect_true(all(model_sd >= 0, na.rm = TRUE))
})

test_that("initiate_model_error scalar fallback broadcasts constant value across depths", {
  dir <- file.path(normalizePath(tempdir(), winslash = "/"))
  lake_directory <- file.path(dir, "extdata")
  file.copy(system.file("extdata", package = "FLAREr"), dir, recursive = TRUE)
  config <- FLAREr:::set_up_simulation("configure_run.yml", lake_directory,
                                       config_set_name = "default")
  states_config <- readr::read_csv(file.path(config$file_path$configuration_directory,
                                             config$model_settings$states_config_file),
                                   col_types = readr::cols())
  # Remove depth-specific config to force scalar fallback
  config$model_settings$depth_model_sd_config_file <- NULL
  model_sd <- FLAREr:::initiate_model_error(config, states_config)
  # Each row should be constant (same value at all depths)
  for (s in seq_len(nrow(states_config))) {
    expect_true(all(model_sd[s, ] == model_sd[s, 1]))
    expect_equal(model_sd[s, 1], states_config$model_sd[s])
  }
})


# update_parameters_enkf() ----

test_that("update_parameters_enkf returns matrix with [npars, nmembers] shape", {
  set.seed(10)
  npars    <- 2
  nmembers <- 15
  nobs     <- 3

  pars          <- matrix(runif(npars * nmembers, 0.3, 0.7), nrow = npars)
  predicted_obs <- matrix(rnorm(nobs * nmembers, mean = 20), nrow = nobs)
  zt            <- rep(20, nobs)
  psi           <- rep(0.5, nobs)
  z_index       <- seq_len(nobs)
  pars_config   <- data.frame(
    par_lowerbound = c(0.0, 0.0),
    par_upperbound = c(1.0, 1.0)
  )

  result <- FLAREr:::update_parameters_enkf(pars, predicted_obs, zt, psi, z_index, pars_config)
  expect_equal(dim(result), c(npars, nmembers))
})

test_that("update_parameters_enkf keeps all output within bounds", {
  set.seed(11)
  npars    <- 1
  nmembers <- 20
  nobs     <- 5

  # Tight bounds [0.4, 0.6] to maximise chance of hitting them
  lb <- 0.4; ub <- 0.6
  pars          <- matrix(runif(npars * nmembers, lb, ub), nrow = npars)
  predicted_obs <- matrix(rnorm(nobs * nmembers, mean = 5, sd = 3), nrow = nobs)
  zt            <- rep(15, nobs)   # far observation to drive large update
  psi           <- rep(1.0, nobs)
  z_index       <- seq_len(nobs)
  pars_config   <- data.frame(par_lowerbound = lb, par_upperbound = ub)

  result <- FLAREr:::update_parameters_enkf(pars, predicted_obs, zt, psi, z_index, pars_config)
  expect_true(all(result >= lb))
  expect_true(all(result <= ub))
})

test_that("update_parameters_enkf shifts parameter mean toward observation signal", {
  set.seed(12)
  npars    <- 1
  nmembers <- 50
  nobs     <- 1

  # Parameter strongly positively correlated with predicted obs
  par_true      <- 0.5
  pars          <- matrix(rnorm(nmembers, mean = par_true, sd = 0.05), nrow = npars)
  predicted_obs <- matrix(pars * 10 + rnorm(nmembers, 0, 0.01), nrow = nobs)
  zt            <- par_true * 10 + 1.0   # obs is 1 unit above predicted mean
  psi           <- c(0.1)
  z_index       <- 1L
  pars_config   <- data.frame(par_lowerbound = 0.0, par_upperbound = 1.0)

  prior_mean    <- mean(pars)
  result        <- FLAREr:::update_parameters_enkf(pars, predicted_obs, zt, psi, z_index, pars_config)
  posterior_mean <- mean(result)
  # Observation is above predicted → parameter should increase
  expect_gt(posterior_mean, prior_mean)
})

# shift_heights_for_depth_change() ----

test_that("shift_heights_for_depth_change is a no-op for zero or NA shift", {
  heights <- c(9, 6, 3, 1, NA)

  r0 <- FLAREr:::shift_heights_for_depth_change(heights, 0)
  expect_equal(r0$heights, heights)
  expect_equal(r0$diff_height, 0)

  rNA <- FLAREr:::shift_heights_for_depth_change(heights, NA_real_)
  expect_equal(rNA$heights, heights)
  expect_true(is.na(rNA$diff_height))
})

test_that("shift_heights_for_depth_change raises all non-NA heights on an upward shift without pruning", {
  heights <- c(9, 6, 3, 1, NA)
  res <- FLAREr:::shift_heights_for_depth_change(heights, 2)

  expect_equal(res$diff_height, 2)                 # not clamped
  expect_equal(res$heights, c(11, 8, 5, 3, NA))    # trailing NA preserved
})

test_that("shift_heights_for_depth_change prunes only layers driven below zero on a moderate downward shift", {
  heights <- c(9, 6, 3, 1, NA)
  res <- FLAREr:::shift_heights_for_depth_change(heights, -2)

  expect_equal(res$diff_height, -2)                # -2 >= -(2nd highest = 6): no clamp
  # layer at height 1 -> -1 is pruned; surviving block stays contiguous at front
  expect_equal(res$heights, c(7, 4, 1, NA, NA))
  expect_equal(which(!is.na(res$heights)), c(1L, 2L, 3L))
})

test_that("shift_heights_for_depth_change clamps a too-deep downward shift to keep min_layers", {
  heights <- c(9, 6, 3, 1, NA)
  res <- FLAREr:::shift_heights_for_depth_change(heights, -8, min_layers = 2L)

  # 2nd-highest height is 6, so the deepest allowed shift is -6
  expect_equal(res$diff_height, -6)
  # exactly two layers survive; the 2nd-highest lands at exactly 0 (not pruned)
  expect_equal(sum(!is.na(res$heights)), 2L)
  expect_equal(res$heights[!is.na(res$heights)], c(3, 0))
})
