# Tests for DA update algorithms using synthetic ensemble data.
# No GLM binary or network access required.

# Shared helpers -----------------------------------------------------------

# Build a minimal but valid set of inputs for run_enkf / run_particle_filter.
# nstates = 1 (temperature), ndepths depths, nmembers ensemble members.
.make_da_inputs <- function(nstates   = 1L,
                             ndepths   = 3L,
                             nmembers  = 10L,
                             mean_temp = 20.0,
                             seed      = 42L) {
  set.seed(seed)

  modeled_depths <- seq_len(ndepths) * 1.0   # 1, 2, ..., ndepths
  lake_depth     <- ndepths * 1.0
  max_layers     <- ndepths

  # states_depth_start [nstates, ndepths, nmembers]
  states_depth_start <- array(
    rnorm(nstates * ndepths * nmembers, mean = mean_temp, sd = 0.5),
    dim = c(nstates, ndepths, nmembers)
  )

  # x_matrix [nstates*ndepths, nmembers] — depth index within each state block
  x_matrix <- matrix(NA_real_, nrow = nstates * ndepths, ncol = nmembers)
  for (s in seq_len(nstates)) {
    rows <- ((s - 1L) * ndepths + 1L):(s * ndepths)
    x_matrix[rows, ] <- states_depth_start[s, , ]
  }

  # GLM heights from lake bottom: height = lake_depth - depth
  # depths 1..ndepths → heights (ndepths-1)..0
  glm_heights <- lake_depth - modeled_depths  # decreasing

  # model_internal_heights_start [max_layers, nmembers]
  model_internal_heights_start <- matrix(
    rep(glm_heights, nmembers),
    nrow = max_layers, ncol = nmembers
  )

  # states_height_start [nstates, max_layers, nmembers]
  # Heights go bottom-to-top so depth ordering is reversed
  states_height_start <- array(NA_real_, dim = c(nstates, max_layers, nmembers))
  for (s in seq_len(nstates)) {
    for (m in seq_len(nmembers)) {
      states_height_start[s, , m] <- rev(states_depth_start[s, , m])
    }
  }

  # Observation operator: observe first state at every depth (identity-like)
  h <- matrix(0, nrow = ndepths, ncol = nstates * ndepths)
  for (d in seq_len(ndepths)) h[d, d] <- 1.0

  # Observations equal to ensemble mean (no update expected on average)
  zt      <- rowMeans(x_matrix[seq_len(ndepths), , drop = FALSE])
  psi     <- rep(0.5, ndepths)
  z_index <- seq_len(ndepths)

  config <- list(
    model_settings  = list(modeled_depths = modeled_depths),
    da_setup        = list(
      log_transform_wq_obs  = FALSE,
      localization_distance = NA,
      pf_always_resample    = FALSE
    ),
    output_settings = list(
      diagnostics_names  = character(0L),
      diagnostics_daily  = list(csv_names = character(0L))
    )
  )

  # 1 dummy parameter row for the PF (which reads npars from pars_corr directly).
  # EnKF tests use pars_config = NULL (npars = 0) so no parameter rows are
  # appended to x_matrix, avoiding the "incorrect number of dimensions" error
  # that occurs when pars_updated is extracted from a row-vector instead of a matrix.
  pars_corr <- matrix(runif(nmembers, 0.3, 0.7), nrow = 1L)
  pars_config <- NULL   # EnKF uses NULL → npars = 0; PF reads npars from pars_corr

  list(
    x_matrix                     = x_matrix,
    h                            = h,
    pars_corr                    = pars_corr,
    zt                           = zt,
    psi                          = psi,
    z_index                      = z_index,
    states_depth_start           = states_depth_start,
    states_height_start          = states_height_start,
    model_internal_heights_start = model_internal_heights_start,
    lake_depth_start             = rep(lake_depth, nmembers),
    log_particle_weights_start   = rep(log(1.0), nmembers),
    snow_ice_thickness_start     = matrix(0, nrow = 3L, ncol = nmembers),
    diagnostics_start            = NULL,
    diagnostics_daily_start      = NULL,
    pars_config                  = pars_config,
    config                       = config,
    par_fit_method               = "perturb",
    inflation_start              = rep(1.0, nmembers),
    lake_max_depth               = lake_depth,
    nstates                      = nstates,
    ndepths                      = ndepths,
    nmembers                     = nmembers,
    mean_temp                    = mean_temp
  )
}


# run_enkf() ---------------------------------------------------------------

test_that("run_enkf returns a list with the expected 10 named elements", {
  d <- .make_da_inputs()
  result <- FLAREr:::run_enkf(
    x_matrix                     = d$x_matrix,
    h                            = d$h,
    pars_corr                    = d$pars_corr,
    zt                           = d$zt,
    psi                          = d$psi,
    z_index                      = d$z_index,
    states_depth_start           = d$states_depth_start,
    states_height_start          = d$states_height_start,
    model_internal_heights_start = d$model_internal_heights_start,
    lake_depth_start             = d$lake_depth_start,
    log_particle_weights_start   = d$log_particle_weights_start,
    snow_ice_thickness_start     = d$snow_ice_thickness_start,
    diagnostics_start            = d$diagnostics_start,
    diagnostics_daily_start      = d$diagnostics_daily_start,
    pars_config                  = d$pars_config,
    config                       = d$config,
    obs_non_vertical             = list(),
    active_in_xmatrix            = character(0L),
    n_non_vertical               = 0L,
    par_fit_method               = d$par_fit_method,
    inflation_start              = d$inflation_start,
    lake_max_depth               = d$lake_max_depth
  )
  expect_type(result, "list")
  expect_named(result,
               c("pars_updated", "states_depth_updated", "states_height_updated",
                 "lake_depth_updated", "model_internal_heights_updated",
                 "log_particle_weights_updated", "diagnostics_updated",
                 "diagnostics_daily_updated", "snow_ice_thickness_updated",
                 "inflation_update"),
               ignore.order = TRUE)
})

test_that("run_enkf states_depth_updated has correct [nstates, ndepths, nmembers] shape", {
  d <- .make_da_inputs()
  result <- FLAREr:::run_enkf(
    x_matrix = d$x_matrix, h = d$h,
    pars_corr = d$pars_corr, zt = d$zt, psi = d$psi,
    z_index = d$z_index,
    states_depth_start = d$states_depth_start,
    states_height_start = d$states_height_start,
    model_internal_heights_start = d$model_internal_heights_start,
    lake_depth_start = d$lake_depth_start,
    log_particle_weights_start = d$log_particle_weights_start,
    snow_ice_thickness_start = d$snow_ice_thickness_start,
    diagnostics_start = d$diagnostics_start,
    diagnostics_daily_start = d$diagnostics_daily_start,
    pars_config = d$pars_config, config = d$config,
    obs_non_vertical = list(), active_in_xmatrix = character(0L), n_non_vertical = 0L,
    par_fit_method = d$par_fit_method,
    inflation_start = d$inflation_start,
    lake_max_depth = d$lake_max_depth
  )
  expect_equal(dim(result$states_depth_updated),
               c(d$nstates, d$ndepths, d$nmembers))
})

test_that("run_enkf ensemble mean moves toward observation", {
  # Use 50 members for a stable ensemble mean estimate
  d <- .make_da_inputs(nmembers = 50L, seed = 99L)

  # Shift observations 2 degrees above ensemble mean
  obs_shift <- 2.0
  d$zt <- d$zt + obs_shift

  result <- FLAREr:::run_enkf(
    x_matrix = d$x_matrix, h = d$h,
    pars_corr = d$pars_corr, zt = d$zt, psi = d$psi,
    z_index = d$z_index,
    states_depth_start = d$states_depth_start,
    states_height_start = d$states_height_start,
    model_internal_heights_start = d$model_internal_heights_start,
    lake_depth_start = d$lake_depth_start,
    log_particle_weights_start = d$log_particle_weights_start,
    snow_ice_thickness_start = d$snow_ice_thickness_start,
    diagnostics_start = d$diagnostics_start,
    diagnostics_daily_start = d$diagnostics_daily_start,
    pars_config = d$pars_config, config = d$config,
    obs_non_vertical = list(), active_in_xmatrix = character(0L), n_non_vertical = 0L,
    par_fit_method = d$par_fit_method,
    inflation_start = d$inflation_start,
    lake_max_depth = d$lake_max_depth
  )

  prior_mean    <- mean(d$states_depth_start[1, , ])
  posterior_mean <- mean(result$states_depth_updated[1, , ])
  # Posterior mean should be closer to the (higher) observations
  expect_gt(posterior_mean, prior_mean)
})

test_that("run_enkf log_particle_weights_updated are all log(1)", {
  d <- .make_da_inputs()
  result <- FLAREr:::run_enkf(
    x_matrix = d$x_matrix, h = d$h,
    pars_corr = d$pars_corr, zt = d$zt, psi = d$psi,
    z_index = d$z_index,
    states_depth_start = d$states_depth_start,
    states_height_start = d$states_height_start,
    model_internal_heights_start = d$model_internal_heights_start,
    lake_depth_start = d$lake_depth_start,
    log_particle_weights_start = d$log_particle_weights_start,
    snow_ice_thickness_start = d$snow_ice_thickness_start,
    diagnostics_start = d$diagnostics_start,
    diagnostics_daily_start = d$diagnostics_daily_start,
    pars_config = d$pars_config, config = d$config,
    obs_non_vertical = list(), active_in_xmatrix = character(0L), n_non_vertical = 0L,
    par_fit_method = d$par_fit_method,
    inflation_start = d$inflation_start,
    lake_max_depth = d$lake_max_depth
  )
  # EnKF always resets log-weights to log(1) = 0
  expect_equal(result$log_particle_weights_updated, rep(0, d$nmembers))
})


# run_particle_filter() ----------------------------------------------------

test_that("run_particle_filter returns a list with the expected 10 named elements", {
  d   <- .make_da_inputs()
  dir <- tempdir()
  result <- FLAREr:::run_particle_filter(
    x_matrix                     = d$x_matrix,
    h                            = d$h,
    pars_corr                    = d$pars_corr,
    zt                           = d$zt,
    psi                          = d$psi,
    z_index                      = d$z_index,
    states_depth_start           = d$states_depth_start,
    states_height_start          = d$states_height_start,
    model_internal_heights_start = d$model_internal_heights_start,
    lake_depth_start             = d$lake_depth_start,
    log_particle_weights_start   = d$log_particle_weights_start,
    snow_ice_thickness_start     = d$snow_ice_thickness_start,
    diagnostics_start            = d$diagnostics_start,
    diagnostics_daily_start      = d$diagnostics_daily_start,
    pars_config                  = d$pars_config,
    config                       = d$config,
    obs_non_vertical             = list(),
    active_in_xmatrix            = character(0L),
    n_non_vertical               = 0L,
    par_fit_method               = d$par_fit_method,
    vertical_obs                 = 1L,
    working_directory            = dir,
    obs_config                   = data.frame(state_names_obs = "temp"),
    inflation_start              = d$inflation_start
  )
  expect_type(result, "list")
  expect_named(result,
               c("pars_updated", "states_depth_updated", "states_height_updated",
                 "lake_depth_updated", "model_internal_heights_updated",
                 "log_particle_weights_updated", "diagnostics_updated",
                 "diagnostics_daily_updated", "snow_ice_thickness_updated",
                 "inflation_update"),
               ignore.order = TRUE)
})

test_that("run_particle_filter resamples and resets weights when observation is extreme", {
  # Place all members near 20 °C; put observation at 40 °C with tight SD.
  # This makes one range of temperatures vastly more likely → ESS << N/2.
  set.seed(77)
  d <- .make_da_inputs(nmembers = 20L, seed = 77L)

  # Extreme observation far from all ensemble members
  d$zt  <- rep(40.0, d$ndepths)
  d$psi <- rep(0.1, d$ndepths)   # tight SD → very low likelihood for members at 20

  dir <- tempdir()
  result <- FLAREr:::run_particle_filter(
    x_matrix                     = d$x_matrix,
    h                            = d$h,
    pars_corr                    = d$pars_corr,
    zt                           = d$zt,
    psi                          = d$psi,
    z_index                      = d$z_index,
    states_depth_start           = d$states_depth_start,
    states_height_start          = d$states_height_start,
    model_internal_heights_start = d$model_internal_heights_start,
    lake_depth_start             = d$lake_depth_start,
    log_particle_weights_start   = d$log_particle_weights_start,
    snow_ice_thickness_start     = d$snow_ice_thickness_start,
    diagnostics_start            = d$diagnostics_start,
    diagnostics_daily_start      = d$diagnostics_daily_start,
    pars_config                  = d$pars_config,
    config                       = d$config,
    obs_non_vertical             = list(),
    active_in_xmatrix            = character(0L),
    n_non_vertical               = 0L,
    par_fit_method               = d$par_fit_method,
    vertical_obs                 = 1L,
    working_directory            = dir,
    obs_config                   = data.frame(state_names_obs = "temp"),
    inflation_start              = d$inflation_start
  )
  # After resampling, accumulated log-weights are reset to log(1) = 0
  expect_equal(result$log_particle_weights_updated, rep(0, d$nmembers))
})

test_that("run_particle_filter preserves states_depth shape [nstates, ndepths, nmembers]", {
  d   <- .make_da_inputs()
  dir <- tempdir()
  result <- FLAREr:::run_particle_filter(
    x_matrix                     = d$x_matrix,
    h                            = d$h,
    pars_corr                    = d$pars_corr,
    zt                           = d$zt,
    psi                          = d$psi,
    z_index                      = d$z_index,
    states_depth_start           = d$states_depth_start,
    states_height_start          = d$states_height_start,
    model_internal_heights_start = d$model_internal_heights_start,
    lake_depth_start             = d$lake_depth_start,
    log_particle_weights_start   = d$log_particle_weights_start,
    snow_ice_thickness_start     = d$snow_ice_thickness_start,
    diagnostics_start            = d$diagnostics_start,
    diagnostics_daily_start      = d$diagnostics_daily_start,
    pars_config                  = d$pars_config,
    config                       = d$config,
    obs_non_vertical             = list(),
    active_in_xmatrix            = character(0L),
    n_non_vertical               = 0L,
    par_fit_method               = d$par_fit_method,
    vertical_obs                 = 1L,
    working_directory            = dir,
    obs_config                   = data.frame(state_names_obs = "temp"),
    inflation_start              = d$inflation_start
  )
  expect_equal(dim(result$states_depth_updated),
               c(d$nstates, d$ndepths, d$nmembers))
})
