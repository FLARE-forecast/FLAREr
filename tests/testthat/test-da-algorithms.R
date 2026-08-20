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


# da_updated subsetting (carry-through states) -----------------------------

# Build run_enkf inputs with `nstates` full states but only the first
# `n_da_states` of them in the EnKF state block (x_matrix / h). The remaining
# states are "carry-through" (da_updated == 0): present in the full state arrays
# but excluded from assimilation.
.make_da_subset_inputs <- function(nstates     = 2L,
                                    n_da_states = 1L,
                                    ndepths     = 3L,
                                    nmembers    = 40L,
                                    seed        = 7L) {
  set.seed(seed)
  da_idx <- seq_len(n_da_states)

  modeled_depths <- seq_len(ndepths) * 1.0
  lake_depth     <- ndepths * 1.0
  max_layers     <- ndepths

  states_depth_start <- array(
    rnorm(nstates * ndepths * nmembers, mean = 20.0, sd = 0.5),
    dim = c(nstates, ndepths, nmembers)
  )

  # x_matrix spans ONLY the assimilated states (rows = n_da_states * ndepths)
  x_matrix <- matrix(NA_real_, nrow = n_da_states * ndepths, ncol = nmembers)
  for (s in seq_len(n_da_states)) {
    rows <- ((s - 1L) * ndepths + 1L):(s * ndepths)
    x_matrix[rows, ] <- states_depth_start[s, , ]
  }

  glm_heights <- lake_depth - modeled_depths
  model_internal_heights_start <- matrix(rep(glm_heights, nmembers),
                                         nrow = max_layers, ncol = nmembers)

  states_height_start <- array(NA_real_, dim = c(nstates, max_layers, nmembers))
  for (s in seq_len(nstates)) {
    for (m in seq_len(nmembers)) {
      states_height_start[s, , m] <- rev(states_depth_start[s, , m])
    }
  }

  # Observe the first assimilated state at every depth.
  h <- matrix(0, nrow = ndepths, ncol = n_da_states * ndepths)
  for (d in seq_len(ndepths)) h[d, d] <- 1.0

  # Push the observation well away from the prior mean so the update is large.
  zt      <- rowMeans(x_matrix[seq_len(ndepths), , drop = FALSE]) + 5.0
  psi     <- rep(0.3, ndepths)
  z_index <- seq_len(ndepths)

  config <- list(
    model_settings  = list(modeled_depths = modeled_depths),
    da_setup        = list(log_transform_wq_obs = FALSE,
                           localization_distance = NA),
    output_settings = list(diagnostics_names = character(0L),
                           diagnostics_daily = list(csv_names = character(0L)))
  )

  list(x_matrix = x_matrix, h = h, zt = zt, psi = psi, z_index = z_index,
       states_depth_start = states_depth_start,
       states_height_start = states_height_start,
       model_internal_heights_start = model_internal_heights_start,
       lake_depth_start = rep(lake_depth, nmembers),
       log_particle_weights_start = rep(log(1.0), nmembers),
       snow_ice_thickness_start = matrix(0, nrow = 3L, ncol = nmembers),
       config = config, nstates = nstates, n_da_states = n_da_states,
       da_idx = da_idx, ndepths = ndepths, nmembers = nmembers,
       lake_depth_val = lake_depth)
}

test_that("run_enkf leaves da_updated == 0 states at their forecast value", {
  d <- .make_da_subset_inputs(nstates = 2L, n_da_states = 1L)

  result <- FLAREr:::run_enkf(
    x_matrix = d$x_matrix, h = d$h,
    pars_corr = NULL, zt = d$zt, psi = d$psi, z_index = d$z_index,
    states_depth_start = d$states_depth_start,
    states_height_start = d$states_height_start,
    model_internal_heights_start = d$model_internal_heights_start,
    lake_depth_start = d$lake_depth_start,
    log_particle_weights_start = d$log_particle_weights_start,
    snow_ice_thickness_start = d$snow_ice_thickness_start,
    diagnostics_start = NULL, diagnostics_daily_start = NULL,
    pars_config = NULL, config = d$config,
    obs_non_vertical = list(), active_in_xmatrix = character(0L),
    n_non_vertical = 0L, par_fit_method = "perturb",
    inflation_start = rep(1.0, d$nmembers), lake_max_depth = d$lake_depth_val,
    n_da_states = d$n_da_states, da_idx = d$da_idx
  )

  # Full state shape is preserved (both states present in the output).
  expect_equal(dim(result$states_depth_updated),
               c(d$nstates, d$ndepths, d$nmembers))

  # Carry-through state (index 2) is untouched by DA: depth and height arrays
  # equal the forecast exactly.
  expect_equal(result$states_depth_updated[2, , ],
               d$states_depth_start[2, , ])
  expect_equal(result$states_height_updated[2, , ],
               d$states_height_start[2, , ])

  # Assimilated state (index 1) moves toward the (shifted) observation.
  expect_gt(mean(result$states_depth_updated[1, , ]),
            mean(d$states_depth_start[1, , ]))
})

test_that("run_enkf da_idx = all reproduces the legacy (no-subset) result", {
  # With every state assimilated, passing da_idx/n_da_states explicitly must
  # match omitting them (the NULL default), guaranteeing a no-op for old configs.
  base <- .make_da_inputs(nstates = 2L, nmembers = 30L, seed = 11L)

  call_enkf <- function(extra) {
    set.seed(123L)
    do.call(FLAREr:::run_enkf, c(list(
      x_matrix = base$x_matrix, h = base$h, pars_corr = base$pars_corr,
      zt = base$zt, psi = base$psi, z_index = base$z_index,
      states_depth_start = base$states_depth_start,
      states_height_start = base$states_height_start,
      model_internal_heights_start = base$model_internal_heights_start,
      lake_depth_start = base$lake_depth_start,
      log_particle_weights_start = base$log_particle_weights_start,
      snow_ice_thickness_start = base$snow_ice_thickness_start,
      diagnostics_start = base$diagnostics_start,
      diagnostics_daily_start = base$diagnostics_daily_start,
      pars_config = base$pars_config, config = base$config,
      obs_non_vertical = list(), active_in_xmatrix = character(0L),
      n_non_vertical = 0L, par_fit_method = base$par_fit_method,
      inflation_start = base$inflation_start,
      lake_max_depth = base$lake_max_depth
    ), extra))
  }

  legacy   <- call_enkf(list())
  explicit <- call_enkf(list(n_da_states = 2L, da_idx = 1:2))
  expect_equal(explicit$states_depth_updated, legacy$states_depth_updated)
  expect_equal(explicit$states_height_updated, legacy$states_height_updated)
})


# update_glm_restart_file carry-through ------------------------------------

# Minimal GLM restart NetCDF with the variables update_glm_restart_file touches.
.make_restart_nc <- function(path, max_layers, wq_names, wq_sentinel) {
  nwq      <- length(wq_names)
  dim_lev  <- ncdf4::ncdim_def("nlev", "", seq_len(max_layers))
  dim_wq   <- ncdf4::ncdim_def("n_wq", "", seq_len(nwq))
  dim_one  <- ncdf4::ncdim_def("one", "", 1L)
  dim_char <- ncdf4::ncdim_def("nchar", "", seq_len(64L), create_dimvar = FALSE)

  v_temp  <- ncdf4::ncvar_def("lake_temp",   "", dim_lev, prec = "double")
  v_salt  <- ncdf4::ncvar_def("lake_salt",   "", dim_lev, prec = "double")
  v_hgt   <- ncdf4::ncvar_def("lake_height", "", dim_lev, prec = "double")
  v_wq    <- ncdf4::ncvar_def("wq_vars", "", list(dim_lev, dim_wq), prec = "double")
  v_names <- ncdf4::ncvar_def("wq_var_names", "", list(dim_char, dim_wq), prec = "char")
  v_bice  <- ncdf4::ncvar_def("blue_ice",       "", dim_one, prec = "double")
  v_wice  <- ncdf4::ncvar_def("white_ice",      "", dim_one, prec = "double")
  v_snow  <- ncdf4::ncvar_def("snow_thickness", "", dim_one, prec = "double")

  nc <- ncdf4::nc_create(path, list(v_temp, v_salt, v_hgt, v_wq, v_names,
                                     v_bice, v_wice, v_snow))
  ncdf4::ncvar_put(nc, v_temp, rep(0, max_layers))
  ncdf4::ncvar_put(nc, v_salt, rep(0, max_layers))
  ncdf4::ncvar_put(nc, v_hgt,  rep(0, max_layers))
  # Each WQ column seeded with a distinct sentinel so carry-through is visible.
  wq_init <- matrix(rep(wq_sentinel, each = max_layers), nrow = max_layers)
  ncdf4::ncvar_put(nc, v_wq, wq_init)
  ncdf4::ncvar_put(nc, v_names, wq_names)
  ncdf4::ncvar_put(nc, v_bice, 0); ncdf4::ncvar_put(nc, v_wice, 0)
  ncdf4::ncvar_put(nc, v_snow, 0)
  ncdf4::ncatt_put(nc, 0, "NumLayers", max_layers, prec = "int")
  ncdf4::nc_close(nc)
}

test_that("update_glm_restart_file skips da_updated == 0 water-quality states", {
  skip_if_not_installed("ncdf4")

  tmp <- tempfile()
  dir.create(tmp)
  m <- 1L
  max_layers  <- 4L
  wq_names    <- c("OXY_oxy", "CAR_dic", "NIT_amm")
  wq_sentinel <- c(-101, -102, -103)   # per-column carry-through markers
  .make_restart_nc(file.path(tmp, paste0("glm_restart_", m, ".nc")),
                   max_layers, wq_names, wq_sentinel)

  state_names <- c("temp", "salt", wq_names)   # nstates = 5
  da_updated  <- c(1L, 1L, 1L, 0L, 1L)         # CAR_dic (wq #2) carried through

  # states_heights_start[state, layer]; distinct values per state.
  states_heights_start <- matrix(NA_real_, nrow = 5L, ncol = max_layers)
  for (s in seq_len(5L)) states_heights_start[s, ] <- s * 10 + seq_len(max_layers)
  glm_heights_start <- c(3, 2, 1, 0)           # all non-NA -> nlev = 4

  FLAREr:::update_glm_restart_file(
    ens_working_directory    = tmp,
    m                        = m,
    states_heights_start     = states_heights_start,
    glm_heights_start        = glm_heights_start,
    snow_ice_thickness_start = c(0, 0, 0),
    num_wq_vars              = 3L,
    include_wq               = TRUE,
    state_names              = state_names,
    da_updated               = da_updated
  )

  nc       <- ncdf4::nc_open(file.path(tmp, paste0("glm_restart_", m, ".nc")))
  wqv      <- ncdf4::ncvar_get(nc, "wq_vars")
  tmp_temp <- ncdf4::ncvar_get(nc, "lake_temp")
  ncdf4::nc_close(nc)

  # Carry-through column (CAR_dic) keeps its sentinel untouched.
  expect_equal(wqv[, 2], rep(wq_sentinel[2], max_layers))
  # Assimilated WQ columns are overwritten with rev(states_heights_start[2+wq, ]).
  expect_equal(wqv[, 1], rev(states_heights_start[3, ]))
  expect_equal(wqv[, 3], rev(states_heights_start[5, ]))
  # temp (assimilated) is written.
  expect_equal(as.numeric(tmp_temp), rev(states_heights_start[1, ]))
})
