#' @title Run particle filter on model predictions
#'
#' @param x_matrix matrix of model states (includes secchi and depths)
#' @param h matrix to map x matrix to zt vector
#' @param pars_corr matrix of parameters
#' @param zt vector of observations
#' @param psi vector of observation standard deviations
#' @param z_index indexes of observations
#' @param states_depth_start states orientated by depth
#' @param states_height_start states orientated by height
#' @param model_internal_heights_start heights predicted by GLM model
#' @param lake_depth_start lake depth
#' @param log_particle_weights_start log of particle weights (cumulative across
#'   assimilation steps since last resample; reset to log(1) after resampling)
#' @param snow_ice_thickness_start vector of snow and ice thickness
#' @param diagnostics_start diagnostics
#' @param diagnostics_daily_start daily diagnostics (a restart variable)
#' @param pars_config parameter configuration list
#' @param config FLARE configuration list
#' @param obs_non_vertical named list of non-vertical observation metadata
#' @param active_in_xmatrix character vector of non-vertical variable names in state-vector order
#' @param n_non_vertical integer number of non-vertical variables in the augmented state
#' @param par_fit_method method for fixing parameters
#' @param vertical_obs number of vertical observations (i.e. states not
#'   associated with a depth)
#' @param working_directory current working directory for simulation
#' @param obs_config list of observation configurations
#' @param inflation_start covariance inflation factor (a restart variable;
#'   passed through unchanged — the PF does not use covariance inflation)
#'
#' @return list of updated model states, diagnostics, and parameters
#' @noRd
#'
run_particle_filter <- function(x_matrix,
                                h,
                                pars_corr,
                                zt,
                                psi,
                                z_index,
                                states_depth_start,
                                states_height_start,
                                model_internal_heights_start,
                                lake_depth_start,
                                log_particle_weights_start,
                                snow_ice_thickness_start,
                                diagnostics_start,
                                diagnostics_daily_start,
                                pars_config,
                                config,
                                obs_non_vertical,
                                active_in_xmatrix,
                                n_non_vertical,
                                par_fit_method,
                                vertical_obs,
                                working_directory,
                                obs_config,
                                inflation_start) {

  npars           <- if (is.null(pars_corr)) 0L else nrow(pars_corr)
  nmembers        <- dim(states_depth_start)[3]   # FIX: was length(nmembers) = 1
  nstates         <- dim(states_depth_start)[1]
  ndepths_modeled <- length(config$model_settings$modeled_depths)

  # Project all ensemble members through the observation operator.
  # Result: obs_states[m, i] = predicted value of observation i by member m.
  obs_states <- t(h %*% x_matrix)  # [nmembers, nobs]

  # ---------------------------------------------------------------------------
  # Step 1: Per-member log-likelihood
  # ---------------------------------------------------------------------------
  # Sum log N(zt_i | obs_states[m,i], psi[z_index[i]]) over active observations.
  #
  # Shallow-member handling: when a member's lake does not reach an observation
  # depth, obs_states[m,i] is NA.  We assign a log-likelihood contribution of 0
  # (i.e., no evidence) rather than an arbitrary large negative penalty.
  # Penalising shallow members here conflates the observation operator with lake
  # physics; the depth observation already down-weights members whose lake is too
  # shallow through its own likelihood term.

  LL <- numeric(nmembers)   # FIX: was rep(NA, length(nmembers)) — length(scalar) = 1
  for (m in seq_len(nmembers)) {
    LL_vector <- dnorm(zt, mean = obs_states[m, ], sd = psi[z_index], log = TRUE)

    if (any(is.infinite(LL_vector))) {
      warning("infinite log-likelihood for member ", m,
              ": check observation standard deviations")
    }

    # Observations that the member cannot see (lake too shallow) get 0
    # contribution rather than a hard -Inf / large-negative penalty.
    na_index <- which(is.na(obs_states[m, ]))
    if (length(na_index) > 0) {
      LL_vector[na_index] <- 0
    }

    LL[m] <- sum(LL_vector)
  }

  # ---------------------------------------------------------------------------
  # Step 2: Numerically stable weight update using the log-sum-exp trick
  # ---------------------------------------------------------------------------
  # Naive normalisation exp(LL) / sum(exp(LL)) causes 0/0 = NaN whenever all
  # LL values are sufficiently negative (a realistic scenario in high-dimensional
  # or tightly constrained problems).  Subtracting max(LL) before exponentiating
  # keeps values in (0, 1] and is algebraically equivalent.
  #
  # Tempered likelihood: raising the likelihood to power phi in (0, 1] softens
  # observation influence and prevents weight collapse when observations are very
  # informative relative to ensemble spread.  In log-space this is multiplication
  # by phi.  phi = 1 (default) recovers the standard bootstrap PF.

  phi <- if (is.null(config$da_setup$pf_tempering_factor)) {
    1.0
  } else {
    config$da_setup$pf_tempering_factor
  }
  if (!is.numeric(phi) || length(phi) != 1L || phi <= 0 || phi > 1) {
    stop("config$da_setup$pf_tempering_factor must be a single number in (0, 1]")
  }

  LL_tempered <- phi * LL
  LL_max      <- max(LL_tempered)
  shifted     <- exp(LL_tempered - LL_max)               # all values in (0, 1]
  log_wt_step <- (LL_tempered - LL_max) - log(sum(shifted))  # normalised log weights for this step

  # Guard: NaN in log_wt_step indicates a model / config problem (e.g. psi = 0),
  # not numerical underflow (which the log-sum-exp trick already handles).
  if (anyNA(log_wt_step)) {
    index     <- ceiling(z_index[z_index <= vertical_obs * ndepths_modeled] / ndepths_modeled)
    obs_names <- obs_config$state_names_obs[index]
    obs_names <- c(obs_names, active_in_xmatrix)
    readr::write_csv(
      x = tibble::tibble(obs_names = obs_names,
                         obs  = zt,
                         pred = obs_states[1, ],
                         sd   = psi[z_index],
                         LL   = dnorm(zt, mean = obs_states[1, ], sd = psi[z_index], log = TRUE)),
      file = file.path(working_directory, "PF_with_NaN.csv"))
    stop("PF log-weights contain NaN; check observation standard deviations and ",
         "predicted state values. Diagnostics written to PF_with_NaN.csv")
  }

  # Accumulate log weights across assimilation steps.  log_particle_weights_start
  # is a vector of log(1) = 0 immediately after a resample event, so weights
  # accumulate only between resampling steps.
  log_particle_weights_updated <- log_particle_weights_start + log_wt_step

  # ---------------------------------------------------------------------------
  # Step 3: Effective sample size (ESS)
  # ---------------------------------------------------------------------------
  # ESS = 1 / sum(w_i^2) where w_i are *normalised* weights (summing to 1).
  # Normalise accumulated log-weights before computing ESS; without this,
  # multi-step weight products do not sum to 1 and the formula is incorrect.

  w_acc  <- exp(log_particle_weights_updated)
  w_norm <- w_acc / sum(w_acc)
  Neff   <- 1 / sum(w_norm^2)

  # ---------------------------------------------------------------------------
  # Step 4: Resampling
  # ---------------------------------------------------------------------------
  # Resample when ESS < N/2, or unconditionally when pf_always_resample = TRUE.
  # pf_always_resample is useful for methodological comparisons (bootstrap PF).
  #
  # FIX: use || (scalar short-circuit) instead of | (element-wise vectorised or).

  if (Neff < nmembers / 2 || config$da_setup$pf_always_resample) {

    # Systematic resampling: strictly lower variance than multinomial resampling
    # for the same expected particle counts.  A single uniform draw u0 locates N
    # equally-spaced positions across the cumulative weight CDF, so every weight
    # interval of size 1/N is guaranteed to be visited exactly once on average.
    cumwt         <- cumsum(w_norm)
    cumwt[nmembers] <- 1.0           # prevent floating-point rounding past 1
    u0            <- runif(1, 0, 1 / nmembers)
    positions     <- u0 + (seq_len(nmembers) - 1L) / nmembers
    # findInterval(x, vec) returns i s.t. vec[i] <= x < vec[i+1]; +1L maps to
    # 1-based particle indices.  pmin/pmax clamp boundary floating-point edge cases.
    samples <- pmin(pmax(findInterval(positions, cumwt) + 1L, 1L), nmembers)

    # Reset accumulated weights to uniform after resampling.
    log_particle_weights_updated[] <- log(1.0)

    # Resample GLM restart files to match particle resampling.
    # Stage unique sources to temp files first to avoid overwrite conflicts
    # (e.g. when two members swap their restart files with each other).
    rst_name <- function(member_dir, member_idx) {
      file.path(working_directory, member_dir, paste0("glm_restart_", member_idx, ".nc"))
    }
    have_restart <- file.exists(rst_name(samples, samples))
    if (any(have_restart)) {
      unique_sources <- unique(samples[have_restart])
      tmp_files <- setNames(
        vapply(unique_sources, function(s) {
          tmp <- tempfile(fileext = ".nc")
          file.copy(rst_name(s, s), tmp)
          tmp
        }, character(1)),
        as.character(unique_sources)
      )
      for (m in seq_len(nmembers)) {
        key <- as.character(samples[m])
        if (key %in% names(tmp_files)) {
          file.copy(tmp_files[[key]], rst_name(m, m), overwrite = TRUE)
        }
      }
      unlink(tmp_files)
    }

    idx <- samples   # member indices to draw from

  } else {
    idx <- seq_len(nmembers)   # identity selection — no resampling
  }

  # ---------------------------------------------------------------------------
  # Step 5: Apply idx to all state arrays
  # ---------------------------------------------------------------------------
  # idx is either the resampled particle indices or the identity permutation,
  # so the same indexing code handles both the resample and pass-through cases,
  # eliminating the duplicate branch that existed in the original implementation.

  update              <- x_matrix[seq_len(ndepths_modeled * nstates), idx]
  states_depth_updated   <- aperm(array(c(update),
                                        dim = c(ndepths_modeled, nstates, nmembers)),
                                   perm = c(2, 1, 3))
  states_height_updated          <- states_height_start[, , idx]
  snow_ice_thickness_updated     <- snow_ice_thickness_start[, idx]
  lake_depth_updated             <- lake_depth_start[idx]
  model_internal_heights_updated <- model_internal_heights_start[, idx]

  pars_updated <- NULL
  if (npars > 0) {
    pars_updated <- pars_corr[, idx]
  }

  # Diagnostics arrays vary in dimensionality depending on how many diagnostic
  # variables are configured.  The original resample branch had an unreachable
  # ndiag == 1 case (the outer guard was `> 1`, so the inner `== 1` could never
  # be true); the unified idx approach below fixes that silently.
  ndiag <- length(config$output_settings$diagnostics_names)
  if (ndiag > 1) {
    diagnostics_updated <- diagnostics_start[, , idx]
  } else if (ndiag == 1) {
    # diagnostics_start is 2-D [ndepths, nmembers]; wrap to [1, ndepths, nmembers]
    # to match the 3-D layout expected by the output writer.
    diagnostics_updated        <- array(NA, dim = c(1L, dim(diagnostics_start)))
    diagnostics_updated[1, , ] <- diagnostics_start[, idx]
  } else {
    diagnostics_updated <- diagnostics_start
  }

  ndiag_daily <- length(config$output_settings$diagnostics_daily$csv_names)
  if (ndiag_daily > 1) {
    diagnostics_daily_updated <- diagnostics_daily_start[, idx]
  } else if (ndiag_daily == 1) {
    diagnostics_daily_updated        <- array(NA, dim = c(1L, dim(diagnostics_daily_start)))
    diagnostics_daily_updated[1, , ] <- diagnostics_daily_start[, idx]
  } else {
    diagnostics_daily_updated <- diagnostics_daily_start
  }

  return(list(pars_updated                  = pars_updated,
              states_depth_updated          = states_depth_updated,
              states_height_updated         = states_height_updated,
              lake_depth_updated            = lake_depth_updated,
              model_internal_heights_updated = model_internal_heights_updated,
              log_particle_weights_updated  = log_particle_weights_updated,
              diagnostics_updated           = diagnostics_updated,
              diagnostics_daily_updated     = diagnostics_daily_updated,
              snow_ice_thickness_updated    = snow_ice_thickness_updated,
              inflation_update              = inflation_start))  # PF does not use inflation
}
