#' @title Run Local Ensemble Transform Kalman Filter on model predictions
#'
#' @description Per-depth-layer ETKF with Gaussian observation localization.
#'   Each depth layer gets its own local analysis using only nearby observations,
#'   with R inflated by 1/weight for partially-local observations.
#'   Non-depth rows (lake depth, secchi, parameters) receive a global ETKF update.
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
#' @param log_particle_weights_start log of particle weights
#' @param snow_ice_thickness_start vector of snow and ice thickness
#' @param diagnostics_start diagnostics
#' @param diagnostics_daily_start daily diagnostics
#' @param pars_config parameter configuration list
#' @param config FLARE configuration list
#' @param depth_index index in x matrix with depth values
#' @param secchi_index index in x matrix with secchi values
#' @param depth_obs observed depth
#' @param depth_sd observed depth standard deviation
#' @param par_fit_method method for fixing parameters
#' @param inflation_start inflation array from prior
#' @param lake_max_depth maximum lake depth
#' @noRd
#'
#' @return list of updated model states, diagnostics, and parameters
run_letkf <- function(x_matrix,
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
                      depth_index,
                      secchi_index,
                      depth_obs,
                      depth_sd,
                      par_fit_method,
                      inflation_start,
                      lake_max_depth) {

  if (!is.null(pars_config)) {
    npars <- dim(pars_corr)[1]
  } else {
    npars <- 0
  }
  nmembers        <- dim(states_depth_start)[3]
  nstates         <- dim(states_depth_start)[1]
  ndepths_modeled <- length(config$model_settings$modeled_depths)
  modeled_depths  <- config$model_settings$modeled_depths
  loc_dist        <- config$da_setup$localization_distance
  nobs            <- length(z_index)

  # Depth of each active observation, derived from its position in the full
  # psi vector.  The first (length(psi) - depth_index - secchi_index) entries
  # are vertical observations ordered as state-type × depth; non-vertical
  # entries (lake-depth obs, secchi obs) get NA and will receive weight = 1.
  n_vert     <- length(psi) - depth_index - secchi_index
  vert_mask  <- z_index <= n_vert
  obs_depths <- rep(NA_real_, nobs)
  obs_depths[vert_mask] <-
    modeled_depths[(z_index[vert_mask] - 1L) %% ndepths_modeled + 1L]

  # Ensemble mean and perturbation matrix (computed once for the full step)
  ens_mean <- rowMeans(x_matrix)
  A        <- x_matrix - ens_mean   # [nx, N]

  # Full predicted-observation perturbations and innovation (reused each layer)
  R <- FLAREr:::build_R_matrix(psi, z_index)   # [nobs, nobs]
  Y <- h %*% A                                  # [nobs, N]
  d <- zt - h %*% ens_mean                      # [nobs]

  # Initialise update to prior; depth loop overwrites state rows.
  update <- x_matrix

  # ---- Per-depth local analysis ----
  for (d_idx in seq_along(modeled_depths)) {

    depth_d <- modeled_depths[d_idx]

    # Gaussian localization weights; NA obs_depths (non-vertical) → weight 1
    loc_weights <- exp(-0.5 * ((obs_depths - depth_d) / loc_dist)^2)
    loc_weights[is.na(loc_weights)] <- 1.0

    local_mask <- loc_weights > 0.01
    if (!any(local_mask)) next

    # Local R inflated by 1/weight (distant obs trusted less)
    n_local <- sum(local_mask)
    R_local  <- diag(psi[z_index[local_mask]]^2 / loc_weights[local_mask],
                     nrow = n_local)
    Y_local  <- Y[local_mask, , drop = FALSE]    # [n_local, N]
    d_local  <- d[local_mask]                    # [n_local]

    # Local ETKF in ensemble space [N, N]
    C_local <- crossprod(Y_local, solve(R_local, Y_local)) +
               (nmembers - 1) * diag(nmembers)
    eig_l   <- eigen(C_local, symmetric = TRUE)
    T_local <- sqrt(nmembers - 1) *
               eig_l$vectors %*%
               diag(1 / sqrt(pmax(eig_l$values, 0))) %*%
               t(eig_l$vectors)
    w_mean  <- solve(C_local, crossprod(Y_local, solve(R_local, d_local)))

    # Apply update to the nstates rows for this depth layer only
    local_rows  <- d_idx + (seq_len(nstates) - 1L) * ndepths_modeled
    A_local     <- A[local_rows, , drop = FALSE]   # [nstates, N]
    mean_local  <- ens_mean[local_rows] + A_local %*% w_mean

    update[local_rows, ] <- matrix(mean_local, nstates, nmembers) +
                            A_local %*% T_local
  }

  # ---- Global ETKF for non-depth rows (lake depth, secchi, parameters) ----
  n_global    <- nrow(x_matrix) - nstates * ndepths_modeled
  global_rows <- seq_len(n_global) + nstates * ndepths_modeled

  if (n_global > 0) {
    C_global <- crossprod(Y, solve(R, Y)) + (nmembers - 1) * diag(nmembers)
    eig_g    <- eigen(C_global, symmetric = TRUE)
    T_global <- sqrt(nmembers - 1) *
                eig_g$vectors %*%
                diag(1 / sqrt(pmax(eig_g$values, 0))) %*%
                t(eig_g$vectors)
    w_global    <- solve(C_global, crossprod(Y, solve(R, d)))
    A_global    <- A[global_rows, , drop = FALSE]
    mean_global <- ens_mean[global_rows] + A_global %*% w_global

    update[global_rows, ] <- matrix(mean_global, n_global, nmembers) +
                             A_global %*% T_global
  }

  FLAREr:::apply_da_updates(
    update                       = update,
    states_depth_start           = states_depth_start,
    states_height_start          = states_height_start,
    model_internal_heights_start = model_internal_heights_start,
    lake_depth_start             = lake_depth_start,
    snow_ice_thickness_start     = snow_ice_thickness_start,
    diagnostics_start            = diagnostics_start,
    diagnostics_daily_start      = diagnostics_daily_start,
    pars_corr                    = pars_corr,
    pars_config                  = pars_config,
    config                       = config,
    depth_index                  = depth_index,
    secchi_index                 = secchi_index,
    par_fit_method               = par_fit_method,
    inflation_start              = inflation_start,
    lake_max_depth               = lake_max_depth,
    nmembers                     = nmembers,
    nstates                      = nstates,
    ndepths_modeled              = ndepths_modeled,
    npars                        = npars
  )
}
