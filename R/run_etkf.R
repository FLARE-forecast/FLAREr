#' @title Run Ensemble Transform Kalman Filter on model predictions
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
run_etkf <- function(x_matrix,
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

  R        <- FLAREr:::build_R_matrix(psi, z_index)
  ens_mean <- rowMeans(x_matrix)
  A        <- x_matrix - ens_mean  # [nx, N]
  Y        <- h %*% A              # [nobs, N]
  d        <- zt - h %*% ens_mean  # [nobs]

  if (!is.null(config$da_setup$localization_distance) &&
      !is.na(config$da_setup$localization_distance)) {

    # Localized path: materialize P_t, apply Schur taper, then use
    # a deterministic (noise-free) Kalman update on the ensemble.
    p_t <- A %*% t(A) / (nmembers - 1)
    p_t <- FLAREr:::localization(
      mat                   = p_t,
      nstates               = nstates,
      modeled_depths        = config$model_settings$modeled_depths,
      localization_distance = config$da_setup$localization_distance,
      num_single_states     = dim(p_t)[1] - nstates * ndepths_modeled
    )
    s_mat <- h %*% p_t %*% t(h) + R
    k_t   <- t(solve(s_mat, h %*% p_t, tol = .Machine$double.eps))

    ens_mean_upd <- ens_mean + k_t %*% d

    # (I - KH) A without forming the [nx, nx] identity matrix
    update <- matrix(ens_mean_upd, nrow(x_matrix), nmembers) + A - k_t %*% Y

  } else {

    # Standard ETKF: efficient [N, N] eigendecomposition avoids
    # materializing the full [nx, nx] state covariance.
    C   <- crossprod(Y, solve(R, Y)) + (nmembers - 1) * diag(nmembers)
    eig <- eigen(C, symmetric = TRUE)

    # Symmetric matrix square root C^{-1/2}
    T_mat <- eig$vectors %*%
             diag(1 / sqrt(eig$values)) %*%
             t(eig$vectors)

    # Mean update: x̄_a = x̄_f + A C^{-1} Y^T R^{-1} d  (Hunt et al. 2007 eq. 5)
    w_mean       <- solve(C, crossprod(Y, solve(R, d)))
    ens_mean_upd <- ens_mean + A %*% w_mean

    # Deterministic ensemble perturbations: A sqrt(N-1) C^{-1/2}
    update <- matrix(ens_mean_upd, nrow(x_matrix), nmembers) +
              sqrt(nmembers - 1) * A %*% T_mat
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
