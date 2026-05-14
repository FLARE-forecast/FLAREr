#' @title Run Ensemble Smoother with Multiple Data Assimilation
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
#' @param obs_non_vertical named list of non-vertical observation metadata
#' @param active_in_xmatrix character vector of non-vertical variable names in state-vector order
#' @param n_non_vertical integer number of non-vertical variables in the augmented state
#' @param par_fit_method method for fixing parameters
#' @param inflation_start inflation array from prior
#' @param lake_max_depth maximum lake depth
#' @noRd
#'
#' @return list of updated model states, diagnostics, and parameters
run_esmda <- function(x_matrix,
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

  n_iter <- config$da_setup$esmda_iterations

  # Observation error inflated by sqrt(n_iter) per iteration so that
  # sum(1/alpha_i) = 1 with alpha_i = n_iter (Emerick & Reynolds 2013).
  curr_psi_iter <- (psi[z_index] * sqrt(n_iter))^2
  if (length(z_index) > 1) {
    psi_t_iter <- diag(curr_psi_iter)
  } else {
    psi_t_iter <- curr_psi_iter
  }

  x_cur <- x_matrix

  for (iter in seq_len(n_iter)) {

    d_mat <- t(mvtnorm::rmvnorm(n = nmembers, mean = zt,
                                sigma = as.matrix(psi_t_iter)))

    if (isTRUE(config$da_setup$log_transform_wq_obs)) {
      wq_rows <- which(z_index > ndepths_modeled)
      if (length(wq_rows) > 0) {
        pos_rows  <- wq_rows[zt[wq_rows] > 0]
        zero_rows <- wq_rows[zt[wq_rows] <= 0]
        for (row_idx in pos_rows) {
          yt_i      <- zt[row_idx]
          sig_i     <- sqrt(curr_psi_iter[row_idx])
          sigma_log <- sqrt(log(1 + (sig_i / yt_i)^2))
          mu_log    <- log(yt_i) - sigma_log^2 / 2
          d_mat[row_idx, ] <- exp(stats::rnorm(nmembers,
                                               mean = mu_log, sd = sigma_log))
        }
        if (length(zero_rows) > 0) {
          if (isTRUE(config$da_setup$log_transform_wq_zero_collapse)) {
            d_mat[zero_rows, ] <- 0.0
          } else {
            d_mat[zero_rows, ][d_mat[zero_rows, ] < 0] <- 0.0
          }
        }
      }
    } else {
      d_mat[which(z_index > ndepths_modeled & d_mat < 0.0)] <- 0.0
    }

    # Recompute ensemble covariance from current (evolving) ensemble
    ens_mean <- rowMeans(x_cur)
    a_mat    <- x_cur - ens_mean
    p_t      <- a_mat %*% t(a_mat) / (nmembers - 1)

    if (!is.null(config$da_setup$localization_distance) &&
          !is.na(config$da_setup$localization_distance)) {
      p_t <- FLAREr:::localization(
        mat                   = p_t,
        nstates               = nstates,
        modeled_depths        = config$model_settings$modeled_depths,
        localization_distance = config$da_setup$localization_distance,
        num_single_states     = dim(p_t)[1] - nstates * ndepths_modeled
      )
    }

    s_mat <- h %*% p_t %*% t(h) + psi_t_iter
    k_t   <- t(solve(s_mat, h %*% p_t, tol = .Machine$double.eps))
    x_cur <- x_cur + k_t %*% (d_mat - h %*% x_cur)
  }

  FLAREr:::apply_da_updates(
    update                       = x_cur,
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
    obs_non_vertical             = obs_non_vertical,
    active_in_xmatrix            = active_in_xmatrix,
    n_non_vertical               = n_non_vertical,
    par_fit_method               = par_fit_method,
    inflation_start              = inflation_start,
    lake_max_depth               = lake_max_depth,
    nmembers                     = nmembers,
    nstates                      = nstates,
    ndepths_modeled              = ndepths_modeled,
    npars                        = npars
  )
}
