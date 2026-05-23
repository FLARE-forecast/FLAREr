#' @title Run Ensemble Kalman filter on model predictions
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
#' @param diagnostics_daily_start daily diagnostics (a restart variable)
#' @param pars_config parameter configuration list
#' @param config FLARE configuration list
#' @param obs_non_vertical named list of non-vertical observation metadata
#' @param active_in_xmatrix character vector of non-vertical variable names
#'   in state-vector order
#' @param n_non_vertical integer number of non-vertical variables in the
#'   augmented state
#' @param par_fit_method method for fixing parameters
#' @param inflation_start covariance inflation factor (a restart variable)
#' @param lake_max_depth maximum lake depth
#' @param states_config data frame of state configuration (used by the PHY
#'   update diagnostic; see \code{options(flare.enkf.debug_phy = TRUE)})
#' @param obs_diag_meta optional named list with \code{variable} (character)
#'   and \code{depth} (numeric) vectors of length \code{length(z_index)},
#'   giving the obs type name and depth for each active observation.  Only
#'   used when \code{config$da_setup$save_da_diagnostics} is \code{TRUE}.
#' @noRd
#'
#' @return list of updated model states, diagnostics, and parameters.  When
#'   \code{config$da_setup$save_da_diagnostics} is \code{TRUE} the list also
#'   contains a \code{da_diag} element (a named list of raw filter-health
#'   quantities for this assimilation step).
run_enkf <- function(x_matrix,
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
                     lake_max_depth,
                     states_config = NULL,
                     obs_diag_meta = NULL) {

  if (!is.null(pars_config)) {
    npars <- dim(pars_corr)[1]
  } else {
    npars <- 0
  }
  nmembers        <- dim(states_depth_start)[3]
  nstates         <- dim(states_depth_start)[1]
  ndepths_modeled <- length(config$model_settings$modeled_depths)

  curr_psi <- psi[z_index]^2

  if (length(z_index) > 1) {
    psi_t <- diag(curr_psi)
  } else {
    psi_t <- curr_psi
  }

  d_mat <- t(mvtnorm::rmvnorm(n = nmembers, mean = zt,
                              sigma = as.matrix(psi_t)))

  if (isTRUE(config$da_setup$log_transform_wq_obs)) {
    wq_rows <- which(z_index > ndepths_modeled)
    if (length(wq_rows) > 0) {
      pos_rows  <- wq_rows[zt[wq_rows] > 0]
      zero_rows <- wq_rows[zt[wq_rows] <= 0]
      for (row_idx in pos_rows) {
        yt_i      <- zt[row_idx]
        sig_i     <- sqrt(curr_psi[row_idx])
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

  ens_mean <- rowMeans(x_matrix)
  a_mat    <- x_matrix - ens_mean
  p_t      <- a_mat %*% t(a_mat) / (nmembers - 1)

  if (!is.null(config$da_setup$localization_distance) &&
        !is.na(config$da_setup$localization_distance)) {
    p_t <- localization(
      mat                   = p_t,
      nstates               = nstates,
      modeled_depths        = config$model_settings$modeled_depths,
      localization_distance = config$da_setup$localization_distance,
      num_single_states     = dim(p_t)[1] - nstates * ndepths_modeled
    )
  }

  # Kalman gain: solve directly to avoid forming the explicit inverse.
  s_mat <- h %*% p_t %*% t(h) + psi_t
  k_t   <- t(solve(s_mat, h %*% p_t, tol = .Machine$double.eps))

  if (isTRUE(getOption("flare.enkf.debug_phy")) && !is.null(states_config)) {
    .diagnose_enkf_phy_update(
      x_matrix        = x_matrix,
      h               = h,
      k_t             = k_t,
      d_mat           = d_mat,
      ndepths_modeled = ndepths_modeled,
      states_config   = states_config
    )
    if (isTRUE(npars > 0L)) {
      .diagnose_enkf_par_update(
        x_matrix        = x_matrix,
        h               = h,
        k_t             = k_t,
        d_mat           = d_mat,
        ndepths_modeled = ndepths_modeled,
        states_config   = states_config,
        pars_config     = pars_config,
        n_non_vertical  = n_non_vertical,
        npars           = npars
      )
    }
  }

  update <- x_matrix + k_t %*% (d_mat - h %*% x_matrix)

  result <- apply_da_updates(
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

  if (isTRUE(config$da_setup$save_da_diagnostics) && length(zt) > 0L) {
    result$da_diag <- .compute_enkf_da_diag(
      x_matrix        = x_matrix,
      update          = update,
      h               = h,
      k_t             = k_t,
      s_mat           = s_mat,
      zt              = zt,
      curr_psi        = curr_psi,
      ens_mean        = ens_mean,
      nstates         = nstates,
      ndepths_modeled = ndepths_modeled,
      n_non_vertical  = n_non_vertical,
      npars           = npars,
      inflation_start = inflation_start,
      pars_config     = pars_config,
      states_config   = states_config,
      obs_diag_meta   = obs_diag_meta
    )
  }

  result
}

#' Compute raw EnKF diagnostic quantities for one assimilation step
#'
#' All quantities are computed from objects that already exist at the call
#' site in \code{run_enkf}, so there is no redundant matrix work.
#'
#' @param x_matrix  prior ensemble `[n_cols x nmembers]`
#' @param update    posterior ensemble `[n_cols x nmembers]` (before bound clipping)
#' @param h         observation operator `[n_obs x n_cols]`
#' @param k_t       Kalman gain `[n_cols x n_obs]`
#' @param s_mat     innovation covariance H*P*H^T + R `[n_obs x n_obs]`
#' @param zt        active observation vector `[n_obs]`
#' @param curr_psi  observation variances `[n_obs]` (= `psi[z_index]^2`)
#' @param ens_mean  prior ensemble row means `[n_cols]`
#' @param nstates,ndepths_modeled,n_non_vertical,npars  dimension integers
#' @param inflation_start  covariance inflation factor applied this step
#' @param pars_config,states_config  config data frames (may be NULL)
#' @param obs_diag_meta  named list(variable=, depth=) pre-built by caller
#' @return named list of raw diagnostic scalars/vectors
#' @keywords internal
.compute_enkf_da_diag <- function(x_matrix, update, h, k_t, s_mat,
                                   zt, curr_psi, ens_mean,
                                   nstates, ndepths_modeled,
                                   n_non_vertical, npars,
                                   inflation_start,
                                   pars_config, states_config,
                                   obs_diag_meta) {
  n_obs      <- length(zt)
  n_state_cols <- nstates * ndepths_modeled

  # --- Predicted observations and innovation ---
  hx      <- h %*% x_matrix           # [n_obs x nmembers]
  hx_mean <- rowMeans(hx)
  hx_sd   <- apply(hx, 1L, stats::sd)
  innov   <- zt - hx_mean

  # --- Per-obs NIS: innov^2 / (HPH^T_diag + R_diag) ---
  # HPH^T_diag is the diagonal of s_mat - R
  hpht_diag <- diag(as.matrix(s_mat)) - curr_psi
  hpht_diag <- pmax(hpht_diag, 0)  # numerical safety
  nis_single <- innov^2 / pmax(hpht_diag + curr_psi, .Machine$double.eps)

  # --- Total NIS (chi-squared / n_obs): innovation^T * S^-1 * innovation / n_obs ---
  nis_total <- NA_real_
  tryCatch({
    s_inv_innov <- solve(s_mat, innov, tol = .Machine$double.eps)
    nis_total   <- as.numeric(t(innov) %*% s_inv_innov) / n_obs
  }, error = function(e) {
    nis_total <<- NA_real_
  })

  # --- Observation influence and DFS: diagonal of H * K ---
  # diag(H %*% K) = rowSums(H * t(K)) -- avoids forming the full n_obs x n_obs matrix
  obs_influence <- rowSums(h * t(k_t))
  dfs           <- sum(obs_influence)

  # --- Rank of each observation within its prior ensemble column ---
  obs_rank <- vapply(seq_len(n_obs), function(r) {
    sum(hx[r, ] < zt[r]) + 1L
  }, integer(1L))

  # --- Prior and posterior spread for states ---
  prior_sd <- apply(x_matrix[seq_len(n_state_cols), , drop = FALSE], 1L, stats::sd)
  post_sd  <- apply(update[seq_len(n_state_cols), , drop = FALSE],   1L, stats::sd)
  prior_mean_states <- ens_mean[seq_len(n_state_cols)]
  post_mean_states  <- rowMeans(update[seq_len(n_state_cols), , drop = FALSE])

  # --- Find composite PHY_TCHLA obs rows (shared by parameter and covariance diagnostics) ---
  composite_rows <- integer(0L)
  phy_state_idx  <- integer(0L)
  if (!is.null(states_config)) {
    phy_state_idx <- grep("^PHY_", states_config$state_names)
    if (length(phy_state_idx) > 0L) {
      phy_cols_tmp <- unlist(lapply(phy_state_idx, function(ki)
        (ki - 1L) * ndepths_modeled + seq_len(ndepths_modeled)))
      phy_cols_tmp <- phy_cols_tmp[phy_cols_tmp <= ncol(h)]
      if (length(phy_cols_tmp) > 0L) {
        composite_rows <- which(
          rowSums(h[, phy_cols_tmp, drop = FALSE] != 0) > 1L
        )
      }
    }
  }

  # --- Parameter diagnostics (only when pars are in x_matrix) ---
  pars_in_x <- (nrow(x_matrix) == n_state_cols + n_non_vertical + npars) && npars > 0L
  par_prior_mean <- par_prior_sd <- par_post_mean <- par_post_sd <- NULL
  par_net_update <- par_phy_contribution <- par_other_contribution <- NULL
  par_frac_lower <- par_frac_upper <- NULL

  if (pars_in_x) {
    par_rows   <- seq(n_state_cols + n_non_vertical + 1L,
                      n_state_cols + n_non_vertical + npars)
    par_prior  <- x_matrix[par_rows, , drop = FALSE]
    par_post   <- update[par_rows, , drop = FALSE]

    par_prior_mean <- rowMeans(par_prior)
    par_prior_sd   <- apply(par_prior, 1L, stats::sd)
    par_post_mean  <- rowMeans(par_post)
    par_post_sd    <- apply(par_post, 1L, stats::sd)

    par_net_update <- as.numeric(k_t[par_rows, , drop = FALSE] %*% innov)

    if (length(composite_rows) > 0L) {
      par_phy_contribution <- as.numeric(
        k_t[par_rows, composite_rows, drop = FALSE] %*% innov[composite_rows]
      )
    } else {
      par_phy_contribution <- rep(0, npars)
    }
    par_other_contribution <- par_net_update - par_phy_contribution

    if (!is.null(pars_config)) {
      par_frac_lower <- vapply(seq_len(npars), function(p) {
        mean(par_post[p, ] < pars_config$par_lowerbound[p])
      }, numeric(1L))
      par_frac_upper <- vapply(seq_len(npars), function(p) {
        mean(par_post[p, ] > pars_config$par_upperbound[p])
      }, numeric(1L))
    }
  }

  # --- Cross-state covariance and cross-depth update structure ---
  cross_state_names    <- character(0L)
  cross_state_cov      <- numeric(0L)
  cross_state_mean_k   <- numeric(0L)
  cross_state_mean_upd <- numeric(0L)
  cross_state_sign_ok  <- logical(0L)

  cross_depth_phy  <- character(0L)
  cross_depth_ridx <- integer(0L)
  cross_depth_jidx <- integer(0L)
  cross_depth_odep <- numeric(0L)
  cross_depth_val  <- numeric(0L)

  if (!is.null(states_config) && length(composite_rows) > 0L) {
    hx_sub      <- hx[composite_rows, , drop = FALSE]
    innov_cs    <- innov[composite_rows]
    hx_anom_avg <- colMeans(sweep(hx_sub, 1L, rowMeans(hx_sub), `-`))

    comp_obs_depths <- if (!is.null(obs_diag_meta) &&
                           length(obs_diag_meta$depth) >= max(composite_rows)) {
      obs_diag_meta$depth[composite_rows]
    } else {
      rep(NA_real_, length(composite_rows))
    }

    coupled_patterns_cs <- c("^NIT_", "^PHS_", "^SIL_", "^OXY_", "^CAR_", "^OGM_")
    coupled_idx_cs <- setdiff(
      grep(paste(coupled_patterns_cs, collapse = "|"), states_config$state_names),
      phy_state_idx
    )

    for (k in coupled_idx_cs) {
      col_range <- (k - 1L) * ndepths_modeled + seq_len(ndepths_modeled)
      if (max(col_range) > nrow(x_matrix) || max(col_range) > nrow(k_t)) next
      state_anom_avg <- colMeans(
        sweep(x_matrix[col_range, , drop = FALSE],
              1L, rowMeans(x_matrix[col_range, , drop = FALSE]), `-`)
      )
      cov_val  <- sum(state_anom_avg * hx_anom_avg) / (ncol(x_matrix) - 1L)
      k_sub    <- k_t[col_range, composite_rows, drop = FALSE]
      mean_k   <- mean(k_sub)
      upd_mean <- mean(rowSums(sweep(k_sub, 2L, innov_cs, `*`)))
      sign_ok  <- if (abs(cov_val) < 1e-10 || abs(mean_k) < 1e-12) {
        NA
      } else {
        sign(mean_k) == sign(cov_val)
      }
      cross_state_names    <- c(cross_state_names,    states_config$state_names[k])
      cross_state_cov      <- c(cross_state_cov,      cov_val)
      cross_state_mean_k   <- c(cross_state_mean_k,   mean_k)
      cross_state_mean_upd <- c(cross_state_mean_upd, upd_mean)
      cross_state_sign_ok  <- c(cross_state_sign_ok,  sign_ok)
    }

    for (k in phy_state_idx) {
      col_range <- (k - 1L) * ndepths_modeled + seq_len(ndepths_modeled)
      if (max(col_range) > nrow(k_t)) next
      k_sub   <- k_t[col_range, composite_rows, drop = FALSE]
      upd_mat <- t(sweep(k_sub, 2L, innov_cs, `*`))  # [n_comp x ndepths]
      n_comp  <- length(composite_rows)
      for (r in seq_len(n_comp)) {
        for (j in seq_len(ndepths_modeled)) {
          cross_depth_phy  <- c(cross_depth_phy,  states_config$state_names[k])
          cross_depth_ridx <- c(cross_depth_ridx, r)
          cross_depth_jidx <- c(cross_depth_jidx, j)
          cross_depth_odep <- c(cross_depth_odep, comp_obs_depths[r])
          cross_depth_val  <- c(cross_depth_val,  upd_mat[r, j])
        }
      }
    }
  }

  list(
    n_obs                  = n_obs,
    nis                    = nis_total,
    dfs                    = dfs,
    inflation_applied      = inflation_start,
    obs_variable           = if (!is.null(obs_diag_meta)) obs_diag_meta$variable else NULL,
    obs_depth              = if (!is.null(obs_diag_meta)) obs_diag_meta$depth    else NULL,
    obs_value              = zt,
    hx_mean                = hx_mean,
    hx_sd                  = hx_sd,
    obs_sd                 = sqrt(curr_psi),
    innov                  = innov,
    nis_single             = nis_single,
    obs_influence          = obs_influence,
    obs_rank               = obs_rank,
    prior_mean_states      = prior_mean_states,
    prior_sd_states        = prior_sd,
    post_mean_states       = post_mean_states,
    post_sd_states         = post_sd,
    nstates                = nstates,
    ndepths_modeled        = ndepths_modeled,
    pars_in_x              = pars_in_x,
    npars                  = npars,
    par_prior_mean         = par_prior_mean,
    par_prior_sd           = par_prior_sd,
    par_post_mean          = par_post_mean,
    par_post_sd            = par_post_sd,
    par_net_update         = par_net_update,
    par_phy_contribution   = par_phy_contribution,
    par_other_contribution = par_other_contribution,
    par_frac_lower         = par_frac_lower,
    par_frac_upper         = par_frac_upper,
    cross_state = list(
      state_names  = cross_state_names,
      cov_with_phy = cross_state_cov,
      mean_k       = cross_state_mean_k,
      mean_update  = cross_state_mean_upd,
      sign_ok      = cross_state_sign_ok
    ),
    cross_depth = list(
      phy_state    = cross_depth_phy,
      obs_idx      = cross_depth_ridx,
      state_idx    = cross_depth_jidx,
      obs_depth    = cross_depth_odep,
      contribution = cross_depth_val
    )
  )
}
