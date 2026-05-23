#' @title Diagnose parameter estimation performance in the EnKF
#' @details For each estimated parameter in the augmented state vector, prints:
#'
#'   - Pre-update ensemble mean and SD.
#'
#'   - Net update decomposed into PHY_TCHLA contribution and all other active
#'     observations.  If \code{net_upd} is dominated by \code{other_upd}, the
#'     parameter is being driven mainly by temperature or oxygen obs rather than
#'     chlorophyll.
#'
#'   - Top state-depth covariances \code{Cov(par, state_depth)}: the pairs with
#'     the largest ensemble cross-covariance identify what is driving the
#'     parameter update.  The \code{obs} column shows which observation activates
#'     each covariance path:
#'     \itemize{
#'       \item \code{PHY_TCHLA} — state maps to a composite chlorophyll obs row
#'       \item \code{other}     — state is seen by a different obs type this step
#'       \item \code{unobserved} — state has no active obs row; covariance exists
#'         in the ensemble but cannot be used by the filter this step
#'     }
#'
#'   - Top active observation contributions \code{K[par,obs] * innov}: the
#'     individual observations (ranked by |contribution|) that produce
#'     \code{net_upd}.  Each row shows the observation label inferred from the
#'     H matrix, the Kalman gain entry for this parameter, the mean innovation,
#'     and the resulting contribution.  When high-covariance states are all
#'     \code{unobserved}, the dominant contributions here come from observations
#'     of different variable types (e.g. temperature driving a sediment-oxygen
#'     parameter), which may indicate spurious cross-variable updating.
#'
#' @section Enabling:
#'   Set \code{options(flare.enkf.debug_phy = TRUE)} before calling
#'   \code{run_flare()}.  Output goes to \code{message()}.
#'
#' @param x_matrix numeric matrix \[nstate_cols x nmembers\]
#' @param h numeric matrix \[nobs_active x nstate_cols\]
#' @param k_t numeric matrix \[nstate_cols x nobs_active\] — Kalman gain
#' @param d_mat numeric matrix \[nobs_active x nmembers\]
#' @param ndepths_modeled integer
#' @param states_config data frame with a \code{state_names} column
#' @param pars_config data frame with a \code{par_names} column
#' @param n_non_vertical integer number of non-vertical state variables
#'   preceding the parameter block in the augmented state vector
#' @param npars integer number of estimated parameters
#' @param n_top integer number of top covariances/contributions to show per
#'   parameter (default 5)
#' @return invisibly NULL
#' @keywords internal

.diagnose_enkf_par_update <- function(x_matrix, h, k_t, d_mat,
                                       ndepths_modeled, states_config,
                                       pars_config, n_non_vertical, npars,
                                       n_top = 5L) {

  if (isTRUE(npars == 0L) || is.null(pars_config)) return(invisible(NULL))

  state_names  <- states_config$state_names
  nstates      <- length(state_names)
  n_state_cols <- nstates * ndepths_modeled
  nmembers     <- ncol(x_matrix)
  nobs_active  <- nrow(h)

  par_row_start <- n_state_cols + n_non_vertical + 1L
  par_rows      <- par_row_start + seq_len(npars) - 1L

  if (max(par_rows) > nrow(x_matrix)) return(invisible(NULL))

  par_names_vec <- if (!is.null(pars_config$par_names)) {
    pars_config$par_names[seq_len(npars)]
  } else {
    sprintf("par_%d", seq_len(npars))
  }

  # Full innovation vector (all active obs)
  hx         <- h %*% x_matrix
  innov_full <- rowMeans(d_mat) - rowMeans(hx)

  # Identify composite PHY_TCHLA obs rows so the update can be decomposed
  phy_idx      <- grep("^PHY_", state_names)
  phy_cols_all <- unlist(lapply(phy_idx, function(k)
    (k - 1) * ndepths_modeled + seq_len(ndepths_modeled)))
  phy_cols_all    <- phy_cols_all[phy_cols_all <= ncol(h)]
  composite_rows  <- if (length(phy_cols_all) > 0L) {
    which(rowSums(h[, phy_cols_all, drop = FALSE] != 0) > 1L)
  } else {
    integer(0L)
  }

  # For each state-depth column: which obs type sees it?
  obs_type_for_col <- vapply(seq_len(n_state_cols), function(col) {
    active_rows <- which(h[, col] != 0)
    if (length(active_rows) == 0L)          return("unobserved")
    if (any(active_rows %in% composite_rows)) return("PHY_TCHLA")
    "other"
  }, character(1L))

  # Label each active obs row from the H matrix structure.
  # Composite PHY rows → "PHY_TCHLA_dD"; single-state rows → "STATE_dD";
  # non-vertical rows (no nonzero state columns) → "non_vert_N".
  obs_labels <- vapply(seq_len(nobs_active), function(r) {
    nz_state <- which(h[r, seq_len(n_state_cols), drop = FALSE] != 0)
    if (length(nz_state) == 0L) {
      nv_idx <- which(h[r, seq(n_state_cols + 1L,
                               n_state_cols + n_non_vertical), drop = FALSE] != 0)
      return(if (length(nv_idx) > 0L) sprintf("non_vert_%d", nv_idx[1L])
             else sprintf("obs_%d", r))
    }
    if (r %in% composite_rows) {
      didx <- (nz_state[1L] - 1L) %% ndepths_modeled + 1L
      return(sprintf("PHY_TCHLA_d%d", didx))
    }
    col   <- nz_state[1L]
    sidx  <- (col - 1L) %/% ndepths_modeled + 1L
    didx  <- (col - 1L) %% ndepths_modeled + 1L
    sname <- if (sidx <= nstates) state_names[sidx] else sprintf("state_%d", sidx)
    sprintf("%s_d%d", sname, didx)
  }, character(1L))

  # Pre-compute state anomaly matrix once for all covariance calculations
  a_states <- x_matrix[seq_len(n_state_cols), , drop = FALSE] -
    rowMeans(x_matrix[seq_len(n_state_cols), , drop = FALSE])

  sep <- strrep("-", 82)
  message("\n", sep)
  message("  EnKF Parameter Update Diagnostic")
  message(sep)
  message("  net_upd   = K[par, all_obs] * innov  (actual shift this step)")
  message("  PHY_upd   = contribution from PHY_TCHLA obs only")
  message("  other_upd = contribution from all other simultaneously active obs")
  message(paste0("  Top Cov(par, state_depth): state-depth pairs most correlated",
                 " with the parameter;"))
  message(paste0("  obs column shows which observation activates each path",
                 " (PHY_TCHLA / other / unobserved)"))
  message(paste0("  Top obs contributions: K[par,obs]*innov ranked by |contribution|;",
                 " reveals which obs type is actually driving the update"))

  for (p in seq_along(par_rows)) {
    pr       <- par_rows[p]
    pname    <- par_names_vec[p]
    par_vals <- x_matrix[pr, ]
    pre_mean <- mean(par_vals)
    pre_sd   <- stats::sd(par_vals)

    net_upd <- mean(k_t[pr, , drop = FALSE] %*% innov_full)
    phy_upd <- if (length(composite_rows) > 0L) {
      mean(k_t[pr, composite_rows, drop = FALSE] %*%
             innov_full[composite_rows])
    } else {
      0
    }
    other_upd <- net_upd - phy_upd

    message(sprintf("\n  %s", pname))
    message(sprintf("    pre-update: mean=%+.6f  SD=%.6f", pre_mean, pre_sd))
    message(sprintf(
      "    net_upd=%+.6f  PHY_upd=%+.6f  other_upd=%+.6f",
      net_upd, phy_upd, other_upd))

    # Cov(par, state_i_depth_j) via efficient matrix-vector product
    par_anom <- par_vals - pre_mean
    cov_vec  <- as.vector(a_states %*% par_anom) / (nmembers - 1)

    top_idx <- order(abs(cov_vec), decreasing = TRUE)[
      seq_len(min(n_top, length(cov_vec)))
    ]

    message(sprintf("    %-15s  %6s  %14s  %s",
                    "State", "depth", "Cov(par,state)", "obs"))
    for (idx in top_idx) {
      sidx  <- (idx - 1L) %/% ndepths_modeled + 1L
      didx  <- (idx - 1L) %% ndepths_modeled + 1L
      sname <- if (sidx <= nstates) state_names[sidx] else
        sprintf("state_%d", sidx)
      message(sprintf("    %-15s  %6d  %+14.6f  %s",
                      sname, didx, cov_vec[idx], obs_type_for_col[idx]))
    }

    # Top active observation contributions: K[par,obs] * innov per obs row
    contributions <- as.vector(k_t[pr, ]) * innov_full
    top_obs_idx   <- order(abs(contributions), decreasing = TRUE)[
      seq_len(min(n_top, nobs_active))
    ]

    message(sprintf("    %-20s  %12s  %10s  %14s",
                    "Observation", "K[par,obs]", "innov", "contribution"))
    for (oidx in top_obs_idx) {
      message(sprintf("    %-20s  %+12.6f  %+10.4f  %+14.6f",
                      obs_labels[oidx], k_t[pr, oidx],
                      innov_full[oidx], contributions[oidx]))
    }
  }

  message(strrep("-", 82), "\n")
  invisible(NULL)
}
