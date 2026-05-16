#' @title Diagnose why PHY_TCHLA may not be updating individual phytoplankton states
#' @details Prints diagnostics at each DA step (when enabled) covering:
#'
#'   1. **H coefficients** — mapping coefficients the H matrix is actually
#'      using for each PHY state.  Should match 12/Xcc from aed.nml.
#'
#'   2. **Ensemble spread** — pre-update SD of each PHY state.  Near-zero SD
#'      means the ensemble has collapsed; the filter cannot update without spread.
#'
#'   3. **Innovation & predicted obs** — mean observed value, mean H*x, ensemble
#'      SD of H*x, and innovation (obs - H*x).  Near-zero SD of H*x means K~0
#'      regardless of obs_sd.  Near-zero innovation means no update is needed.
#'
#'   4. **Kalman gain & update** — |K| and K*innovation for each PHY state.
#'
#'   5. **Cross-state updates** — nutrients and other biologically coupled states
#'      (NIT, PHS, SIL, OXY, CAR, OGM) driven by PHY_TCHLA obs via ensemble
#'      covariance.  Cov sign reflects the current bloom phase, not a fixed rule.
#'
#'   6. **Update decomposition** — PHY_TCHLA contribution vs all other active obs.
#'
#'   7. **Cross-depth structure** — per-obs-depth update profile across state
#'      depths, with cross-depth spreading percentage.
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
#' @param states_config data frame with \code{state_names},
#'   \code{states_to_obs}, and \code{states_to_obs_mapping} list columns
#' @return invisibly NULL
#' @keywords internal

.diagnose_enkf_phy_update <- function(x_matrix, h, k_t, d_mat,
                                       ndepths_modeled, states_config) {

  state_names <- states_config$state_names
  phy_idx     <- grep("^PHY_", state_names)
  if (length(phy_idx) == 0L) return(invisible(NULL))

  phy_cols_all <- unlist(lapply(phy_idx, function(k)
    (k - 1) * ndepths_modeled + seq_len(ndepths_modeled)))
  phy_cols_all <- phy_cols_all[phy_cols_all <= ncol(h)]

  # na.rm = TRUE: NA entries in h (depths above lake surface) must not
  # propagate to NA and break the if() guard.
  if (!isTRUE(any(h[, phy_cols_all] != 0, na.rm = TRUE))) {
    return(invisible(NULL))
  }

  sep <- strrep("-", 82)
  message("\n", sep)
  message("  EnKF PHY Update Diagnostic")
  message(sep)

  # 1. H coefficients actually used for each PHY state ----------------------
  message("\n[1] H matrix coefficients in use for PHY states:")
  message("    (Should match 12/Xcc from aed.nml)")
  for (k in phy_idx) {
    col_range <- (k - 1) * ndepths_modeled + seq_len(ndepths_modeled)
    if (max(col_range) > ncol(h)) next
    h_sub <- h[, col_range, drop = FALSE]
    nz    <- which(h_sub != 0, arr.ind = TRUE)
    if (nrow(nz) == 0) {
      message(sprintf(
        "  %-15s  NO nonzero H entries -- state invisible to all obs",
        state_names[k]))
    } else {
      vals <- sort(unique(round(h_sub[nz], 6)))
      message(sprintf("  %-15s  %s  (across %d obs-depth rows)",
                      state_names[k], paste(vals, collapse = ", "), nrow(nz)))
    }
  }

  # 2. Ensemble spread of each PHY state (pre-update) -----------------------
  message("\n[2] PHY state ensemble SD before update:")
  for (k in phy_idx) {
    col_range <- (k - 1) * ndepths_modeled + seq_len(ndepths_modeled)
    if (max(col_range) > nrow(x_matrix)) next
    sds <- apply(x_matrix[col_range, , drop = FALSE], 1L, stats::sd)
    message(sprintf("  %-15s  mean_sd=%8.4f  max_sd=%8.4f  min_sd=%8.4f",
                    state_names[k], mean(sds), max(sds), min(sds)))
  }

  composite_rows <- which(
    rowSums(h[, phy_cols_all, drop = FALSE] != 0) > 1L
  )

  if (length(composite_rows) == 0L) {
    message("\n[3] No composite PHY obs rows active at this time step")
    message(strrep("-", 82), "\n")
    return(invisible(NULL))
  }

  hx         <- h %*% x_matrix
  hx_sub     <- hx[composite_rows, , drop = FALSE]
  hx_mean    <- rowMeans(hx_sub)
  hx_sd      <- apply(hx_sub, 1L, stats::sd)
  obs_mean   <- rowMeans(d_mat[composite_rows, , drop = FALSE])
  innov      <- obs_mean - hx_mean
  innov_full <- rowMeans(d_mat) - rowMeans(hx)

  # 3. Innovation and predicted obs spread -----------------------------------
  message(sprintf(
    "\n[3] Composite PHY_TCHLA obs (%d active depth(s)):",
    length(composite_rows)))
  message(sprintf(
    "    Mean observed value : mean=%.4f  max=%.4f  min=%.4f",
    mean(obs_mean), max(obs_mean), min(obs_mean)))
  message(sprintf(
    "    Mean H*x (predicted): mean=%.4f  max=%.4f  min=%.4f",
    mean(hx_mean), max(hx_mean), min(hx_mean)))
  message(sprintf(
    "    Ensemble SD of H*x : mean=%.4f  max=%.4f  min=%.4f",
    mean(hx_sd), max(hx_sd), min(hx_sd)))
  message(sprintf(
    "    Innovation (obs-Hx): mean=%+.4f  max=%+.4f  min=%+.4f",
    mean(innov), max(innov), min(innov)))
  message("    (Near-zero SD of H*x => K~0 regardless of obs_sd)")
  message("    (Near-zero innovation => ensemble matches obs; no update)")

  # 4. Kalman gain and expected state update ---------------------------------
  message("\n[4] Kalman gain and expected update for PHY states:")
  message(sprintf("  %-15s  %14s  %14s  %14s",
                  "State", "mean|K|", "max|K|", "mean_update"))
  for (k in phy_idx) {
    col_range <- (k - 1) * ndepths_modeled + seq_len(ndepths_modeled)
    if (max(col_range) > nrow(k_t)) next
    k_sub    <- k_t[col_range, composite_rows, drop = FALSE]
    upd_mean <- mean(rowSums(sweep(k_sub, 2L, innov, `*`)))
    message(sprintf("  %-15s  %14.6f  %14.6f  %+14.4f",
                    state_names[k],
                    mean(abs(k_sub)), max(abs(k_sub)), upd_mean))
  }

  # 5. Cross-state updates: states biologically coupled to PHY ---------------
  # The sign of Cov(nutrient, PHY_TCHLA_Hx) depends on bloom phase:
  #
  #   Cov < 0 (drawdown): PHY is consuming nutrients -- members with high
  #     PHY have drawn down NIT/PHS.  Assimilating PHY downward pushes
  #     nutrients upward.
  #
  #   Cov > 0 (growth phase): high nutrients are fuelling high PHY across
  #     members.  Assimilating PHY downward will also push nutrients down
  #     (those members had less nutrient uptake driving them).
  #
  #   Cov ~ 0: no ensemble coupling -- obs cannot propagate to nutrients.
  #     Check model_sd for nutrients and ensemble spinup time.
  #
  # sign_ok: "NO" when sign(K) != sign(Cov) -- a numerical or structural
  # problem in the gain, not a biological expectation violation.

  coupled_patterns <- c("^NIT_", "^PHS_", "^SIL_", "^OXY_", "^CAR_", "^OGM_")
  coupled_idx <- grep(paste(coupled_patterns, collapse = "|"), state_names)
  coupled_idx <- setdiff(coupled_idx, phy_idx)

  if (length(coupled_idx) > 0L) {
    message("\n[5] Cross-state updates driven by PHY_TCHLA obs:")
    message("    Cov sign depends on bloom phase:")
    message("      Cov < 0: drawdown -- PHY consuming nuts (high PHY, low nuts)")
    message("      Cov > 0: growth   -- nuts fuelling PHY (high nuts, high PHY)")
    message("      Cov ~ 0: no ensemble coupling -- obs cannot reach nutrients")
    message("    sign_ok = 'NO': sign(K) != sign(Cov), numerical/structural problem")
    message(sprintf("  %-15s  %16s  %12s  %12s  %s",
                    "State", "Cov(st,PHY_Hx)", "mean K",
                    "mean_update", "sign_ok"))

    hx_anom_avg <- colMeans(sweep(hx_sub, 1L, rowMeans(hx_sub), `-`))

    for (k in coupled_idx) {
      col_range <- (k - 1) * ndepths_modeled + seq_len(ndepths_modeled)
      if (max(col_range) > nrow(x_matrix) || max(col_range) > nrow(k_t)) next

      state_anom_avg <- colMeans(
        sweep(x_matrix[col_range, , drop = FALSE],
              1L, rowMeans(x_matrix[col_range, , drop = FALSE]), `-`)
      )
      cov_val  <- sum(state_anom_avg * hx_anom_avg) / (ncol(x_matrix) - 1)
      k_sub    <- k_t[col_range, composite_rows, drop = FALSE]
      mean_k   <- mean(k_sub)
      upd_mean <- mean(rowSums(sweep(k_sub, 2L, innov, `*`)))

      sign_ok <- if (abs(cov_val) < 1e-10 || abs(mean_k) < 1e-12) {
        "N/A"
      } else if (sign(mean_k) == sign(cov_val)) {
        "yes"
      } else {
        "NO"
      }
      message(sprintf("  %-15s  %16.6f  %12.6f  %+12.4f  %s",
                      state_names[k], cov_val, mean_k, upd_mean, sign_ok))
    }

    message(paste0(
      "\n  Key pattern to look for:\n",
      "  If PHY groups are assimilated downward (negative innovation in [3]) but\n",
      "  NIT_amm / NIT_nit / PHS_frp show mean_update near zero or wrong sign,\n",
      "  the ensemble cross-covariance between PHY and nutrients is missing.\n",
      "  Check: (1) model_sd for nutrient states -- near-zero suppresses covariance;\n",
      "         (2) Cov ~ 0 -- nutrients and PHY moving independently;\n",
      "         (3) sign_ok = 'NO' -- wrong-sign K, check H matrix."
    ))

    # 6. Decompose nutrient update: PHY_TCHLA vs all other active obs --------
    message("\n[6] Update decomposition: PHY_TCHLA obs vs all other obs:")
    message("    PHY_upd   = PHY_TCHLA contribution only (= [5] mean_update)")
    message("    other_upd = all other obs assimilated this step")
    message("    total_upd = actual net shift applied to this state")
    message(sprintf("  %-15s  %12s  %12s  %12s",
                    "State", "PHY_upd", "other_upd", "total_upd"))

    for (k in coupled_idx) {
      col_range <- (k - 1) * ndepths_modeled + seq_len(ndepths_modeled)
      if (max(col_range) > nrow(k_t)) next
      phy_upd   <- mean(
        k_t[col_range, composite_rows, drop = FALSE] %*%
          innov_full[composite_rows]
      )
      total_upd <- mean(k_t[col_range, , drop = FALSE] %*% innov_full)
      other_upd <- total_upd - phy_upd
      message(sprintf("  %-15s  %+12.4f  %+12.4f  %+12.4f",
                      state_names[k], phy_upd, other_upd, total_upd))
    }
  }

  # 7. Cross-depth update structure for PHY groups ---------------------------
  # Rows = active PHY_TCHLA obs depths, columns = modeled state depths.
  # Cell[r,j] = K[PHY_state_depth_j, obs_depth_r] * innov[r].
  # Same-depth entries should dominate when localization is active.
  # cross% = fraction of total update at state depth j from obs at depths
  #   other than the one with the largest |K| for that state depth.

  message("\n[7] Cross-depth update structure for PHY states:")
  message(paste0("    rows=obs depths  cols=state depths  ",
                 "cell=K[st_d,obs_d]*innov"))
  message("    cross% = update fraction from non-primary obs depths")

  for (k in phy_idx) {
    col_range <- (k - 1) * ndepths_modeled + seq_len(ndepths_modeled)
    if (max(col_range) > nrow(k_t)) next

    k_sub      <- k_t[col_range, composite_rows, drop = FALSE]
    upd_matrix <- t(sweep(k_sub, 2L, innov, `*`))
    total_upd  <- colSums(upd_matrix)
    ens_sd     <- apply(x_matrix[col_range, , drop = FALSE], 1L, stats::sd)

    message(sprintf("\n  %s:", state_names[k]))
    depth_hdr <- paste(sprintf("  d%-3d", seq_len(ndepths_modeled)), collapse = "")
    message(sprintf("  %-24s%s", "", depth_hdr))

    n_obs <- length(composite_rows)
    for (r in seq_len(n_obs)) {
      vals_str <- paste(sprintf("%+5.3f", upd_matrix[r, ]), collapse = "  ")
      message(sprintf("  obs_d%d (innov=%+6.3f):   %s", r, innov[r], vals_str))
    }

    total_str <- paste(sprintf("%+5.3f", total_upd), collapse = "  ")
    message(sprintf("  %-24s%s", "total_upd:", total_str))

    sd_str <- paste(sprintf(" %4.3f", ens_sd), collapse = "  ")
    message(sprintf("  %-24s%s", "ens_SD:", sd_str))

    if (n_obs > 1L) {
      cross_pct <- vapply(seq_len(ndepths_modeled), function(j) {
        primary_r <- which.max(abs(k_sub[j, ]))
        if (abs(total_upd[j]) < 1e-10) return(0)
        abs(total_upd[j] - upd_matrix[primary_r, j]) / abs(total_upd[j]) * 100
      }, numeric(1L))
      cross_str <- paste(sprintf(" %3.0f%%", cross_pct), collapse = "  ")
      message(sprintf("  %-24s%s", "cross_depth%:", cross_str))
    }
  }

  message(strrep("-", 82), "\n")
  invisible(NULL)
}
