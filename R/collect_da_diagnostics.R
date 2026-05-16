#' @title Format raw EnKF diagnostics into tidy tibbles for one DA step
#'
#' @description Takes the \code{da_diag} list produced by \code{.compute_enkf_da_diag}
#'   plus step metadata and returns five tidy tibbles ready to accumulate and
#'   write to disk.
#'
#' @param da_diag     named list returned by \code{.compute_enkf_da_diag}
#' @param time        POSIXct datetime of this assimilation step
#' @param states_config  data frame with at least a \code{state_names} column
#' @param pars_config    data frame with \code{par_names_save} column (may be NULL)
#' @param modeled_depths numeric vector of modelled depths
#'
#' @return named list with elements \code{filter_health}, \code{innovations},
#'   \code{ensemble_spread}, \code{parameters} (NULL when no pars), \code{ranks}.
#'   Each element is a \code{\link[tibble]{tibble}}.
#' @keywords internal
collect_da_diagnostics <- function(da_diag, time, states_config,
                                   pars_config, modeled_depths) {

  nstates         <- da_diag$nstates
  ndepths         <- da_diag$ndepths_modeled
  n_state_cols    <- nstates * ndepths
  state_names_vec <- states_config$state_names

  # ── 1. Filter health (one row per DA step) ────────────────────────────────
  nis_val  <- da_diag$nis
  n_obs    <- da_diag$n_obs
  nis_pval <- if (!is.na(nis_val) && n_obs > 0L) {
    stats::pchisq(nis_val * n_obs, df = n_obs, lower.tail = FALSE)
  } else {
    NA_real_
  }

  filter_health <- tibble::tibble(
    time              = time,
    n_obs             = n_obs,
    nis               = nis_val,
    nis_pval          = nis_pval,
    dfs               = da_diag$dfs,
    inflation_applied = da_diag$inflation_applied
  )

  # ── 2. Innovations (one row per active observation) ───────────────────────
  innovations <- tibble::tibble(
    time          = time,
    variable      = if (!is.null(da_diag$obs_variable)) da_diag$obs_variable
                    else paste0("obs_", seq_len(n_obs)),
    depth         = if (!is.null(da_diag$obs_depth)) da_diag$obs_depth
                    else rep(NA_real_, n_obs),
    obs_value     = da_diag$obs_value,
    prior_hx_mean = da_diag$hx_mean,
    prior_hx_sd   = da_diag$hx_sd,
    innovation    = da_diag$innov,
    obs_sd        = da_diag$obs_sd,
    nis_single    = da_diag$nis_single,
    obs_influence = da_diag$obs_influence,
    obs_rank      = da_diag$obs_rank
  )

  # ── 3. Ensemble spread (one row per state × depth) ────────────────────────
  # x_matrix rows are ordered: state1_d1, state1_d2, ..., state1_dN, state2_d1, ...
  state_names_rep <- rep(state_names_vec, each = ndepths)
  depths_rep      <- rep(modeled_depths,  times = nstates)

  spread_ratio <- da_diag$post_sd_states /
    pmax(da_diag$prior_sd_states, .Machine$double.eps)

  ensemble_spread <- tibble::tibble(
    time             = time,
    variable         = state_names_rep,
    depth            = depths_rep,
    prior_mean       = da_diag$prior_mean_states,
    prior_sd         = da_diag$prior_sd_states,
    posterior_mean   = da_diag$post_mean_states,
    posterior_sd     = da_diag$post_sd_states,
    update_magnitude = abs(da_diag$post_mean_states - da_diag$prior_mean_states),
    spread_ratio     = spread_ratio
  )

  # ── 4. Parameters (one row per parameter) ─────────────────────────────────
  parameters <- NULL
  if (isTRUE(da_diag$pars_in_x) && da_diag$npars > 0L &&
      !is.null(da_diag$par_prior_mean)) {

    npars   <- da_diag$npars
    par_names_save <- if (!is.null(pars_config) &&
                          !is.null(pars_config$par_names_save)) {
      pars_config$par_names_save[seq_len(npars)]
    } else {
      sprintf("par_%d", seq_len(npars))
    }

    parameters <- tibble::tibble(
      time                = time,
      parameter           = par_names_save,
      prior_mean          = da_diag$par_prior_mean,
      prior_sd            = da_diag$par_prior_sd,
      posterior_mean      = da_diag$par_post_mean,
      posterior_sd        = da_diag$par_post_sd,
      update_magnitude    = abs(da_diag$par_post_mean - da_diag$par_prior_mean),
      phy_contribution    = if (!is.null(da_diag$par_phy_contribution))
                              da_diag$par_phy_contribution
                            else rep(NA_real_, npars),
      other_contribution  = if (!is.null(da_diag$par_other_contribution))
                              da_diag$par_other_contribution
                            else rep(NA_real_, npars),
      frac_at_lower_bound = if (!is.null(da_diag$par_frac_lower))
                              da_diag$par_frac_lower
                            else rep(NA_real_, npars),
      frac_at_upper_bound = if (!is.null(da_diag$par_frac_upper))
                              da_diag$par_frac_upper
                            else rep(NA_real_, npars)
    )
  }

  # ── 5. Ranks (one row per active observation, for rank histograms) ─────────
  ranks <- tibble::tibble(
    time     = time,
    variable = if (!is.null(da_diag$obs_variable)) da_diag$obs_variable
               else paste0("obs_", seq_len(n_obs)),
    depth    = if (!is.null(da_diag$obs_depth)) da_diag$obs_depth
               else rep(NA_real_, n_obs),
    obs_rank = da_diag$obs_rank
  )

  # ── 6. Cross-state covariance with PHY_TCHLA observations ────────────────────
  cross_state <- NULL
  if (!is.null(da_diag$cross_state) &&
      length(da_diag$cross_state$state_names) > 0L) {
    cs <- da_diag$cross_state
    cross_state <- tibble::tibble(
      time            = time,
      variable        = cs$state_names,
      cov_with_phy_hx = cs$cov_with_phy,
      mean_k          = cs$mean_k,
      mean_update     = cs$mean_update,
      sign_consistent = cs$sign_ok
    )
  }

  # ── 7. Cross-depth update structure per PHY state ─────────────────────────────
  cross_depth <- NULL
  if (!is.null(da_diag$cross_depth) &&
      length(da_diag$cross_depth$phy_state) > 0L) {
    cd <- da_diag$cross_depth
    cross_depth <- tibble::tibble(
      time                = time,
      phy_state           = cd$phy_state,
      obs_depth_idx       = cd$obs_idx,
      state_depth_idx     = cd$state_idx,
      obs_depth           = cd$obs_depth,
      state_depth         = modeled_depths[cd$state_idx],
      update_contribution = cd$contribution
    )
  }

  list(
    filter_health   = filter_health,
    innovations     = innovations,
    ensemble_spread = ensemble_spread,
    parameters      = parameters,
    ranks           = ranks,
    cross_state     = cross_state,
    cross_depth     = cross_depth
  )
}
