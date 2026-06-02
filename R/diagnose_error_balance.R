#' @title Diagnose balance between model process error and observation error
#' @details For each observation in obs_config, propagates model process noise
#'   through the linear observation operator H to compute the equivalent model
#'   error standard deviation in observation space: sqrt(H*Q*Ht), where Q is
#'   the diagonal covariance built from model_sd values and H is the row of
#'   the observation operator for that observation. This is then compared to
#'   the observation error (obs_sd) and their variance ratio is reported.
#'
#'   A ratio near 1 means the filter will weight model and observations roughly
#'   equally.  A ratio >> 1 means model error dominates and the filter will
#'   lean heavily on observations; ratio << 1 means observations will have
#'   little influence.
#'
#'   For composite observations (e.g. PHY_TCHLA = sum of phyto groups times
#'   their chlorophyll-to-biomass ratios), each contributing state is listed
#'   with its mapping coefficient in parentheses.
#'
#'   Observations with no state mapping (non-vertical or diagnostic variables
#'   such as depth and secchi) are listed as N/A.
#'
#' @param states_config data frame returned by generate_states_to_obs_mapping;
#'   must already contain the `states_to_obs` and `states_to_obs_mapping` list
#'   columns
#' @param obs_config data frame of observation configuration
#' @param model_sd numeric matrix of model error SDs, dimensions
#'   `[n_states x n_depths]` as returned by initiate_model_error
#' @return invisibly, a data frame with one row per observation containing
#'   obs_name, assimilate, obs_sd, model_sd_obs_space, var_ratio, and
#'   contributing_states
#' @keywords internal

diagnose_error_balance <- function(states_config, obs_config, model_sd) {

  # Normalise optional assimilate column; absent or NA defaults to 1.
  if (is.null(obs_config$assimilate)) obs_config$assimilate <- 1L
  obs_config$assimilate[is.na(obs_config$assimilate)] <- 1L

  n_obs    <- nrow(obs_config)
  n_states <- nrow(states_config)

  obs_name_out   <- character(n_obs)
  obs_sd_out     <- numeric(n_obs)
  model_sd_out   <- numeric(n_obs)
  ratio_out      <- rep(NA_real_, n_obs)
  states_out     <- character(n_obs)
  assim_out      <- integer(n_obs)

  for (k in seq_len(n_obs)) {
    model_var <- 0
    contrib   <- character(0)

    for (s in seq_len(n_states)) {
      s_to_obs <- states_config$states_to_obs[[s]]
      if (length(s_to_obs) == 0 || all(is.na(s_to_obs))) next
      if (!(k %in% s_to_obs)) next

      pos      <- which(s_to_obs == k)
      coef     <- states_config$states_to_obs_mapping[[s]][pos]
      mean_msd <- mean(model_sd[s, ], na.rm = TRUE)
      model_var <- model_var + coef^2 * mean_msd^2

      label    <- if (abs(coef - 1) < 1e-9) {
        states_config$state_names[s]
      } else {
        paste0(states_config$state_names[s], "(x", round(coef, 4), ")")
      }
      contrib <- c(contrib, label)
    }

    obs_name_out[k]  <- obs_config$state_names_obs[k]
    obs_sd_out[k]    <- obs_config$obs_sd[k]
    model_sd_out[k]  <- sqrt(model_var)
    obs_var          <- obs_config$obs_sd[k]^2
    ratio_out[k]     <- if (obs_var > 0 && model_var > 0) model_var / obs_var else NA_real_
    states_out[k]    <- if (length(contrib) > 0) paste(contrib, collapse = " + ") else "(none)"
    assim_out[k]     <- obs_config$assimilate[k]
  }

  # --- format and emit ---
  sep <- strrep("-", 103)

  header <- sprintf(
    "  %-20s  %6s  %8s  %12s  %7s  %s",
    "Observation", "DA", "obs_sd", "model_sd(Hx)", "ratio",
    "Contributing states (mapping coef)"
  )

  lines <- vapply(seq_len(n_obs), function(k) {
    da_flag <- if (assim_out[k] == 1L) "yes" else "no"
    if (states_out[k] == "(none)") {
      sprintf("  %-20s  %6s  %8.3f  %12s  %7s  %s",
              obs_name_out[k], da_flag, obs_sd_out[k], "N/A", "N/A",
              "(no state mapping -- non-vertical or diagnostic)")
    } else {
      sprintf("  %-20s  %6s  %8.3f  %12.3f  %7.2f  %s",
              obs_name_out[k], da_flag, obs_sd_out[k], model_sd_out[k],
              ratio_out[k], states_out[k])
    }
  }, character(1))

  message(paste0(
    "\n", sep, "\n",
    "  Obs vs. Model Uncertainty Balance Diagnostic\n",
    "  DA: whether the observation is used in the data assimilation update\n",
    "  model_sd(Hx) = sqrt(H*Q*Ht): model process noise projected into observation space\n",
    "  ratio = Var(model in obs space) / Var(obs):  ~1 balanced  |  >>1 model dominates  |  <<1 obs dominates\n",
    sep, "\n",
    header, "\n",
    sep, "\n",
    paste(lines, collapse = "\n"), "\n",
    sep
  ))

  invisible(data.frame(
    obs_name            = obs_name_out,
    assimilate          = assim_out,
    obs_sd              = obs_sd_out,
    model_sd_obs_space  = model_sd_out,
    var_ratio           = ratio_out,
    contributing_states = states_out,
    stringsAsFactors    = FALSE
  ))
}
