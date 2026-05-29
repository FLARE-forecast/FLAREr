#' @title Build observation error covariance matrix
#' @param psi vector of observation standard deviations (all types)
#' @param z_index integer indices of active observations this time-step
#' @noRd
#' @return diagonal `[nobs, nobs]` matrix with squared SDs on the diagonal
build_R_matrix <- function(psi, z_index) {
  diag(psi[z_index]^2, nrow = length(z_index))
}

#' @title Rigidly shift GLM internal heights by a lake-depth change
#'
#' Adds `diff_height` to every non-NA layer height and prunes (sets NA) any layer
#' pushed below the basin bottom. Mirrors the depth-update shift in
#' `apply_da_updates()`. A downward shift is clamped so at least `min_layers`
#' layers survive, since the `approx()` interpolations that consume the heights
#' (in `add_process_noise()` and the DA reconstruction) need at least two points.
#' The (possibly clamped) shift is returned so the caller can keep `lake_depth`
#' consistent with the new top height.
#'
#' @param heights numeric vector of model internal heights (may contain trailing NA)
#' @param diff_height scalar lake-depth change to apply
#' @param min_layers minimum number of layers to keep after a downward shift
#' @noRd
#' @return list with `heights` (shifted/pruned) and `diff_height` (after clamping)
shift_heights_for_depth_change <- function(heights, diff_height, min_layers = 2L) {
  if (is.na(diff_height) || diff_height == 0) {
    return(list(heights = heights, diff_height = diff_height))
  }
  non_na <- which(!is.na(heights))
  if (diff_height < 0 && length(non_na) >= min_layers) {
    # min_layers-th highest height; clamp so it (and everything above) stays >= 0
    kth_highest <- sort(heights[non_na], decreasing = TRUE)[min_layers]
    diff_height <- max(diff_height, -kth_highest)
  }
  heights[non_na] <- heights[non_na] + diff_height
  heights[which(heights < 0)] <- NA          # prune layers with negative height
  list(heights = heights, diff_height = diff_height)
}

#' @title Apply DA posterior updates shared across all linear DA methods
#'
#' @param update `[nstates*ndepths + n_non_vertical + npars, nmembers]` updated state matrix
#' @param states_depth_start states orientated by depth
#' @param states_height_start states orientated by height
#' @param model_internal_heights_start heights predicted by GLM model
#' @param lake_depth_start lake depth
#' @param snow_ice_thickness_start vector of snow and ice thickness
#' @param diagnostics_start diagnostics
#' @param diagnostics_daily_start daily diagnostics
#' @param pars_corr matrix of parameters
#' @param pars_config parameter configuration data frame; may include an optional
#'   `par_min_sd` column — if present and non-NA for a parameter, the posterior
#'   ensemble SD is floored to that value after inflation (multiplicative rescaling
#'   when SD > 0; full redraw from N(mean, par_min_sd) when SD == 0)
#' @param config FLARE configuration list
#' @param obs_non_vertical named list of non-vertical observation metadata (from create_obs_non_vertical)
#' @param active_in_xmatrix character vector of variable names in the same order they were appended to x_matrix
#' @param n_non_vertical integer number of non-vertical variables in the augmented state vector
#' @param par_fit_method method for fixing parameters
#' @param inflation_start inflation array from prior
#' @param lake_max_depth maximum lake depth
#' @param nmembers number of ensemble members
#' @param nstates number of states (full state array, all modeled variables)
#' @param ndepths_modeled number of modeled depths
#' @param npars number of parameters
#' @param n_da_states number of assimilated states in the EnKF state block
#'   (`length(da_idx)`); defaults to `nstates` when `da_idx` is NULL
#' @param da_idx integer indices (into the full `nstates` arrays) of the
#'   assimilated states; rows not in `da_idx` retain their forecast value (the
#'   GLM-propagated state) and are excluded from covariance inflation. NULL
#'   means every state is assimilated (legacy behavior).
#' @noRd
#'
#' @return 13-element named list of updated model states, diagnostics, and parameters
apply_da_updates <- function(update,
                             states_depth_start,
                             states_height_start,
                             model_internal_heights_start,
                             lake_depth_start,
                             snow_ice_thickness_start,
                             diagnostics_start,
                             diagnostics_daily_start,
                             pars_corr,
                             pars_config,
                             config,
                             obs_non_vertical,
                             active_in_xmatrix,
                             n_non_vertical = 0L,
                             par_fit_method,
                             inflation_start,
                             lake_max_depth,
                             nmembers,
                             nstates,
                             ndepths_modeled,
                             npars,
                             n_da_states = NULL,
                             da_idx = NULL) {

  if (is.null(da_idx)) da_idx <- seq_len(nstates)
  if (is.null(n_da_states)) n_da_states <- length(da_idx)

  inflation_update <- inflation_start

  # The EnKF update block spans only the assimilated states (n_da_states). Start
  # every state from its forecast, then overwrite the assimilated rows, so
  # da_updated == 0 states keep their GLM-propagated value (zero DA delta).
  states_depth_updated <- states_depth_start
  da_block <- update[1:(ndepths_modeled * n_da_states), , drop = FALSE]
  da_block <- aperm(array(c(da_block), dim = c(ndepths_modeled, n_da_states, nmembers)), perm = c(2,1,3))
  states_depth_updated[da_idx, , ] <- da_block

  model_internal_heights_updated <- model_internal_heights_start
  lake_depth_updated <- lake_depth_start

  # Write-back for each non-vertical variable in the order they appear in the
  # augmented state vector.  State variables (model_source == "state") update
  # model internals directly; diagnostic variables with a non-NULL inverse_fn
  # write back through their physics formula; pure diagnostics (NULL inverse_fn)
  # propagate only through ensemble covariance and need no write-back.
  for (k in seq_len(n_non_vertical)) {
    nv_var      <- active_in_xmatrix[k]
    meta        <- obs_non_vertical[[nv_var]]
    ops         <- get_non_vertical_operator(nv_var)
    row_idx     <- ndepths_modeled * n_da_states + k
    updated_val <- update[row_idx, ]

    if (nv_var == "depth") {
      lake_depth_updated <- updated_val
      index <- which(lake_depth_updated > lake_max_depth)
      lake_depth_updated[index] <- lake_max_depth
      for (m in 1:nmembers) {
        non_na_heights <- which(!is.na(model_internal_heights_start[ , m]))
        diff_height    <- lake_depth_updated[m] - lake_depth_start[m]
        model_internal_heights_updated[non_na_heights, m] <-
          model_internal_heights_start[non_na_heights, m] + diff_height
        neg_idx <- which(model_internal_heights_updated[, m] < 0)
        model_internal_heights_updated[neg_idx, m] <- NA
      }

    } else if (meta$model_source == "diagnostic" && !is.null(ops$inverse_fn)) {
      diag_names <- config$output_settings$diagnostics_names
      diag_idx   <- which(diag_names == meta$model_variable)
      if (length(diag_idx) > 0) {
        depth_idx <- resolve_depth_index(meta$model_depth_m, config)
        if (length(dim(diagnostics_start)) == 3) {
          diagnostics_start[diag_idx, depth_idx, ] <- ops$inverse_fn(updated_val)
        }
      }
    }
    # NULL inverse_fn: no write-back needed
  }

  # Start from the model's native height representation and apply only the DA
  # correction as a delta, so the model's exact layer grid is preserved and
  # interpolation error is confined to the small correction term.
  states_height_updated <- states_height_start

  for(s in 1:nstates){
    for(m in 1:nmembers){
      valid_depth_idx <- which(config$model_settings$modeled_depths <= lake_depth_updated[m])
      non_na_heights <- which(!is.na(model_internal_heights_start[ , m]))

      if(s > 1){
        index <- which(states_depth_updated[s, , m] < 0.0 & !is.na(states_depth_updated[s, , m]))
        states_depth_updated[s, index, m] <- -states_depth_updated[s, index, m]
        still_neg <- which(states_depth_updated[s, , m] < 0.0 & !is.na(states_depth_updated[s, , m]))
        states_depth_updated[s, still_neg, m] <- 0.0
      }

      delta_depth <- states_depth_updated[s, valid_depth_idx, m] - states_depth_start[s, valid_depth_idx, m]
      delta_height <- approx(
        lake_depth_updated[m] - config$model_settings$modeled_depths[valid_depth_idx],
        delta_depth,
        model_internal_heights_updated[non_na_heights, m],
        rule = 2)$y
      states_height_updated[s, non_na_heights, m] <- states_height_start[s, non_na_heights, m] + delta_height
      if(s > 1){
        neg_idx <- which(states_height_updated[s, non_na_heights, m] < 0.0)
        states_height_updated[s, non_na_heights[neg_idx], m] <- 0.0
      }
    }
  }

  if(isTRUE(config$da_setup$use_inflation_factor) && !is.null(config$da_setup$inflation_factor)){
    inf <- config$da_setup$inflation_factor
    nlayers <- dim(states_height_updated)[2]
    for(s in da_idx){
      for(h_idx in 1:nlayers){
        valid_m <- which(!is.na(states_height_updated[s, h_idx, ]))
        if(length(valid_m) > 1){
          state_mean <- mean(states_height_updated[s, h_idx, valid_m])
          states_height_updated[s, h_idx, valid_m] <- state_mean + inf * (states_height_updated[s, h_idx, valid_m] - state_mean)
        }
      }
    }
  }

  if(isTRUE(npars > 0)){
    if(par_fit_method != "perturb_init"){
      pars_updated <- update[(dim(update)[1]-npars+1):dim(update)[1], ]
    }else{
      pars_updated <- pars_corr
    }
  }else{
    pars_updated <- NULL
  }

  if(length(config$output_settings$diagnostics_names) > 0){
    if(length(config$output_settings$diagnostics_names) > 1){
      diagnostics_updated <- diagnostics_start[ , ,]
    }else if(length(config$output_settings$diagnostics_names) == 1){
      diagnostics_updated <- array(NA, dim = c(1, dim(diagnostics_start)))
      diagnostics_updated[1, , ] <- diagnostics_start[ , ]
    }else{
      diagnostics_updated <- diagnostics_start
    }
    for(d in 1:dim(diagnostics_updated)[1]){
      for(m in 1:nmembers){
        above_lake_idx <- which(config$model_settings$modeled_depths > lake_depth_updated[m])
        diagnostics_updated[d, above_lake_idx, m] <- NA
      }
    }
  }else{
    diagnostics_updated <- diagnostics_start
  }

  if(length(config$output_settings$diagnostics_daily$csv_names) > 0){
    if(length(config$output_settings$diagnostics_daily$csv_names) > 1){
      diagnostics_daily_updated <- diagnostics_daily_start[ , ]
    }else if(length(config$output_settings$diagnostics_daily$csv_names) == 1){
      diagnostics_daily_updated <- array(NA, dim = c(1, dim(diagnostics_daily_start)))
      diagnostics_daily_updated[1, , ] <- diagnostics_daily_start[ , ]
    }else{
      diagnostics_daily_updated <- diagnostics_daily_start
    }
  }else{
    diagnostics_daily_updated <- diagnostics_daily_start
  }

  log_particle_weights_updated <- rep(log(1.0), nmembers)

  num_out_depths <- length(which(!is.na(states_height_start[1, ,1])))


  #Correct any parameter values outside bounds using reflective bounds to preserve ensemble spread
  if(isTRUE(npars > 0)){

    for(par in 1:npars){
      if(par_fit_method == "inflate" && pars_config$fix_par[par] == 0){
        par_mean <- mean(pars_updated[par, ])
        pars_updated[par, ] <- par_mean + pars_config$perturb_par[par] * (pars_updated[par, ] - par_mean)
      }

      if("par_min_sd" %in% names(pars_config) &&
         !is.na(pars_config$par_min_sd[par]) &&
         pars_config$fix_par[par] == 0){
        par_min_sd <- pars_config$par_min_sd[par]
        par_mean   <- mean(pars_updated[par, ])
        par_sd     <- sd(pars_updated[par, ])
        if(par_sd == 0){
          pars_updated[par, ] <- rnorm(nmembers, mean = par_mean, sd = par_min_sd)
        } else if(par_sd < par_min_sd){
          pars_updated[par, ] <- par_mean + (par_min_sd / par_sd) * (pars_updated[par, ] - par_mean)
        }
      }
      lb <- pars_config$par_lowerbound[par]
      ub <- pars_config$par_upperbound[par]
      low_index  <- which(pars_updated[par, ] < lb)
      high_index <- which(pars_updated[par, ] > ub)
      pars_updated[par, low_index]  <- 2 * lb - pars_updated[par, low_index]
      pars_updated[par, high_index] <- 2 * ub - pars_updated[par, high_index]
      # Safety clamp for members that overshoot by more than the interval width
      pars_updated[par, ] <- pmax(lb, pmin(ub, pars_updated[par, ]))
    }
  }

  snow_ice_thickness_updated <- snow_ice_thickness_start

  list(pars_updated = pars_updated,
       states_depth_updated = states_depth_updated,
       states_height_updated = states_height_updated,
       lake_depth_updated = lake_depth_updated,
       model_internal_heights_updated = model_internal_heights_updated,
       log_particle_weights_updated = log_particle_weights_updated,
       diagnostics_updated = diagnostics_updated,
       diagnostics_daily_updated = diagnostics_daily_updated,
       snow_ice_thickness_updated = snow_ice_thickness_updated,
       inflation_update = inflation_update)
}

#' @title Parameter-only EnKF update for one-step lag dual EnKF
#'
#' @param pars `[npars, nmembers]` prior parameter ensemble (after inflation/perturbation)
#' @param predicted_obs `[nobs_active, nmembers]` H*x_forecast from the state filter
#' @param zt vector of active observations (length nobs_active)
#' @param psi vector of all observation SDs (indexed by z_index)
#' @param z_index integer indices of active observations into psi
#' @param pars_config parameter configuration list
#' @noRd
#' @return `[npars, nmembers]` updated parameter ensemble
update_parameters_enkf <- function(pars, predicted_obs, zt, psi, z_index, pars_config) {
  npars    <- nrow(pars)
  nmembers <- ncol(pars)
  nobs     <- length(z_index)

  R <- build_R_matrix(psi, z_index)

  # Perturbed observations [nobs, nmembers] — stochastic EnKF
  d_mat <- matrix(zt, nrow = nobs, ncol = nmembers) +
    matrix(rnorm(nobs * nmembers), nrow = nobs, ncol = nmembers) * sqrt(diag(R))

  # Parameter and predicted-observation perturbation matrices
  A_pars <- pars - rowMeans(pars)
  Y      <- predicted_obs - rowMeans(predicted_obs)

  # Kalman gain for parameters: K = A Y^T (Y Y^T + (N-1)R)^{-1}
  C_yy    <- Y %*% t(Y) + (nmembers - 1) * R
  K_theta <- (A_pars %*% t(Y)) %*% solve(C_yy)

  pars_updated <- pars + K_theta %*% (d_mat - predicted_obs)

  # Reflective bounds
  for (par in seq_len(npars)) {
    lb <- pars_config$par_lowerbound[par]
    ub <- pars_config$par_upperbound[par]
    lo <- which(pars_updated[par, ] < lb)
    hi <- which(pars_updated[par, ] > ub)
    pars_updated[par, lo] <- 2 * lb - pars_updated[par, lo]
    pars_updated[par, hi] <- 2 * ub - pars_updated[par, hi]
    pars_updated[par, ]   <- pmax(lb, pmin(ub, pars_updated[par, ]))
  }

  pars_updated
}
