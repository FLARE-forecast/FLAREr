#' @title Run Ensemble Kalman filter on model predictions
#'
#' @param x_matrix matrix of model states (includes secchi and depths)
#' @param h matrix to map x matrix to zt vector
#' @param pars_corr matrix of parameters
#' @param zt vector of observations
#' @param psi_t vector of observation standard deviations
#' @param z_index indexes of observations
#' @param states_depth_start states orientated by depth
#' @param states_height_start states orientated by height
#' @param model_internal_heights_start heights predicted by GLM model
#' @param lake_depth_start lake depth
#' @param log_particle_weights_start log of particle weights
#' @param snow_ice_thickness_start vector of snow and ice thickness
#' @param avg_surf_temp_start average surface temperature (a restart variable)
#' @param mixer_count_start mix count (a restart variable)
#' @param mixing_vars_start mixing variables (a restart variable)
#' @param diagnostics_start diagnostics
#' @param pars_config parameter configuration list
#' @param config FLARE configuration list
#' @param depth_index index in x matrix with depth values
#' @param secchi_index in x matrix with secchi values
#' @param depth_obs observed depth
#' @param depth_sd observed depth standard deviation
#' @param par_fit_method method for fixing parameters
#' @noRd
#'
#' @return list of updated model states, diagnostics, and parameters
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
                     avg_surf_temp_start,
                     mixer_count_start,
                     mixing_vars_start,
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
                     lake_max_depth){

  #Extract the data uncertainty for the data
  #types present during the time-step

  if(!is.null(pars_config)){
    npars <- dim(pars_corr)[1]
  }else{
    npars <- 0
  }
  nmembers <- dim(states_depth_start)[3]
  nstates <- dim(states_depth_start)[1]
  ndepths_modeled <- length(config$model_settings$modeled_depths)

  curr_psi <- psi[z_index] ^ 2

  if(length(z_index) > 1){
    psi_t <- diag(curr_psi)
  }else{
    #Special case where there is only one data
    #type during the time-step
    psi_t <- curr_psi
  }

  d_mat <- t(mvtnorm::rmvnorm(n = nmembers, mean = zt, sigma=as.matrix(psi_t)))

  if(isTRUE(config$da_setup$log_transform_wq_obs)){
    wq_rows <- which(z_index > ndepths_modeled)
    if(length(wq_rows) > 0){
      pos_rows  <- wq_rows[zt[wq_rows] > 0]
      zero_rows <- wq_rows[zt[wq_rows] <= 0]
      for(row_idx in pos_rows){
        yt_i      <- zt[row_idx]
        sig_i     <- sqrt(curr_psi[row_idx])
        sigma_log <- sqrt(log(1 + (sig_i / yt_i)^2))
        mu_log    <- log(yt_i) - sigma_log^2 / 2
        d_mat[row_idx, ] <- exp(stats::rnorm(nmembers, mean = mu_log, sd = sigma_log))
      }
      if(length(zero_rows) > 0){
        if(isTRUE(config$da_setup$log_transform_wq_zero_collapse)){
          d_mat[zero_rows, ] <- 0.0
        } else {
          d_mat[zero_rows, ][d_mat[zero_rows, ] < 0] <- 0.0
        }
      }
    }
  } else {
    d_mat[which(z_index > ndepths_modeled & d_mat < 0.0)] <- 0.0
  }

  #Ensemble mean
  ens_mean <- apply(x_matrix, 1, mean)

  #Ensemble perturbation matrix and sample covariance via BLAS dgemm
  a_mat <- x_matrix - ens_mean
  p_t   <- a_mat %*% t(a_mat) / (nmembers - 1)

  if(!is.null(config$da_setup$localization_distance)){
    if(!is.na(config$da_setup$localization_distance)){
      p_t <- FLAREr:::localization(mat = p_t,
                                   nstates = nstates,
                                   modeled_depths = config$model_settings$modeled_depths,
                                   localization_distance = config$da_setup$localization_distance,
                                   num_single_states = dim(p_t)[1] - nstates * length(config$model_settings$modeled_depths))
    }
  }

  #Kalman gain: solve the linear system directly to avoid forming the explicit
  #inverse; uses standard tolerance so near-singularity is detected rather than
  #silently producing extreme values.
  s_mat <- h %*% p_t %*% t(h) + psi_t
  k_t   <- t(solve(s_mat, h %*% p_t, tol = .Machine$double.eps))

  # Inflation is applied to the prior ensemble in run_da_forecast before this
  inflation_update <- inflation_start

  #Update states array (transposes are necessary to convert
  #between the dims here and the dims in the EnKF formulations)

  update <-  x_matrix + k_t %*% (d_mat - h %*% x_matrix)
  states_depth_updated<- update[1:(ndepths_modeled*nstates), ]
  states_depth_updated<- aperm(array(c(states_depth_updated), dim = c(ndepths_modeled, nstates, nmembers)), perm = c(2,1,3))

  model_internal_heights_updated <- model_internal_heights_start

  if(depth_index > 0){
    lake_depth_updated<- update[(ndepths_modeled*nstates + depth_index), ]
    index <- which(lake_depth_updated > lake_max_depth)
    lake_depth_updated[index] <- lake_max_depth
    for(m in 1:nmembers){
      non_na_heights <- which(!is.na(model_internal_heights_start[ , m]))
      diff_height <- lake_depth_updated[m] - model_internal_heights_start[1, m]
      model_internal_heights_updated[non_na_heights, m] <- model_internal_heights_start[non_na_heights, m ] +  diff_height
      index <- which(model_internal_heights_updated[, m] < 0)
      model_internal_heights_updated[index, m] <- NA
    }
  }else{
    lake_depth_updated <- lake_depth_start
  }


  states_height_updated <- array(NA, dim = c(nstates, dim(model_internal_heights_start)[1], nmembers))

  for(s in 1:nstates){
    for(m in 1:nmembers){
      valid_depth_idx <- which(config$model_settings$modeled_depths <= lake_depth_updated[m])
      #Map updates to GLM native depths
      non_na_heights <- which(!is.na(model_internal_heights_start[ , m]))

      if(s > 1){
        index <- which(states_depth_updated[s, , m] < 0.0 & !is.na(states_depth_updated[s, , m]))
        states_depth_updated[s, index, m] <- -states_depth_updated[s, index, m]
        still_neg <- which(states_depth_updated[s, , m] < 0.0 & !is.na(states_depth_updated[s, , m]))
        states_depth_updated[s, still_neg, m] <- 0.0
      }

      states_height_updated[s, non_na_heights, m] <- approx(
        lake_depth_updated[m] - config$model_settings$modeled_depths[valid_depth_idx],
        states_depth_updated[s, valid_depth_idx, m],
        model_internal_heights_start[non_na_heights, m],
        rule = 2)$y
    }
  }

  if(npars > 0){
    if(par_fit_method != "perturb_init"){
      pars_updated <- update[(dim(update)[1]-npars+1):dim(update)[1], ]
    }else{
      pars_updated  <- pars_corr
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

  log_particle_weights_updated <-rep(log(1.0), nmembers)

  num_out_depths <- length(which(!is.na(states_height_start[1, ,1])))

  #Correct any parameter values outside bounds using reflective bounds to preserve ensemble spread
  if(npars > 0){
    for(par in 1:npars){
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
  avg_surf_temp_updated <- avg_surf_temp_start
  mixer_count_updated <- mixer_count_start
  mixing_vars_updated <- mixing_vars_start

  return(list(pars_updated = pars_updated,
              states_depth_updated = states_depth_updated,
              states_height_updated = states_height_updated,
              lake_depth_updated = lake_depth_updated,
              model_internal_heights_updated = model_internal_heights_updated,
              log_particle_weights_updated = log_particle_weights_updated,
              diagnostics_updated = diagnostics_updated,
              diagnostics_daily_updated = diagnostics_daily_updated,
              snow_ice_thickness_updated = snow_ice_thickness_updated,
              avg_surf_temp_updated = avg_surf_temp_updated,
              mixer_count_updated = mixer_count_updated,
              mixing_vars_updated = mixing_vars_updated,
              inflation_update = inflation_update))
}
