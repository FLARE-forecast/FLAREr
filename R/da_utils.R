#' @title Build observation error covariance matrix
#' @param psi vector of observation standard deviations (all types)
#' @param z_index integer indices of active observations this time-step
#' @noRd
#' @return diagonal [nobs, nobs] matrix with squared SDs on the diagonal
build_R_matrix <- function(psi, z_index) {
  diag(psi[z_index]^2, nrow = length(z_index))
}

#' @title Apply DA posterior updates shared across all linear DA methods
#'
#' @param update [nstates*ndepths + depth_bool + secchi_bool + npars, nmembers] updated state matrix
#' @param states_depth_start states orientated by depth
#' @param states_height_start states orientated by height
#' @param model_internal_heights_start heights predicted by GLM model
#' @param lake_depth_start lake depth
#' @param snow_ice_thickness_start vector of snow and ice thickness
#' @param avg_surf_temp_start average surface temperature
#' @param mixer_count_start mix count
#' @param mixing_vars_start mixing variables
#' @param diagnostics_start diagnostics
#' @param diagnostics_daily_start daily diagnostics
#' @param pars_corr matrix of parameters
#' @param pars_config parameter configuration list
#' @param config FLARE configuration list
#' @param depth_index index in x matrix with depth values
#' @param par_fit_method method for fixing parameters
#' @param inflation_start inflation array from prior
#' @param lake_max_depth maximum lake depth
#' @param nmembers number of ensemble members
#' @param nstates number of states
#' @param ndepths_modeled number of modeled depths
#' @param npars number of parameters
#' @noRd
#'
#' @return 13-element named list of updated model states, diagnostics, and parameters
apply_da_updates <- function(update,
                             states_depth_start,
                             states_height_start,
                             model_internal_heights_start,
                             lake_depth_start,
                             snow_ice_thickness_start,
                             avg_surf_temp_start,
                             mixer_count_start,
                             mixing_vars_start,
                             diagnostics_start,
                             diagnostics_daily_start,
                             pars_corr,
                             pars_config,
                             config,
                             depth_index,
                             par_fit_method,
                             inflation_start,
                             lake_max_depth,
                             nmembers,
                             nstates,
                             ndepths_modeled,
                             npars) {

  inflation_update <- inflation_start

  states_depth_updated <- update[1:(ndepths_modeled*nstates), ]
  states_depth_updated <- aperm(array(c(states_depth_updated), dim = c(ndepths_modeled, nstates, nmembers)), perm = c(2,1,3))

  model_internal_heights_updated <- model_internal_heights_start

  if(depth_index > 0){
    lake_depth_updated <- update[(ndepths_modeled*nstates + depth_index), ]
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

  list(pars_updated = pars_updated,
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
       inflation_update = inflation_update)
}
