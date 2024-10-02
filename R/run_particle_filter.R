#' @title Run particle filter on model predictions
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
#' @param vertical_obs number of vertical observations (i.e. states not associated with a depth)
#' @param working_directory current working directory for simulation
#' @param obs_config list of observation configurations
#'
#' @return list of updated model states, diagnostics, and parameters
#' @noRd
#'
run_particle_filter <- function(x_matrix,
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
                                vertical_obs,
                                working_directory,
                                obs_config){

  npars <- dim(pars_corr)[1]
  nmembers <- dim(states_depth_start)[3]
  nstates <- dim(states_depth_start)[1]
  ndepths_modeled <- length(config$model_settings$modeled_depths)

  obs_states <- t(h %*% x_matrix)

  LL <- rep(NA, length(nmembers))
  for(m in 1:nmembers){
    LL_vector <- dnorm(zt, mean = obs_states[m, ], sd = psi[z_index], log = TRUE)

    if(length(which(is.infinite(LL_vector))) > 0){
      warning("infinite likelihood")
    }

    #This handles the case where an observed depth is missing from an ensemble member
    #due to the ensemble member being shallower than the depth of the observation
    na_index <- which(is.na(obs_states[m, ]))

    if(length(na_index) > 0){
      LL_vector[na_index] <- log(1.0e-20) #assign a very low probability density
    }
    LL[m] <- sum(LL_vector)
  }

  wt_norm <- exp(LL) / sum(exp(LL))

  # Check
  if(length(which(is.nan(wt_norm))) > 0){
    index <- ceiling(z_index[which(z_index <= vertical_obs * ndepths_modeled)] / ndepths_modeled)
    obs_names <- c(obs_config$state_names_obs[index])
    if(depth_index > 0){
      obs_names <- c(obs_names,"depth")
    }
    if(secchi_index > 0){
      obs_names <- c(obs_names,"seechi")
    }
    readr::write_csv(x = tibble(obs_names = obs_names,
                                obs = zt,
                                pred = obs_states[which(is.nan(wt_norm))[1], ],
                                sd = psi[z_index],
                                LL = dnorm(zt, mean = obs_states[1, ], sd = psi[z_index], log = TRUE)),
                     file = paste0(working_directory, "/PF_with_NaN.csv"))
    stop("PF weights too small resulting in division by 0; see PF_with_NaN.csv file in working directory to diagnosis")
  }

  log_particle_weights_updated <- log_particle_weights_start + log(wt_norm)

  Neff <- 1/sum(exp(log_particle_weights_updated)^2)

  if(Neff < nmembers/2 | config$da_setup$pf_always_resample){
    samples <- sample.int(nmembers, replace = TRUE, prob = exp(log_particle_weights_updated))
    log_particle_weights_updated[] <- log(1.0)

    update <- x_matrix[1:(ndepths_modeled*nstates), samples]
    states_depth_updated <- aperm(array(c(update), dim = c(ndepths_modeled, nstates, nmembers)), perm = c(2,1,3))
    states_height_updated <-  states_height_start[ , ,samples]

    if(npars > 0){
      pars_updated <- pars_corr[, samples]
    }

    snow_ice_thickness_updated <- snow_ice_thickness_start[ , samples]
    avg_surf_temp_updated <- avg_surf_temp_start[samples]
    lake_depth_updated <- lake_depth_start[ samples]
    model_internal_heights_updated <- model_internal_heights_start[ , samples]
    mixer_count_updated <- mixer_count_start[samples]
    mixing_vars_updated <- mixing_vars_start[, samples]

    if(length(config$output_settings$diagnostics_names) > 1){
      if(length(config$output_settings$diagnostics_names) > 1){
        diagnostics_updated <- diagnostics_start[ , ,samples]
      }else if(length(config$output_settings$diagnostics_names) == 1){
        diagnostics_updated <- array(NA, dim = c(1, dim(diagnostics_start)))
        diagnostics_updated[1, , ] <- diagnostics_start[ ,samples]
      }
    }else{
      diagnostics_updated <- diagnostics_start

    }

    if(length(config$output_settings$diagnostics_daily$csv_names) > 0){
      if(length(config$output_settings$diagnostics_daily$csv_names) > 1){
        diagnostics_daily_updated <- diagnostics_daily_start[ , samples]
      }else if(length(config$output_settings$diagnostics_daily$csv_names) == 1){
        diagnostics_daily_updated <- array(NA, dim = c(1, dim(diagnostics_daily_start)))
        diagnostics_daily_updated[1, , ] <- diagnostics_daily_start[ , samples]
      }else{
        diagnostics_daily_updated <- diagnostics_daily_start
      }
    }else{
      diagnostics_daily_updated <- diagnostics_daily_start
    }

  }else{

    update <- x_matrix[1:(ndepths_modeled*nstates), ]
    states_depth_updated <- aperm(array(c(update), dim = c(ndepths_modeled, nstates, nmembers)), perm = c(2,1,3))
    states_height_updated <-  states_height_start[ , ,]

    if(npars > 0){
      pars_updated <- pars_corr[, ]
    }

    snow_ice_thickness_updated <- snow_ice_thickness_start[ , ]
    avg_surf_temp_updated <- avg_surf_temp_start[]
    lake_depth_updated <- lake_depth_start[ ]
    model_internal_heights_updated <- model_internal_heights_start[ , ]
    mixer_count_updated <- mixer_count_start[]
    mixing_vars_updated <- mixing_vars_start[, ]

    if(length(config$output_settings$diagnostics_names) > 0){

      if(length(config$output_settings$diagnostics_names) > 1){
        diagnostics_updated <- diagnostics_start[ , ,]
      }else if(length(config$output_settings$diagnostics_names) == 1){
        diagnostics_updated <- array(NA, dim = c(1, dim(diagnostics_start)))
        diagnostics_updated[1, , ] <- diagnostics_start[ , ]
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

  }


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
              mixing_vars_updated = mixing_vars_updated))
}

