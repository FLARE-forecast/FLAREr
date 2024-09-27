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
                     par_fit_method){

  #Extract the data uncertainty for the data
  #types present during the time-step

  npars <- dim(pars_corr)[1]
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

  #Set any negative observations of water quality variables to zero
  d_mat[which(z_index > ndepths_modeled & d_mat < 0.0)] <- 0.0

  #Ensemble mean
  ens_mean <- apply(x_matrix, 1, mean)

  if(npars > 0){
    par_mean <- apply(pars_corr, 1, mean)
    if(par_fit_method == "inflate"){
      for(m in 1:nmembers){
        pars_corr[, m] <- pars_config$perturb_par * (pars_corr[, m] - par_mean) + par_mean
      }
      par_mean <- apply(pars_corr, 1, mean)
    }
  }

  dit <- matrix(NA, nrow = nmembers, ncol = dim(x_matrix)[1])

  if(npars > 0) dit_pars<- array(NA, dim = c(nmembers, npars))

  #Loop through ensemble members
  for(m in 1:nmembers){
    #  #Ensemble specific deviation
    dit[m, ] <- x_matrix[, m] - ens_mean
    if(npars > 0){
      dit_pars[m, ] <- pars_corr[, m] - par_mean
    }
    if(m == 1){
      p_it <- dit[m, ] %*% t(dit[m, ])
      if(npars > 0){
        p_it_pars <- dit_pars[m, ] %*% t(dit[m, ])
      }
    }else{
      p_it <- dit[m, ] %*% t(dit[m, ]) +  p_it
      if(npars > 0){
        p_it_pars <- dit_pars[m, ] %*% t(dit[m, ]) + p_it_pars
      }
    }
  }

  if(is.null(config$da_setup$inflation_factor)){
    config$da_setup$inflation_factor <- 1.0
  }

  #estimate covariance
  p_t <- config$da_setup$inflation_factor * (p_it / (nmembers - 1))
  if(npars > 0){
    p_t_pars <- config$da_setup$inflation_factor * (p_it_pars / (nmembers - 1))
  }

  if(!is.null(config$da_setup$localization_distance)){
    if(!is.na(config$da_setup$localization_distance)){
      p_t <- localization(mat = p_t,
                          nstates = nstates,
                          modeled_depths = config$model_settings$modeled_depths,
                          localization_distance = config$da_setup$localization_distance,
                          num_single_states = dim(p_t)[1] - nstates * length(config$model_settings$modeled_depths))
    }
  }
  #Kalman gain
  k_t <- p_t %*% t(h) %*% solve(h %*% p_t %*% t(h) + psi_t, tol = 1e-17)
  if(npars > 0){
    k_t_pars <- p_t_pars %*% t(h) %*% solve(h %*% p_t %*% t(h) + psi_t, tol = 1e-17)
  }

  #Update states array (transposes are necessary to convert
  #between the dims here and the dims in the EnKF formulations)
  update <-  x_matrix + k_t %*% (d_mat - h %*% x_matrix)
  states_depth_updated<- update[1:(ndepths_modeled*nstates), ]
  states_depth_updated<- aperm(array(c(states_depth_updated), dim = c(ndepths_modeled, nstates, nmembers)), perm = c(2,1,3))

  model_internal_heights_updated <- model_internal_heights_start

  if(depth_index > 0){
    lake_depth_updated<- update[(ndepths_modeled*nstates + depth_index), ]
    nml <- read_nml(file.path(config$file_path$configuration_directory, config$model_settings$base_GLM_nml))
    max_depth <- nml$morphometry$H[length(nml$morphometry$H)] - nml$morphometry$H[1]
    index <- which(lake_depth_updated > max_depth)
    lake_depth_updated[index] <- max_depth
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
      depth_index <- which(config$model_settings$modeled_depths <= lake_depth_updated[m])
      #Map updates to GLM native depths
      non_na_heights <- which(!is.na(model_internal_heights_start[ , m]))

      if(s > 1){
        index <- which(states_depth_updated[s, , m] < 0.0 & !is.na(states_depth_updated[s, , m]))
        states_depth_updated[s, index, m] <- 0.0
      }

      states_height_updated[s,non_na_heights,m] <- approx(lake_depth_updated[m] - config$model_settings$modeled_depths[depth_index],
                                                          states_depth_updated[s, depth_index, m ],
                                                          model_internal_heights_start[non_na_heights , m],
                                                          rule = 2)$y
    }
  }

  if(npars > 0){
    if(par_fit_method != "perturb_init"){
      pars_updated <- pars_corr + k_t_pars %*% (d_mat - h %*% x_matrix)
    }else{
      pars_updated  <- pars_corr
    }
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
        depth_index <- which(config$model_settings$modeled_depths > lake_depth_updated[m])
        diagnostics_updated[d,depth_index, m] <- NA
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

  #Correct any parameter values outside bounds
  if(npars > 0){
    for(par in 1:npars){
      low_index <- which(pars_updated[par ,] < pars_config$par_lowerbound[par])
      high_index <- which(pars_updated[par ,] > pars_config$par_upperbound[par])
      pars_updated[par, low_index] <- pars_config$par_lowerbound[par]
      pars_updated[par, high_index]  <- pars_config$par_upperbound[par]
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
              mixing_vars_updated = mixing_vars_updated))
}
