run_enkf <- function(
  zt, psi_t,
  ndepths_modeled,
  z_index,
  x_matrix,
  pars_corr,
  inflat_pars,
  par_fit_method,
  npars,
  inflation_factor,
  localization_distance,
  modeled_depths,
  lake_depth,
  nstates,
  obs_depth,
  model_internal_depths,
  glm_native_x){

  #Extract the data uncertainty for the data
  #types present during the time-step

  d_mat <- t(mvtnorm::rmvnorm(n = nmembers, mean = zt, sigma=as.matrix(psi_t)))

  #Set any negative observations of water quality variables to zero
  d_mat[which(z_index > ndepths_modeled & d_mat < 0.0)] <- 0.0

  #Ensemble mean
  ens_mean <- apply(x_matrix, 1, mean)

  if(npars > 0){
    par_mean <- apply(pars_corr, 1, mean)
    if(par_fit_method == "inflate"){
      for(m in 1:nmembers){
        pars_corr[, m] <- pars_config$inflat_pars * (pars_corr[, m] - par_mean) + par_mean
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
      p_t <- FLAREr:::localization(mat = p_t,
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
  update <- update[1:(ndepths_modeled*nstates), ]
  update <- aperm(array(c(update), dim = c(ndepths_modeled, nstates, nmembers)), perm = c(2,1,3))

  if(!is.null(obs_depth)){
    if(!is.na(obs_depth$obs[i])){
      lake_depth[i, ] <- rnorm(nmembers, obs_depth$obs[i], sd = obs_depth$depth_sd)
      for(m in 1:nmembers){
        depth_index <- which(model_internal_depths[i, , m] > lake_depth[i, m])
        model_internal_depths[i,depth_index , m] <- NA
      }
    }
  }

  for(s in 1:nstates){
    for(m in 1:nmembers){
      depth_index <- which(config$model_settings$modeled_depths <= lake_depth[i, m])
      x[i, s, depth_index, m ] <- update[s,depth_index , m]

      #Map updates to GLM native depths
      native_depth_index <- which(!is.na(model_internal_depths[i, , m]))
      glm_native_x[i,s,native_depth_index,m] <- approx(config$model_settings$modeled_depths[depth_index],
                                                       x[i, s, depth_index, m ], model_internal_depths[i,native_depth_index , m],
                                                       rule = 2)$y
    }
  }

  if(npars > 0){
    if(par_fit_method != "perturb_init"){
      pars[i, , ] <- pars_corr + k_t_pars %*% (d_mat - h %*% x_matrix)
    }else{
      pars[i, , ]  <- pars[i-1, , ]
    }
  }

  return(pars = pars, glm_native_x = glm_native_x, x = x)

}
