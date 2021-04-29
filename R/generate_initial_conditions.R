#' Generate initial conditions from observations and default values
#'
#' @param states_config
#' @param obs_config
#' @param pars_config
#' @param obs
#' @param config
#'
#' @return
#' @export
#'
#' @examples
generate_initial_conditions <- function(states_config,
                                        obs_config,
                                        pars_config = NULL,
                                        obs,
                                        config){

  init <- list()

  nmembers <- config$ensemble_size

  if(!is.null(pars_config)){
    npars <- nrow(pars_config)
  }else{
    npars <- 0
  }

  ndepths_modeled <- length(config$modeled_depths)

  nstates <- length(states_config$state_names)

  init$states <- array(NA, dim=c(nstates, ndepths_modeled, nmembers))
  init$pars <- array(NA, dim=c(npars, nmembers))
  init$lake_depth <- array(NA, dim=c(nmembers))
  init$snow_ice_thickness <- array(NA, dim=c(3, nmembers))
  init$avg_surf_temp <- array(NA, dim=c(nmembers))
  init$mixing_vars <- array(NA, dim=c(17, nmembers))
  init$model_internal_depths <- array(NA, dim = c(500, nmembers))
  init$salt <- array(NA, dim = c(ndepths_modeled, nmembers))

  alpha_v <- 1 - exp(-states_config$vert_decorr_length)

  q_v <- rep(NA ,ndepths_modeled)
  w <- rep(NA, ndepths_modeled)
  w_new <- rep(NA, ndepths_modeled)

  init_depth <- array(NA, dim = c(nrow(states_config),ndepths_modeled))
  for(i in 1:nrow(states_config)){
    if(!is.na(states_config$init_obs_name[i])){
      obs_index <- which(obs_config$state_names_obs == states_config$init_obs_name[i])
      init_obs <- obs[obs_index, 1, ] * (1/states_config$states_to_obs_mapping_1[i]) * states_config$init_obs_mapping[i]
      if(length(which(!is.na(init_obs))) == 0){
        init_depth[i, ] <- rep(states_config$initial_conditions[i], ndepths_modeled)
        if(states_config$init_obs_name[i] == "temp"){
          init_depth[i, ] <- approx(x = config$default_temp_init_depths, y = config$default_temp_init, xout = config$modeled_depths, rule=2)$y
        }
      }else if(length(which(!is.na(init_obs))) == 1){
        init_depth[i, ]  <- rep(init_obs[!is.na(init_obs)], ndepths_modeled)
      }else{
        init_depth[i, ]  <- approx(x = config$modeled_depths[!is.na(init_obs)], y = init_obs[!is.na(init_obs)], xout = config$modeled_depths, rule=2)$y
      }
    }else{
      init_depth[i, ]  <- rep(states_config$initial_conditions[i], ndepths_modeled)
    }
  }

  for(m in 1:nmembers){
    q_v[] <- NA
    w[] <- NA
    w_new[] <- NA
    for(jj in 1:nstates){
      w[] <- rnorm(ndepths_modeled, 0, 1)
      w_new[1] <- w[1]
      q_v[1] <- states_config$initial_model_sd[jj] * w[1]
      for(kk in 2:ndepths_modeled){
        w_new[kk] <- (alpha_v[jj] * w_new[kk-1] + sqrt(1 - alpha_v[jj]^2) * w[kk])
        q_v[kk] <- w_new[kk] * states_config$initial_model_sd[jj]
        #q_v[kk] <- alpha_v * q_v[kk-1] + sqrt(1 - alpha_v^2) * states_config$initial_model_sd[jj] * w[kk]
      }

      if(config$initial_condition_uncertainty == FALSE){
        init$states[jj, , m] <- init_depth[jj, ]
      }else{
        init$states[jj, , m] <- init_depth[jj, ] + q_v
      }
      if(jj > 1){
        init$states[jj,which(init$states[jj, , m] < 0.0) , m] <- 0.0
      }
    }
  }

  if(npars > 0){
    for(par in 1:npars){
      init$pars[par, ] <- runif(n=nmembers,pars_config$par_init_lowerbound[par], pars_config$par_init_upperbound[par])
    }
  }

  init$lake_depth[] <- round(config$lake_depth_init, 3)
  #Matrix to store snow and ice heights
  init$snow_ice_thickness[1, ] <- config$default_snow_thickness_init
  init$snow_ice_thickness[2, ] <- config$default_white_ice_thickness_init
  init$snow_ice_thickness[3, ] <- config$default_blue_ice_thickness_init
  init$avg_surf_temp[] <- init$states[1 , 1, ]
  init$mixing_vars[, ] <- 0.0
  init$salt[, ] <- config$the_sals_init

  for(m in 1:nmembers){
    init$model_internal_depths[1:ndepths_modeled, m] <- config$modeled_depths
  }

  return(init)
}
