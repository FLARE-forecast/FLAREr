#' @title Generate initial conditions for FLARE
#' @details Function to generate initial conditions from either default values in the states_config, observations (if available), or a previous run using the output as a restart file.
#' @param states_config list; list of state configurations
#' @param obs_config list; list of observation configurations
#' @param pars_config list; list of parameter configurations  (Default = NULL)
#' @param obs array; array of the observations. Required dimensions are `[nobs, time, depth]`
#' @param config list; list of configurations
#' @param restart_file string; netcdf file with full path from FLARE output that is used as initial conditions
#' @param historical_met_error boolean; producted by generate_glm_met_files()
#' @import ncdf4
#' @return list; list contains the initial conditions objects required by run_da_forecast()
#' @export
#' @author Quinn Thomas
#' @examples
#' \dontrun{
#'   init <- generate_initial_conditions(states_config, obs_config, pars_config, obs, config, restart_file = config$run_config$restart_file, historical_met_error = met_out$historical_met_error)
#'   }
generate_initial_conditions <- function(states_config,
                                        obs_config,
                                        pars_config = NULL,
                                        obs,
                                        config,
                                        restart_file = NA,
                                        historical_met_error = FALSE){

  if(is.na(restart_file)){

    init <- list()
    if(!is.null(pars_config)){
      npars <- nrow(pars_config)
    }else{
      npars <- 0
    }

    ndepths_modeled <- length(config$model_settings$modeled_depths)
    nmembers <- config$da_setup$ensemble_size
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
            init_depth[i, ] <- approx(x = config$default_init$temp_depths, y = config$default_init$temp, xout = config$model_settings$modeled_depths, rule=2)$y
          }
        }else if(length(which(!is.na(init_obs))) == 1){
          init_depth[i, ]  <- rep(init_obs[!is.na(init_obs)], ndepths_modeled)
        }else{
          init_depth[i, ]  <- approx(x = config$model_settings$modeled_depths[!is.na(init_obs)], y = init_obs[!is.na(init_obs)], xout = config$model_settings$modeled_depths, rule=2)$y
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

        if(config$uncertainty$initial_condition == FALSE){
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

    init$lake_depth[] <- round(config$default_init$lake_depth, 3)
    #Matrix to store snow and ice heights
    init$snow_ice_thickness[1, ] <- config$default_init$snow_thickness
    init$snow_ice_thickness[2, ] <- config$default_init$white_ice_thickness
    init$snow_ice_thickness[3, ] <- config$default_init$blue_ice_thickness
    init$avg_surf_temp[] <- init$states[1 , 1, ]
    init$mixing_vars[, ] <- 0.0
    init$salt[, ] <- config$default_init$salinity

    for(m in 1:nmembers){
      init$model_internal_depths[1:ndepths_modeled, m] <- config$model_settings$modeled_depths
    }

    aux_states_init <- list()
    aux_states_init$snow_ice_thickness <- init$snow_ice_thickness
    aux_states_init$avg_surf_temp <- init$avg_surf_temp
    aux_states_init$the_sals_init <- config$the_sals_init
    aux_states_init$mixing_vars <- init$mixing_vars
    aux_states_init$model_internal_depths <- init$model_internal_depths
    aux_states_init$lake_depth <- init$lake_depth
    aux_states_init$salt <- init$salt

    init <- list(states = init$states,
                 pars = init$pars,
                 aux_states_init = aux_states_init)

  }else{
    nc <- ncdf4::nc_open(config$run_config$restart_file)
    forecast <- ncdf4::ncvar_get(nc, "forecast")
    ncdf4::nc_close(nc)
    if(historical_met_error){
      restart_index <- max(which(forecast == 0)) + 1
    }else{
      restart_index <- max(which(forecast == 0))
    }
    if(max(which(forecast == 0)) == length(forecast)){
      restart_index <- max(which(forecast == 0))
    }

    out <- FLAREr:::generate_restart_initial_conditions(
      restart_file = config$run_config$restart_file,
      state_names = states_config$state_names,
      par_names = pars_config$par_names_save,
      restart_index = restart_index)

    aux_states_init <- list()
    aux_states_init$snow_ice_thickness <- out$snow_ice_thickness
    aux_states_init$avg_surf_temp <- out$avg_surf_temp
    aux_states_init$the_sals_init <- config$the_sals_init
    aux_states_init$mixing_vars <- out$mixing_vars
    aux_states_init$model_internal_depths <- out$model_internal_depths
    aux_states_init$lake_depth <- out$lake_depth
    aux_states_init$salt <- out$salt

    init <- list(states = out$states,
                 pars = out$pars,
                 aux_states_init = aux_states_init)

  }
  return(init)
}
