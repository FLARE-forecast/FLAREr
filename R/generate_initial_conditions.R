#' @title Generate initial conditions for FLARE
#' @details Function to generate initial conditions from either default values in the states_config, observations (if available), or a previous run using the output as a restart file.
#' @param states_config list; list of state configurations
#' @param obs_config list; list of observation configurations
#' @param pars_config list; list of parameter configurations  (Default = NULL)
#' @param obs array; array of the observations. Required dimensions are `[nobs, time, depth]`
#' @param config list; list of configurations
#' @param obs_non_vertical list; observations of variables that don't have vertical dimension.
#' @import ncdf4
#' @return list; list contains the initial conditions objects required by run_da_forecast()
#' @author Quinn Thomas
#' @keywords internal
generate_initial_conditions <- function(states_config,
                                        obs_config,
                                        pars_config = NULL,
                                        obs,
                                        config,
                                        obs_non_vertical){
  if(is.na(config$run_config$restart_file)){

    init <- list()
    if(!is.null(pars_config)){
      if("model" %in% names(pars_config)){
        pars_config <- pars_config[pars_config$model == config$model_settings$model, ]
      }
      npars <- nrow(pars_config)
    }else{
      npars <- 0
    }

    ndepths_modeled <- length(config$model_settings$modeled_depths)
    nmembers <- config$da_setup$ensemble_size
    nstates <- length(states_config$state_names)

    init$states <- array(NA, dim=c(nstates, config$model_settings$max_model_layers, nmembers))
    init$pars <- array(NA, dim=c(npars, nmembers))
    init$lake_depth <- array(NA, dim=c(nmembers))
    init$snow_ice_thickness <- array(NA, dim=c(3, nmembers))
    init$avg_surf_temp <- array(NA, dim=c(nmembers))
    init$mixing_vars <- array(NA, dim=c(17, nmembers))
    init$model_internal_heights <- array(NA, dim = c(config$model_settings$max_model_layers, nmembers))
    init$salt <- array(NA, dim = c(ndepths_modeled, nmembers))
    init$mixer_count <- array(NA, dim=c(nmembers))
    init$log_particle_weights <- array(NA, dim=c(nmembers))

    init$lake_depth[] <- round(config$default_init$lake_depth, 4)
    nml <- read_nml(file.path(config$file_path$configuration_directory, config$model_settings$base_GLM_nml))
    max_depth <- nml$morphometry$H[length(nml$morphometry$H)] - nml$morphometry$H[1]
    if(!is.null(obs_non_vertical$obs_depth)){
      if(!is.na(obs_non_vertical$obs_depth$obs[1])){
        init$lake_depth <- rnorm(nmembers, obs_non_vertical$obs_depth$obs[1], obs_non_vertical$obs_depth$depth_sd)
        index <- which(init$lake_depth > max_depth)
        init$lake_depth[index] <- max_depth
      }
    }

    for(m in 1:nmembers){
      init$model_internal_heights[1:ndepths_modeled, m] <- init$lake_depth[m] - config$model_settings$modeled_depths
    }



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


    model_sd <- array(NA, dim = c(nrow(states_config),length(config$model_settings$modeled_depths)))
    for(s in 1:nrow(model_sd)){
      model_sd[s, ] <- states_config$initial_model_sd[s]
    }

    for(m in 1:nmembers){

      init$model_internal_heights[1:ndepths_modeled, m] <- init$lake_depth[m] - config$model_settings$modeled_depths

      with_noise <- add_process_noise(states_height_ens = init_depth,
                                      model_sd = model_sd,
                                      model_internal_heights_ens =  init$model_internal_heights[ ,m],
                                      lake_depth_ens = init$lake_depth[m],
                                      modeled_depths = config$model_settings$modeled_depths,
                                      vert_decorr_length = states_config$vert_decorr_length,
                                      include_uncertainty = config$uncertainty$initial_condition)

      init$states[,1:ndepths_modeled , m] <- with_noise$states_height_ens

    }

    if(npars > 0){
      for(par in 1:npars){
        if(pars_config$fix_par[par] == 0){
          init$pars[par, ] <- runif(n=nmembers,pars_config$par_init_lowerbound[par], pars_config$par_init_upperbound[par])
        }else{
          init$pars[par, ] <- pars_config$par_init[par]
        }
      }
    }


    #Matrix to store snow and ice heights
    init$snow_ice_thickness[1, ] <- config$default_init$snow_thickness
    init$snow_ice_thickness[2, ] <- config$default_init$white_ice_thickness
    init$snow_ice_thickness[3, ] <- config$default_init$blue_ice_thickness
    init$avg_surf_temp[] <- init$states[1 , 1, ]
    init$mixing_vars[, ] <- 0.0
    init$mixer_count[] <- 0
    init$salt[, ] <- config$default_init$salinity
    init$log_particle_weights[] <- log(1.0)


    aux_states_init <- list()
    aux_states_init$snow_ice_thickness <- init$snow_ice_thickness
    aux_states_init$avg_surf_temp <- init$avg_surf_temp
    aux_states_init$the_sals_init <- config$the_sals_init
    aux_states_init$mixing_vars <- init$mixing_vars
    aux_states_init$mixer_count <- init$mixer_count
    aux_states_init$model_internal_heights <- init$model_internal_heights
    aux_states_init$lake_depth <- init$lake_depth
    aux_states_init$salt <- init$salt
    aux_states_init$log_particle_weights <- init$log_particle_weights

    init <- list(states = init$states,
                 pars = init$pars,
                 aux_states_init = aux_states_init)

  }else{
    nc <- ncdf4::nc_open(config$run_config$restart_file)
    #forecast <- ncdf4::ncvar_get(nc, "forecast")
    t <- ncdf4::ncvar_get(nc,'time')
    local_tzone <- ncdf4::ncatt_get(nc, 0)$local_time_zone_of_simulation
    datetime <- as.POSIXct(t,
                           origin = '1970-01-01 00:00.00 UTC',
                           tz = "UTC")
    ncdf4::nc_close(nc)


    restart_index <- which(datetime == lubridate::as_datetime(config$run_config$start_datetime))

    if(length(restart_index) != 1){
      warning("start_datetime for this simulation is missing from restart file")
    }

    out <- generate_restart_initial_conditions(
      restart_file = config$run_config$restart_file,
      state_names = states_config$state_names,
      par_names = pars_config$par_names_save,
      restart_index = restart_index)

    aux_states_init <- list()
    aux_states_init$snow_ice_thickness <- out$snow_ice_thickness
    aux_states_init$avg_surf_temp <- out$avg_surf_temp
    aux_states_init$the_sals_init <- config$the_sals_init
    aux_states_init$mixing_vars <- out$mixing_vars
    aux_states_init$mixer_count <- out$mixer_count
    aux_states_init$model_internal_heights <- out$model_internal_heights
    aux_states_init$lake_depth <- out$lake_depth
    aux_states_init$log_particle_weights <- out$log_particle_weights

    init <- list(states = out$states,
                 pars = out$pars,
                 aux_states_init = aux_states_init)

  }
  return(init)
}
