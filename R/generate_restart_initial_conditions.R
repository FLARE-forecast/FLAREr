
#' Generate initial conditions from existing output file
#'
#' @param restart_file file name of FLARE output csv
#' @param state_names names of states that are initialized
#' @param par_names (optional) names of parameters that are initialized
#' @param restart_index (optional) time index in restart file used for initialization
#' @noRd
#' @return list of initial conditions
#'
generate_restart_initial_conditions <- function(restart_file, state_names, par_names = NULL, restart_index){

  nc <- ncdf4::nc_open(restart_file)
  restart_nmembers <- length(ncdf4::ncvar_get(nc, "ensemble"))

  message(paste0("Using restart file with restart index of ", restart_index))

  lake_depth_restart <- ncdf4::ncvar_get(nc, "lake_depth")[restart_index, ]
  snow_ice_thickness_restart <- ncdf4::ncvar_get(nc, "snow_ice_thickness")[, restart_index, ]
  avg_surf_temp_restart <- ncdf4::ncvar_get(nc, "avg_surf_temp")[restart_index, ]
  mixing_restart <- ncdf4::ncvar_get(nc, "mixing_vars")[ ,restart_index, ]
  mixer_count <- ncdf4::ncvar_get(nc, "mixer_count")[restart_index, ]
  log_particle_weights <- ncdf4::ncvar_get(nc, "log_particle_weights")[restart_index, ]
  model_internal_heights  <- ncdf4::ncvar_get(nc, "model_internal_heights")[restart_index, , ]

  states_restart <- array(NA, dim = c(length(state_names), dim(model_internal_heights)[1], restart_nmembers))
  for(i in 1:length(state_names)){
    states_restart[i, , ] <- ncdf4::ncvar_get(nc,paste0(state_names[i],"_heights"))[restart_index, , ]
  }

  if(!is.null(par_names)){
    pars_restart <- array(NA, dim = c(length(par_names), restart_nmembers))
    for(p in 1:length(par_names)){
      pars_restart[p, ] <- ncdf4::ncvar_get(nc, par_names[p])[restart_index, ]
    }
  }else{
    pars_restart <- NULL
  }

  ncdf4::nc_close(nc)

  return(list(states = states_restart,
              pars = pars_restart,
              lake_depth = lake_depth_restart,
              snow_ice_thickness = snow_ice_thickness_restart,
              avg_surf_temp = avg_surf_temp_restart,
              mixing_vars = mixing_restart,
              mixer_count = mixer_count,
              model_internal_heights = model_internal_heights,
              log_particle_weights = log_particle_weights)
  )
}
