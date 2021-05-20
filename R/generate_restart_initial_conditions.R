
#' Generate initial conditions from existing output file
#'
#' @param restart_file file name of FLARE output csv
#' @param state_names names of states that are initialized
#' @param par_names (optional) names of parameters that are initialized
#' @param restart_index (optional) time index in restart file used for initalization
#' @noRd
#' @return
#'
#' @examples
generate_restart_initial_conditions <- function(restart_file, state_names, par_names = NULL, restart_index = NULL){



  nc <- ncdf4::nc_open(restart_file)
  restart_nmembers <- length(ncdf4::ncvar_get(nc, "ensemble"))
  forecast <- ncdf4::ncvar_get(nc, "forecast")
  if(is.null(restart_index)){
    restart_index <- max(which(forecast == 0))
    if(is.na(restart_index)){
      restart_index <- length(data_assimilation)
    }
  }

  print(paste0("Using restart file with restart index of ", restart_index))


  modeled_depths <- ncdf4::ncvar_get(nc, "depth")
  lake_depth_restart <- ncdf4::ncvar_get(nc, "lake_depth")[restart_index, ]
  snow_ice_thickness_restart <- ncdf4::ncvar_get(nc, "snow_ice_thickness")[, restart_index, ]
  avg_surf_temp_restart <- ncdf4::ncvar_get(nc, "avg_surf_temp")[restart_index, ]
  salt_restart <- ncdf4::ncvar_get(nc, "salt")[restart_index, , ]

  mixing_restart <- ncdf4::ncvar_get(nc, "mixing_vars")[ ,restart_index, ]
  model_internal_depths  <- ncdf4::ncvar_get(nc, "model_internal_depths")[restart_index, , ]

  states_restart <- array(NA, dim = c(length(state_names), length(modeled_depths), restart_nmembers))
  for(i in 1:length(state_names)){
    states_restart[i, , ] <- ncdf4::ncvar_get(nc,state_names[i])[restart_index, , ]
  }

  if(!is.null(par_names)){
    pars_restart <- array(NA, dim = c(length(par_names), restart_nmembers))
    for(i in 1:length(par_names)){
      pars_restart[i, ] <- ncdf4::ncvar_get(nc, par_names[i])[restart_index, ]
    }
  }else{
    pars_restart = NULL
  }

  ncdf4::nc_close(nc)

  return(list(states = states_restart,
              pars = pars_restart,
              lake_depth = lake_depth_restart,
              snow_ice_thickness = snow_ice_thickness_restart,
              avg_surf_temp = avg_surf_temp_restart,
              salt = salt_restart,
              mixing_vars = mixing_restart,
              model_internal_depths = model_internal_depths)
  )
}
