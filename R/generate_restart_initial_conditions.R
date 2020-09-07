##' @title Download and Downscale NOAA GEFS for a single site
##' @return None
##'
##' @param site_index, index of site_list, lat_list, lon_list to be downloaded
##' @param lat_list, vector of latitudes that correspond to site codes
##' @param lon_list, vector of longitudes that correspond to site codes
##' @param site_list, vector of site codes, used in directory and file name generation
##' @param downscale, logical specifying whether to downscale from 6-hr to 1-hr
##' @param overwrite, logical stating to overwrite any existing output_file
##' @param model_name, directory name for the 6-hr forecast, this will be used in directory and file name generation
##' @param model_name_ds, directory name for the 1-hr forecast, this will be used in directory and file name generation
##' @param output_directory, directory where the model output will be save
##' @export
##'
##' @author Quinn Thomas
##'
##'

generate_restart_initial_conditions <- function(restart_file, config, states_config, pars_config = NULL){
  print("Using restart file")


  nc <- ncdf4::nc_open(restart_file)
  restart_nmembers <- length(ncdf4::ncvar_get(nc, "ensemble"))
  data_assimilation <- ncdf4::ncvar_get(nc, "data_assimilation")
  restart_index <- max(which(data_assimilation == 0)[1] -1, 2)
  modeled_depths <- ncdf4::ncvar_get(nc, "depth")
  lake_depth_restart <- ncdf4::ncvar_get(nc, "lake_depth")[restart_index, ]
  snow_ice_thickness_restart <- ncdf4::ncvar_get(nc, "snow_ice_thickness")[ ,restart_index, ]
  avg_surf_temp_restart <- ncdf4::ncvar_get(nc, "avg_surf_temp")[restart_index, ]
  salt_restart <- ncdf4::ncvar_get(nc, "salt")[restart_index, , ]

  mixing_restart <- ncdf4::ncvar_get(nc, "mixing_vars")[ ,restart_index, ]
  model_internal_depths  <- ncdf4::ncvar_get(nc, "model_internal_depths")[restart_index, , ]

  states_restart <- array(NA, dim = c(nrow(states_config), length(modeled_depths), restart_nmembers))
  for(i in 1:nrow(states_config)){
    states_restart[i, , ] <- ncdf4::ncvar_get(nc, states_config$state_names[i])[restart_index, , ]
  }

  if(!is.null(pars_config)){
    pars_restart <- array(NA, dim = c(nrow(pars_config), restart_nmembers))
    for(i in 1:nrow(pars_config)){
      pars_restart[i, ] <- ncdf4::ncvar_get(nc, pars_config$par_names_save[i])[restart_index, ]
    }
  }else{
    pars_restart = NULL
  }

  ncdf4::nc_close(nc)

  return(list(statest = states_restart,
              pars = pars_restart,
              lake_depth = lake_depth_restart,
              snow_ice_thickness = snow_ice_thickness_restart,
              avg_surf_temp = avg_surf_temp_restart,
              salt = salt_restart,
              mixing_vars = mixing_restart,
              model_internal_depths = model_internal_depths)
  )
}
