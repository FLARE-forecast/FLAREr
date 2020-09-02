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

generate_restart_initial_conditions <- function(restart_file, config){
  print("Using restart file")
  nc <- nc_open(restart_file)
  restart_nmembers <- length(ncdf4::ncvar_get(nc, "ensemble"))
  surface_height_restart <- ncdf4::ncvar_get(nc, "surface_height_restart")
  snow_ice_thickness_restart <- ncdf4::ncvar_get(nc, "snow_ice_restart")
  avg_surf_temp_restart <- ncdf4::ncvar_get(nc, "avg_surf_temp_restart")
  mixing_restart <- ncdf4::ncvar_get(nc, "mixing_restart")
  glm_depths_restart  <- ncdf4::ncvar_get(nc, "depths_restart")
  restart_x_previous <- ncdf4::ncvar_get(nc, "x_restart")
  ncdf4::nc_close(nc)

  glm_depths <- array(NA, dim = c(config$nmembers, dim(glm_depths_restart)[2]))
  snow_ice_thickness <-  array(NA, dim = c(config$nmembers, 3))

  if(restart_nmembers > config$nmembers){
    #sample restart_nmembers
    sampled_nmembers <- sample(seq(1, restart_nmembers, 1),
                               nmembers,
                               replace=FALSE)

    x_previous <- restart_x_previous[sampled_nmembers, ]
    snow_ice_thickness[ , 1] <- snow_ice_thickness_restart[sampled_nmembers, 1]
    snow_ice_thickness[, 2] <- snow_ice_thickness_restart[sampled_nmembers, 2]
    snow_ice_thickness[, 3] <- snow_ice_thickness_restart[sampled_nmembers, 3]
    surface_height <- surface_height_restart[sampled_nmembers]
    avg_surf_temp <- avg_surf_temp_restart[sampled_nmembers]
    mixing_vars <- mixing_restart[sampled_nmembers, ]

    for(m in 1:nmembers){
      glm_depths[m ,1:dim(glm_depths_restart)[2]] <- glm_depths_restart[sampled_nmembers[m], ]
    }

  }else if(restart_nmembers < nmembers){
    sampled_nmembers <- sample(seq(1, restart_nmembers, 1),
                               nmembers,
                               replace = TRUE)
    x_previous <- restart_x_previous[sampled_nmembers, ]
    snow_ice_thickness[ ,1] <- snow_ice_thickness_restart[sampled_nmembers, 1]
    snow_ice_thickness[ ,2] <- snow_ice_thickness_restart[sampled_nmembers, 2]
    snow_ice_thickness[ ,3] <- snow_ice_thickness_restart[sampled_nmembers, 3]
    surface_height <- surface_height_restart[sampled_nmembers]
    avg_surf_temp <- avg_surf_temp_restart[sampled_nmembers]
    mixing_vars <- mixing_restart[sampled_nmembers, ]

    for(m in 1:nmembers){
      glm_depths[m ,1:dim(glm_depths_restart)[2]] <- glm_depths_restart[sampled_nmembers[m], ]
    }

  }else{
    x_previous <- restart_x_previous
    snow_ice_thickness_init[ ,1] <- snow_ice_thickness_restart[, 1]
    snow_ice_thickness[ ,2] <- snow_ice_thickness_restart[, 2]
    snow_ice_thickness[ ,3] <- snow_ice_thickness_restart[, 3]
    surface_height <- surface_height_restart
    avg_surf_temp <- avg_surf_temp_restart
    mixing_vars <- mixing_restart

    for(m in 1:nmembers){
      glm_depths[m ,1:dim(glm_depths_restart)[2]] <- glm_depths_restart[m, ]
    }
  }

  return(list(x = x_previous,
              surface_height = surface_height,
              avg_surf_temp = avg_surf_temp,
              mixing_vars = mixing_vars,
              glm_depths = glm_depths,
              snow_ice_thickness = snow_ice_thickness)
  )
}
