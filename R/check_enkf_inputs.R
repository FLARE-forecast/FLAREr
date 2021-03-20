#' @title Download and Downscale NOAA GEFS for a single site
#' @return None
#'
#' @param site_index, index of site_list, lat_list, lon_list to be downloaded
#' @param lat_list, vector of latitudes that correspond to site codes
#' @param lon_list, vector of longitudes that correspond to site codes
#' @param site_list, vector of site codes, used in directory and file name generation
#' @param downscale, logical specifying whether to downscale from 6-hr to 1-hr
#' @param overwrite, logical stating to overwrite any existing output_file
#' @param model_name, directory name for the 6-hr forecast, this will be used in directory and file name generation
#' @param model_name_ds, directory name for the 1-hr forecast, this will be used in directory and file name generation
#' @param output_directory, directory where the model output will be save
#' @noRd
#'
#' @author Quinn Thomas
#'

check_enkf_inputs <- function(states_init,
                              pars_init,
                              obs,
                              psi,
                              model_sd,
                              config,
                              pars_config,
                              states_config,
                              obs_config){


  if(!is.null(pars_config)){
    npars <- nrow(pars_config)
  }else{
    npars <- 0
  }

  if(class(states_init) == "numeric"){
    stop("states_initneeds to be an array [number of ensemble members, nstates, ndepths]")
  }else if(length(dim(states_init)) > 3){
    stop("states_init has too many dimensions (should be 2)")
  }

  if(dim(pars_init)[2] != dim(states_init)[3]){
    stop("pars_init and states_init don't have same number of ensemble members")
  }

  if(nrow(model_sd) != nrow(states_config)){
    stop("rnow(model_sd) != nrow(states_config)")
  }

  if((dim(obs)[1] * dim(obs)[3]) != length(psi)){
    stop("(dim(obs)[2] * dim(obs)[2]) != nrow(psi)")
  }

  if(dim(obs)[1] != nrow(obs_config)){
    stop("dim(obs)[3] != nrow(obs_config)")
  }

  if(npars > 0){
    if(is.null(pars_config$par_names)){
      stop("is.null(pars_config$par_names)")
    }
    if(is.null(pars_config$par_names_save)){
      stop("is.null(pars_config$par_names_save)")
    }
    if(is.null(pars_config$par_file)){
      stop("is.null(pars_config$par_file)")
    }
    if(is.null(pars_config$par_lowerbound)){
      stop("is.null(pars_config$par_lowerbound)")
    }
    if(is.null(pars_config$par_upperbound)){
      stop("is.null(pars_config$par_upperbound)")
    }
    if(is.null(pars_config$inflat_pars)){
      stop("is.null(pars_config$inflat_pars)")
    }
    if(is.null(pars_config$par_units)){
      stop("is.null(pars_config$par_units)")
    }
  }
}
