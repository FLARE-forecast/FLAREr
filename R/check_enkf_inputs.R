#' Check that inputs for run_da_forecast are correct
#'
#' @param states_init initial state vector
#' @param pars_init initial parameter vector
#' @param obs observation matrix
#' @param psi observation standard deviation matrix
#' @param model_sd vector of model standard deviations
#' @param config list of flare configurations
#' @param pars_config list of parameter configurations
#' @param states_config list of state configurations
#' @param obs_config list of observation configurations
#'
#' @return
#' @noRd
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

  if(!is.null(pars_init)){
    if(dim(pars_init)[2] != dim(states_init)[3]){
      stop("pars_init and states_init don't have same number of ensemble members")
    }
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
    if(is.null(pars_config$par_nml)){
      stop("is.null(pars_config$par_nml)")
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
