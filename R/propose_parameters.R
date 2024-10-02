#' Propose new parameters for use in data assimilation and forecasting
#'
#' @param i time step index
#' @param m ensemble member number
#' @param pars matrix of parameters
#' @param pars_config parameter configuration data frame
#' @param npars number of parameters
#' @param par_fit_method method for parameter fitting
#' @param da_method data assimilation method
#' @param hist_days number of simulation days before forecasting
#' @param include_uncertainty include parameter uncertainty in forecasts
#' @noRd
#' @return vector of new parameter values for the ensemble member
propose_parameters <- function(i, m, pars, pars_config, npars, par_fit_method, da_method, hist_days,
                               include_uncertainty){

  curr_pars_ens <- NULL

  if(npars > 0){

    if(!("fix_par" %in% names(pars_config))){
      pars_config <- pars_config |> dplyr::mutate(fix_par = 0)
    }

    curr_pars_ens <- rep(NA, npars)
    for(par in 1:npars){
      if(pars_config$fix_par[par] == 1){
        curr_pars_ens[par] <- pars_config$par_init[par]
      }else{
        if(par_fit_method == "inflate" & da_method == "enkf"){
          curr_pars_ens[par] <-  pars[i-1, par , m]
          if(i > (hist_days + 1) & !include_uncertainty){
            curr_pars_ens[par] <- mean(pars[i-1, par, ])
          }
        }else if(par_fit_method %in% c("perturb","perturb_const") & da_method != "none"){
          if(par_fit_method == "perturb_const"){
            par_mean <- mean(pars[i-1, par , ])
            par_sd <- sd(pars[i-1, par , ])

            par_z <- (pars[i-1, par ,m] - par_mean)/par_sd
            curr_pars_ens[par] <- par_z * pars_config$perturb_par[par] + par_mean

            if(i > (hist_days + 1) & !include_uncertainty){
              curr_pars_ens[par]  <- mean(pars[i-1, par, ])
            }

          }else{

            curr_pars_ens[par] <- pars[i-1, par , m] + rnorm(1, mean = 0, sd = pars_config$perturb_par[par])

            if(i > (hist_days + 1) & !include_uncertainty){
              curr_pars_ens[par] <- mean(pars[i-1, par, ])
            }
          }

        }else if(da_method == "none" | par_fit_method == "perturb_init"){
          curr_pars_ens[par] <- pars[i-1, par, m]

          if(i > (hist_days + 1) & !include_uncertainty){
            curr_pars_ens[par] <- mean(pars[i-1, par, ])
          }

        }else{
          message("parameter fitting method not supported.  inflate, perturb. perturb are supported")
        }

        if(curr_pars_ens[par] <  pars_config$par_lowerbound[par]) curr_pars_ens[par] <-  pars_config$par_lowerbound[par]
        if(curr_pars_ens[par] >  pars_config$par_upperbound[par]) curr_pars_ens[par] <-  pars_config$par_upperbound[par]
      }
    }
  }
  return(curr_pars_ens)
}
