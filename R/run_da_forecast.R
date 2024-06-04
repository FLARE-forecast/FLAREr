#' @title Run ensemble data assimilation and/or produce forecasts
#'
#' @details Uses the ensemble data assimilation to predict water quality for a lake
#' or reservoir.  The function requires the initial conditions (`states_init`) for each
#' state and ensemble member using an array with the following dimension order:
#' states, depth, ensembles member.  If you are fitting parameters, it also requires
#' initial conditions for each parameter and ensemble member using an array (`par_init`) with the
#' following dimension order: parameters, ensemble member.  The arrays for states_init
#' and pars_init can be created using the `generate_initial_conditions()` function, if
#' starting from initial conditions in the  `states_config` data frame or from observations
#' in first time column of the `obs` array.
#'
#' @param states_init array of the initial states.  Required dimensions are `[states, depths, ensemble]`
#' @param pars_init array of the initial states.  Required dimensions are `[pars, depths, ensemble]`.  (Default = NULL)
#' @param aux_states_init list of initial conditions for auxillary states.  These are states in the GLM that
#' are require for restarting the model but are not included in data assimilation.  These are states that are not associated
#' with a value in `model_sd`.
#' @param obs array; array of the observations. Required dimensions are `[nobs, time, depth]`
#' @param obs_sd vector; vector of standard deviation for observation
#' @param model_sd vector vector of standard deviations describing the model error for each state
#' @param working_directory string; full path to directory where model executes
#' @param met_file_names vector; vector of full path meteorology file names
#' @param inflow_file_names vector or matrix;; vector of inflow file names
#' @param outflow_file_names vector or matrix; vector of outflow file names
#' @param config list; list of configurations
#' @param pars_config list; list of parameter configurations  (Default = NULL)
#' @param states_config list; list of state configurations
#' @param obs_config list; list of observation configurations
#' @param management list; list of management inputs and configuration  (Default = NULL)
#' @param da_method string; data assimilation method (enkf or pf; Default = enkf)
#' @param par_fit_method string; method for adding noise to parameters during calibration
#' @param debug boolean; add extra diagnostics for debugging (Default = FALSE)
#' @return a list is passed to `write_forecast_netcdf()` to write the
#' netcdf output and `create_flare_eml()` to generate the EML metadata
#' @export
#' @importFrom parallel clusterExport detectCores clusterEvalQ parLapply stopCluster
#' @importFrom GLM3r glm_version
#' @examples
##' \dontrun{
#' da_forecast_output <- FLAREr::run_da_forecast(states_init = init$states,
#'     pars_init = init$pars, aux_states_init = init$aux_states_init,
#'     obs = obs, obs_sd = obs_config$obs_sd, model_sd = model_sd,
#'     working_directory = config$file_path$execute_directory,
#'     met_file_names = met_file_names, inflow_file_names = inflow_file_names,
#'     outflow_file_names = outflow_file_names, config = config,
#'     pars_config = pars_config, states_config = states_config,
#'     obs_config = obs_config)
#' }

run_da_forecast <- function(states_init,
                            pars_init = NULL,
                            aux_states_init,
                            obs,
                            obs_sd,
                            model_sd,
                            working_directory,
                            met_file_names,
                            inflow_file_names = NULL,
                            outflow_file_names = NULL,
                            config,
                            pars_config = NULL,
                            states_config,
                            obs_config,
                            management = NULL,
                            da_method = "enkf",
                            par_fit_method = "inflate",
                            debug = FALSE,
                            log_wq = FALSE,
                            obs_secchi = NULL,
                            obs_depth = NULL){

  if(length(states_config$state_names) > 2){
    config$include_wq <- TRUE
  }else{
    config$include_wq <- FALSE
  }

  if(!("multi_depth" %in% names(obs_config))){
    obs_config <- obs_config |> dplyr::mutate(multi_depth = 1)
  }

  nstates <- dim(states_init)[1]
  ndepths_modeled <- dim(states_init)[2]
  nmembers <- dim(states_init)[3]
  n_met_members <- length(met_file_names)
  model <- config$model_settings$model
  if(!is.null(pars_config)){
    if("model" %in% names(pars_config)){
      pars_config <- pars_config[pars_config$model == model, ]
    }
    npars <- nrow(pars_config)
    par_names <- pars_config$par_names
    par_file <- pars_config$par_file
  }else{
    npars <- 0
    par_names <- NA
    par_file <- NA
  }

  # FLAREr:::check_enkf_inputs(states_init,
  #                            pars_init,
  #                            obs,
  #                            psi,
  #                            model_sd,
  #                            config,
  #                            pars_config,
  #                            states_config,
  #                            obs_config)

  start_datetime <- lubridate::as_datetime(config$run_config$start_datetime)
  if(is.na(config$run_config$forecast_start_datetime)){
    end_datetime <- lubridate::as_datetime(config$run_config$end_datetime)
    forecast_start_datetime <- end_datetime
  }else{
    forecast_start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime)
    end_datetime <- forecast_start_datetime + lubridate::days(config$run_config$forecast_horizon)
  }

  hist_days <- as.numeric(forecast_start_datetime - start_datetime)
  start_forecast_step <- 1 + hist_days
  full_time <- seq(start_datetime, end_datetime, by = "1 day")
  forecast_days <- as.numeric(end_datetime - forecast_start_datetime)
  nsteps <- length(full_time)

  data_assimilation_flag <- rep(NA, nsteps)
  forecast_flag <- rep(NA, nsteps)
  da_qc_flag <- rep(NA, nsteps)

  x <- array(NA, dim=c(nsteps, nstates, ndepths_modeled, nmembers))
  max_layers <- 500
  glm_native_x <- array(NA, dim=c(nsteps, nstates, max_layers, nmembers))

  for(m in 1:nmembers){
    for(s in 1:nstates){
      non_na_depths <- which(!is.na(aux_states_init$model_internal_depths[ ,m]))
      glm_native_x[1,s ,non_na_depths ,] <- states_init[s , ,m ]

      glm_depths_tmp <- c(aux_states_init$model_internal_depths[non_na_depths ,m], aux_states_init$lake_depth[m])
      glm_depths_mid <- glm_depths_tmp[1:(length(glm_depths_tmp)-1)] + diff(glm_depths_tmp)/2
      x[1,s, , m] <- approx(glm_depths_mid,glm_native_x[1, s, non_na_depths, m], config$model_settings$modeled_depths, rule = 2)$y
    }
  }

  if(npars > 0){
    pars <- array(NA, dim=c(nsteps, npars, nmembers))
    pars[1, , ] <- pars_init
  }else{
    pars <- NULL
  }

  alpha_v <- 1 - exp(-states_config$vert_decorr_length)

  output_vars <- states_config$state_names

  if(config$include_wq){
    num_wq_vars <- dim(x)[2] - 2
  }else{
    num_wq_vars <- 0
  }

  if(length(config$output_settings$diagnostics_names) > 0){
    diagnostics <- array(NA, dim=c(length(config$output_settings$diagnostics_names), nsteps, ndepths_modeled, nmembers))
  }else{
    diagnostics <- NA
  }

  num_phytos <- length(which(stringr::str_detect(states_config$state_names,"PHY_") & !stringr::str_detect(states_config$state_names,"_IP") & !stringr::str_detect(states_config$state_names,"_IN")))

  full_time_char <- strftime(full_time,
                             format="%Y-%m-%d %H:%M",
                             tz = "UTC")

  if(!is.null(inflow_file_names)){
    inflow_file_names <- as.matrix(inflow_file_names)
    outflow_file_names <- as.matrix(outflow_file_names)
  }else{
    inflow_file_names <- NULL
    outflow_file_names <- NULL
  }

  config$model_settings$ncore <- min(c(config$model_settings$ncore, parallel::detectCores()))
  if(config$model_settings$ncore == 1) {
    if(!dir.exists(file.path(working_directory, "1"))) {
      dir.create(file.path(working_directory, "1"), showWarnings = FALSE)
    } else {
      unlink(file.path(working_directory, "1"), recursive = TRUE)
      dir.create(file.path(working_directory, "1"), showWarnings = FALSE)
    }
    set_up_model(config,
                 ens_working_directory = file.path(working_directory,"1"),
                 state_names = states_config$state_names,
                 inflow_file_names = inflow_file_names,
                 outflow_file_names = outflow_file_names)
  } else {
    purrr::walk(1:nmembers, function(m){
      if(!dir.exists(file.path(working_directory, m))) {
        dir.create(file.path(working_directory, m), showWarnings = FALSE)
      } else {
        unlink(file.path(working_directory, m), recursive = TRUE)
        dir.create(file.path(working_directory, m), showWarnings = FALSE)
      }
      FLAREr:::set_up_model(config,
                            ens_working_directory = file.path(working_directory, m),
                            state_names = states_config$state_names,
                            inflow_file_names = inflow_file_names,
                            outflow_file_names = outflow_file_names)
    })
  }


  mixing_vars <- array(NA, dim = c(17, nsteps, nmembers))
  mixer_count <- array(NA, dim = c(nsteps, nmembers))

  model_internal_depths <- array(NA, dim = c(nsteps, 500, nmembers))
  lake_depth <- array(NA, dim = c(nsteps, nmembers))
  snow_ice_thickness <- array(NA, dim = c(3, nsteps, nmembers))
  avg_surf_temp <- array(NA, dim = c(nsteps, nmembers))

  mixing_vars[,1, ] <- aux_states_init$mixing_vars
  mixer_count[1, ] <- aux_states_init$mixer_count
  model_internal_depths[1, ,] <- aux_states_init$model_internal_depths
  lake_depth[1, ] <- aux_states_init$lake_depth
  snow_ice_thickness[,1 , ] <- aux_states_init$snow_ice_thickness
  avg_surf_temp[1, ] <- aux_states_init$avg_surf_temp

  if(config$da_setup$assimilate_first_step){
    start_step <- 1
  }else{
    start_step <- 2
  }

  # Print GLM version
  glm_v <- GLM3r::glm_version()
  glm_v <- substr(glm_v[3], 35, 58)
  message("Using GLM ", glm_v)
  config$metadata$model_description$version <- substr(glm_v, 9, 16)

  ###START EnKF

  for(i in start_step:nsteps){

    if(i > 1){
      curr_start <- strftime(full_time[i - 1],
                             format="%Y-%m-%d %H:%M",
                             tz = "UTC")
    }else{
      curr_start <- "Restart"
    }
    curr_stop <- strftime(full_time[i],
                          format="%Y-%m-%d %H:%M",
                          tz = "UTC")

    message(paste0("Running time step ", i-1, "/", (nsteps - 1), " : ",
                   curr_start, " - ",
                   curr_stop, " [", Sys.time(), "]"))

    #setwd(working_directory)

    met_index <- rep(1:length(met_file_names), times = nmembers)
    if(!is.null(ncol(inflow_file_names))) {
      inflow_outflow_index <- rep(1:nrow(inflow_file_names), times = nmembers)
    } else {
      inflow_outflow_index <- NULL
    }

    #Create array to hold GLM predictions for each ensemble
    x_star <- array(NA, dim = c(nstates, ndepths_modeled, nmembers))
    x_corr <- array(NA, dim = c(nstates, ndepths_modeled, nmembers))
    curr_pars <- array(NA, dim = c(npars, nmembers))

    #If i == 1 then assimilate the first time step without running the process
    #model (i.e., use yesterday's forecast of today as initial conditions and
    #assimilate new observations)
    if(i > 1){

      if(config$model_settings$ncore == 1){
        future::plan("future::sequential", workers = config$model_settings$ncore)
      }else{
        future::plan("future::multisession", workers = config$model_settings$ncore)
      }

      out <- furrr::future_map(1:nmembers, function(m) {

        ens_dir_index <- m
        if(config$model_settings$ncore == 1) ens_dir_index <- 1

        #setwd(file.path(working_directory, ens_dir_index))

        if(!config$uncertainty$weather & i >= (hist_days + 1)){
          curr_met_file <- met_file_names[met_index[1]]
        }else{
          curr_met_file <- met_file_names[met_index[m]]
        }

        curr_pars_ens <- FLAREr:::propose_parameters(m,
                                                     pars,
                                                     pars_config,
                                                     npars,
                                                     par_fit_method,
                                                     da_method,
                                                     hist_days,
                                                     include_uncertainty = config$uncertainty$parameter)

        if(!is.null(ncol(inflow_file_names))){
          if(!config$uncertainty$inflow & i > (hist_days + 1)){
            inflow_file_name <- inflow_file_names[inflow_outflow_index[1], ]
            outflow_file_name <- outflow_file_names[inflow_outflow_index[1], ]
          }else{
            inflow_file_name <- inflow_file_names[inflow_outflow_index[m], ]
            outflow_file_name <- outflow_file_names[inflow_outflow_index[m], ]
          }
        }else{
          inflow_file_name <- NULL
          outflow_file_name <- NULL
        }

        out <- run_model(i,
                         m,
                         mixing_vars_start = mixing_vars[,i-1 , m],
                         mixer_count = mixer_count,
                         curr_start,
                         curr_stop,
                         par_names,
                         curr_pars = curr_pars_ens,
                         ens_working_directory = file.path(working_directory, ens_dir_index),
                         par_nml = par_file,
                         num_phytos,
                         glm_depths_start = model_internal_depths[i-1, ,m ],
                         lake_depth_start = lake_depth[i-1, m],
                         x_start = x[i-1, , ,m ],
                         full_time,
                         wq_start = NULL,
                         wq_end = NULL,
                         management = management,
                         hist_days,
                         modeled_depths = config$model_settings$modeled_depths,
                         ndepths_modeled,
                         curr_met_file,
                         inflow_file_name = inflow_file_name,
                         outflow_file_name = outflow_file_name,
                         glm_output_vars = output_vars,
                         diagnostics_names = config$output_settings$diagnostics_names,
                         npars,
                         num_wq_vars,
                         snow_ice_thickness_start = snow_ice_thickness[, i-1, m ],
                         avg_surf_temp_start = avg_surf_temp[i-1, m],
                         nstates,
                         state_names = states_config$state_names,
                         include_wq = config$include_wq,
                         debug = debug,
                         glm_native_x_start = glm_native_x[i-1, , ,m]
        )

      }, .options = furrr::furrr_options(seed = TRUE))

      # Loop through output and assign to matrix
      for(m in 1:nmembers) {
        glm_native_x[i, , , m] <- out[[m]]$x_star_end
        lake_depth[i ,m ] <- out[[m]]$lake_depth_end
        snow_ice_thickness[,i ,m] <- out[[m]]$snow_ice_thickness_end
        avg_surf_temp[i , m] <- out[[m]]$avg_surf_temp_end
        mixing_vars[, i, m] <- out[[m]]$mixing_vars_end
        mixer_count[i, m] <- out[[m]]$mixer_count

        curr_pars[, m] <- out[[m]]$curr_pars

        num_out_depths <- length(out[[m]]$model_internal_depths)
        model_internal_depths[i,1:num_out_depths ,m] <- out[[m]]$model_internal_depths
        non_na_depths_index <- 1:num_out_depths


        glm_depths_tmp <- c(model_internal_depths[i,non_na_depths_index ,m], lake_depth[i ,m ])
        glm_depths_mid <- glm_depths_tmp[1:(length(glm_depths_tmp)-1)] + diff(glm_depths_tmp)/2
        for(s in 1:nstates){
          x_star[s, , m] <- approx(glm_depths_mid,glm_native_x[i,s , non_na_depths_index, m], config$model_settings$modeled_depths, rule = 2)$y
        }
        if(length(config$output_settings$diagnostics_names) > 0){
          for(d in 1:dim(diagnostics)[1]){
            diagnostics[d, i, , m] <- approx(glm_depths_mid,out[[m]]$diagnostics_end[d,non_na_depths_index], config$model_settings$modeled_depths, rule = 2)$y
          }
        }

        q_v <- rep(NA, num_out_depths)
        w <- rep(NA, num_out_depths)
        w_new <- rep(NA, num_out_depths)
        #Add process noise

        for(jj in 1:nrow(model_sd)){
          model_sd_depth <- approx(x = config$model_settings$modeled_depths,
                                   y = model_sd[jj, ],
                                   xout = model_internal_depths[i,1:num_out_depths ,m],
                                   rule = 2)$y

          w[] <- rnorm(num_out_depths, 0, 1)
          if(config$uncertainty$process == FALSE & i > (hist_days + 1)){
            w[] <- 0.0
          }
          for(kk in 1:num_out_depths){
            if(kk == 1){
              w_new[kk] <- w[kk]
            }else{
              alpha <- exp(-states_config$vert_decorr_length[jj] / (model_internal_depths[i,kk ,m]-model_internal_depths[i,kk-1 ,m]))
              w_new[kk] <- ((1 - alpha) * w_new[kk-1] +  alpha * w[kk])

              #w_new[kk] <- (alpha_v[jj] * w_new[kk-1] + sqrt(1 - alpha_v[jj]^2) * w[kk])
            }
            q_v[kk] <- w_new[kk] * model_sd_depth[kk]
            glm_native_x[i, jj, kk, m] <- glm_native_x[i, jj, kk, m] + q_v[kk]
          }
          x_corr[jj, ,m] <- approx(glm_depths_mid,glm_native_x[i, jj, non_na_depths_index, m], config$model_settings$modeled_depths, rule = 2)$y
        }
      } # END ENSEMBLE LOOP

      #Correct any negative water quality states
      if(length(states_config$state_names) > 1 & !log_wq){
        for(s in 2:nstates){
          for(k in 1:ndepths_modeled){
            index <- which(x_corr[s, k, ] < 0.0)
            x_corr[s, k, index] <- 0.0
          }
        }
      }

      if(npars > 0){
        pars_corr <- curr_pars
        if(npars == 1){
          pars_corr <- matrix(pars_corr,nrow = length(pars_corr),ncol = 1)
        }
        pars_star <- pars_corr
      }

    }else{
      stop("NEED TO FIX i = 1 ELSE ~ line 431")
      x_star <- x[i, , , ]
      x_corr <- x_star
      if(npars > 0){
        pars_corr <- pars[i, ,]
        if(npars == 1){
          pars_corr <- matrix(pars_corr,nrow = length(pars_corr),ncol = 1)
        }
        pars_star <- pars_corr
      }
    }

    if(dim(obs)[1] > 1){
      obs_count <- length(which(!is.na(c(aperm(obs[,i , ], perm = c(2,1))))))
    }else{
      obs_count <- length(which(!is.na(c(obs[1,i , ]))))
    }

    if(i > 1){
      #DON"T USE SECCHI ON DAY 1 BECAUSE THE DIAGONOSTIC OF LIGHT EXTINCTION
      #IS NOT IN THE RESTART FILE
      if(!is.null(obs_secchi$obs)){
        if(!is.na(obs_secchi$obs[i])){
          obs_count <- obs_count + 1
        }
      }
    }

    if(!is.null(obs_depth)){
      if(!is.na(obs_depth$obs[i])){
        obs_count <- obs_count + 1
      }
    }

    #if no observations at a time step then just propagate model uncertainty

    if(obs_count == 0 | config$da_setup$da_method == "none" | !config$da_setup$use_obs_constraint){

      if(i > (hist_days + 1)){
        data_assimilation_flag[i] <- 0
        forecast_flag[i] <- 1
        da_qc_flag[i] <- 0
      }else if(i <= (hist_days + 1) & config$da_setup$use_obs_constraint){
        data_assimilation_flag[i] <- 1
        forecast_flag[i] <- 0
        da_qc_flag[i] <- 1
      }else{
        data_assimilation_flag[i] <- 0
        forecast_flag[i] <- 0
        da_qc_flag[i] <- 0
      }

      x[i, , , ] <- x_corr
      if(npars > 0) pars[i, , ] <- pars_star

      if(i == (hist_days + 1) & config$uncertainty$initial_condition == FALSE){
        if(npars > 0) pars[i, , ] <- pars_star
        for(s in 1:nstates){
          for(k in 1:ndepths_modeled){
            x[i, s, k , ] <- mean(x_star[s, k, ])
          }
        }
      }

      for(s in 1:nstates){
        for(m in 1:nmembers){
          depth_index <- which(config$model_settings$modeled_depths > lake_depth[i, m])
          x[i, s, depth_index, m ] <- NA
        }
      }

      if(length(config$output_settings$diagnostics_names) > 0){
        for(d in 1:dim(diagnostics)[1]){
          for(m in 1:nmembers){
            depth_index <- which(config$model_settings$modeled_depths > lake_depth[i, m])
            diagnostics[d,i, depth_index, m] <- NA
          }
        }
      }

    }else{

      message("performing data assimilation")

      x_matrix <- apply(aperm(x_corr[,1:ndepths_modeled,], perm = c(2,1,3)), 3, rbind)
      if(length(config$output_settings$diagnostics_names) > 0 & i > 1){
        modeled_secchi <- 1.7 / diagnostics[1, i, which.min(abs(config$model_settings$modeled_depths-1.0)), ]
        if(!is.null(obs_secchi)){
          x_matrix <- rbind(x_matrix, modeled_secchi)
        }
      }

      if(!is.null(obs_depth) & da_method == "pf"){
        x_matrix <- rbind(x_matrix, lake_depth[i, ])
      }

      data_assimilation_flag[i] <- 1
      forecast_flag[i] <- 0
      da_qc_flag[i] <- 0

      curr_obs <- obs[,i,]

      vertical_obs <- length(which(obs_config$multi_depth == 1))

      if(dim(obs)[1] > 1){
        zt <- c(aperm(curr_obs, perm = c(2,1)))
      }else{
        zt <- curr_obs
      }

      zt <- zt[which(!is.na(zt))]

      secchi_index <- 0
      if(i > 1){
        if(!is.null(obs_secchi)){
          secchi_index <- 1
          if(!is.na(obs_secchi$obs[i])){
            if(!is.na(obs_secchi$obs[i])){
              zt <- c(zt, obs_secchi$obs[i])
            }
          }
        }
      }

      depth_index <- 0
      if(da_method == "pf"){
        if(!is.null(obs_depth)){
          depth_index <- 1
          if(!is.na(obs_depth$obs[i])){
            zt <- c(zt, obs_depth$obs[i])
          }
        }
      }

      #Assign which states have obs in the time step
      h <- matrix(0, nrow = vertical_obs * ndepths_modeled + secchi_index + depth_index, ncol = nstates * ndepths_modeled + secchi_index + depth_index)

      index <- 0
      for(k in 1:nstates){
        for(j in 1:ndepths_modeled){
          index <- index + 1
          if(!is.na(dplyr::first(states_config$states_to_obs[[k]]))){
            for(jj in 1:length(states_config$states_to_obs[[k]])){
              if(!is.na((obs[states_config$states_to_obs[[k]][jj], i, j]))){
                states_to_obs_index <- states_config$states_to_obs[[k]][jj]
                index2 <- (states_to_obs_index - 1) * ndepths_modeled + j
                h[index2,index] <- states_config$states_to_obs_mapping[[k]][jj]
              }
            }
          }
        }
      }

      if(!is.null(obs_secchi)){
        if(!is.na(obs_secchi$obs[i])){
          h[dim(h)[1] - depth_index, dim(h)[2] - depth_index] <- 1
        }
      }

      if(!is.null(obs_depth) & depth_index > 0){
        if(!is.na(obs_depth$obs[i])){
          h[dim(h)[1],dim(h)[2]] <- 1
        }
      }

      z_index <- c()
      for(j in 1:nrow(h)){
        if(sum(h[j, ]) > 0){
          z_index <- c(z_index, j)
        }
      }

      h <- h[z_index, ]

      if(!is.matrix(h)){
        h <- t(as.matrix(h))
      }

      psi <- rep(NA, vertical_obs * ndepths_modeled + secchi_index + depth_index)
      index <- 0
      for(k in 1:vertical_obs){
        for(j in 1:ndepths_modeled){
          index <- index + 1
          if(k == 1){
            psi[index] <- obs_sd[k]
          }else{
            psi[index] <- obs_sd[k]
          }
        }
      }

      if(secchi_index > 0){
        psi[vertical_obs * ndepths_modeled + secchi_index] <- obs_secchi$secchi_sd
      }

      if(depth_index > 0){
        psi[vertical_obs * ndepths_modeled + secchi_index + depth_index] <- obs_depth$depth_sd
      }

      curr_psi <- psi[z_index] ^ 2

      if(length(z_index) > 1){
        psi_t <- diag(curr_psi)
      }else{
        #Special case where there is only one data
        #type during the time-step
        psi_t <- curr_psi
      }

      if(!config$uncertainty$observation){
        psi_t[] <- 0.0
      }


      if(da_method == "enkf"){

        #Extract the data uncertainty for the data
        #types present during the time-step

        d_mat <- t(mvtnorm::rmvnorm(n = nmembers, mean = zt, sigma=as.matrix(psi_t)))

        #Set any negative observations of water quality variables to zero
        d_mat[which(z_index > ndepths_modeled & d_mat < 0.0)] <- 0.0

        #Ensemble mean
        ens_mean <- apply(x_matrix, 1, mean)

        if(npars > 0){
          par_mean <- apply(pars_corr, 1, mean)
          if(par_fit_method == "inflate"){
            for(m in 1:nmembers){
              pars_corr[, m] <- pars_config$inflat_pars * (pars_corr[, m] - par_mean) + par_mean
            }
            par_mean <- apply(pars_corr, 1, mean)
          }
        }

        dit <- matrix(NA, nrow = nmembers, ncol = dim(x_matrix)[1])

        if(npars > 0) dit_pars<- array(NA, dim = c(nmembers, npars))

        #Loop through ensemble members
        for(m in 1:nmembers){
          #  #Ensemble specific deviation
          dit[m, ] <- x_matrix[, m] - ens_mean
          if(npars > 0){
            dit_pars[m, ] <- pars_corr[, m] - par_mean
          }
          if(m == 1){
            p_it <- dit[m, ] %*% t(dit[m, ])
            if(npars > 0){
              p_it_pars <- dit_pars[m, ] %*% t(dit[m, ])
            }
          }else{
            p_it <- dit[m, ] %*% t(dit[m, ]) +  p_it
            if(npars > 0){
              p_it_pars <- dit_pars[m, ] %*% t(dit[m, ]) + p_it_pars
            }
          }
        }

        if(is.null(config$da_setup$inflation_factor)){
          config$da_setup$inflation_factor <- 1.0
        }

        #estimate covariance
        p_t <- config$da_setup$inflation_factor * (p_it / (nmembers - 1))
        if(npars > 0){
          p_t_pars <- config$da_setup$inflation_factor * (p_it_pars / (nmembers - 1))
        }

        if(!is.null(config$da_setup$localization_distance)){
          if(!is.na(config$da_setup$localization_distance)){
            p_t <- FLAREr:::localization(mat = p_t,
                                         nstates = nstates,
                                         modeled_depths = config$model_settings$modeled_depths,
                                         localization_distance = config$da_setup$localization_distance,
                                         num_single_states = dim(p_t)[1] - nstates * length(config$model_settings$modeled_depths))
          }
        }
        #Kalman gain
        k_t <- p_t %*% t(h) %*% solve(h %*% p_t %*% t(h) + psi_t, tol = 1e-17)
        if(npars > 0){
          k_t_pars <- p_t_pars %*% t(h) %*% solve(h %*% p_t %*% t(h) + psi_t, tol = 1e-17)
        }

        #Update states array (transposes are necessary to convert
        #between the dims here and the dims in the EnKF formulations)
        update <-  x_matrix + k_t %*% (d_mat - h %*% x_matrix)
        update <- update[1:(ndepths_modeled*nstates), ]
        update <- aperm(array(c(update), dim = c(ndepths_modeled, nstates, nmembers)), perm = c(2,1,3))

        if(!is.null(obs_depth)){
          if(!is.na(obs_depth$obs[i])){
            lake_depth[i, ] <- rnorm(nmembers, obs_depth$obs[i], sd = obs_depth$depth_sd)
            for(m in 1:nmembers){
              depth_index <- which(model_internal_depths[i, , m] > lake_depth[i, m])
              model_internal_depths[i,depth_index , m] <- NA
            }
          }
        }

        for(s in 1:nstates){
          for(m in 1:nmembers){
            depth_index <- which(config$model_settings$modeled_depths <= lake_depth[i, m])
            x[i, s, depth_index, m ] <- update[s,depth_index , m]

            #Map updates to GLM native depths
            native_depth_index <- which(!is.na(model_internal_depths[i, , m]))
            glm_native_x[i,s,native_depth_index,m] <- approx(config$model_settings$modeled_depths[depth_index],
                                                             x[i, s, depth_index, m ], model_internal_depths[i, , m],
                                                             rule = 2)$y
          }
        }
        if(length(config$output_settings$diagnostics_names) > 0){
          for(d in 1:dim(diagnostics)[1]){
            for(m in 1:nmembers){
              depth_index <- which(config$model_settings$modeled_depths > lake_depth[i, m])
              diagnostics[d,i, depth_index, m] <- NA
            }
          }
        }

        if(npars > 0){
          if(par_fit_method != "perturb_init"){
            pars[i, , ] <- pars_corr + k_t_pars %*% (d_mat - h %*% x_matrix)
          }else{
            pars[i, , ]  <- pars[i-1, , ]
          }
        }

      }else if(da_method == "pf"){

        print("here")

        obs_states <- t(h %*% x_matrix)

        LL <- rep(NA, length(nmembers))
        for(m in 1:nmembers){
          LL_vector <- dnorm(zt, mean = obs_states[m, ], sd = psi[z_index], log = TRUE)

          #This handles the case where an observed depth is missing from an ensemble member
          #due to the ensemble member being shallower than the depth of the observation
          na_index <- which(is.na(obs_states[m, ]))
          if(length(na_index) > 0){
            LL_vector[na_index] <- log(1.0e-20) #assign a very low probability density
          }
          LL[m] <- sum(LL_vector)
        }

        wt_norm <- exp(LL) / sum(exp(LL))
        samples <- sample.int(nmembers, replace = TRUE, prob = wt_norm)

        update <- x_matrix[1:(ndepths_modeled*nstates), samples]
        x[i, , , ] <- aperm(array(c(update), dim = c(ndepths_modeled, nstates, nmembers)), perm = c(2,1,3))
        glm_native_x[i, , , ] <-  glm_native_x[i, , ,samples]

        if(npars > 0){
          pars[i, ,] <- pars_star[, samples]
        }

        snow_ice_thickness[ ,i, ] <- snow_ice_thickness[ ,i, samples]
        avg_surf_temp[i, ] <- avg_surf_temp[i, samples]
        lake_depth[i, ] <- lake_depth[i, samples]
        model_internal_depths[i, , ] <- model_internal_depths[i, , samples]
        if(length(config$output_settings$diagnostics_names) > 0){
          diagnostics[ ,i, , ] <- diagnostics[ ,i, ,samples]
        }

      }else{
        message("da_method not supported; select enkf or pf or none")
      }
    }

    ###################
    ## Quality Control Step
    ##################

    #Correct any negative water quality states
    if(length(states_config$state_names) > 1 & !log_wq){
      for(s in 2:nstates){
        for(k in 1:ndepths_modeled){
          index <- which(x[i, s, k, ] < 0.0)
          x[i, s, k, index] <- 0.0
        }
      }
    }

    #Correct any parameter values outside bounds
    if(npars > 0){
      for(par in 1:npars){
        low_index <- which(pars[i,par ,] < pars_config$par_lowerbound[par])
        high_index <- which(pars[i,par ,] > pars_config$par_upperbound[par])
        pars[i,par, low_index] <- pars_config$par_lowerbound[par]
        pars[i,par, high_index]  <- pars_config$par_upperbound[par]
      }
    }

    ###############

    #Print parameters to screen
    if(npars > 0){
      for(par in 1:npars){
        message(paste0(pars_config$par_names_save[par],": mean ",
                       round(mean(pars[i,par ,]),4)," sd ",
                       round(sd(pars[i,par ,]),4)))
      }
    }
  }

  if(lubridate::day(full_time[1]) < 10){
    file_name_H_day <- paste0("0",lubridate::day(full_time[1]))
  }else{
    file_name_H_day <- lubridate::day(full_time[1])
  }
  if(lubridate::day(full_time[hist_days+1]) < 10){
    file_name_H_end_day <- paste0("0",lubridate::day(full_time[hist_days+1]))
  }else{
    file_name_H_end_day <- lubridate::day(full_time[hist_days+1])
  }
  if(lubridate::month(full_time[1]) < 10){
    file_name_H_month <- paste0("0",lubridate::month(full_time[1]))
  }else{
    file_name_H_month <- lubridate::month(full_time[1])
  }
  if(lubridate::month(full_time[hist_days+1]) < 10){
    file_name_H_end_month <- paste0("0",lubridate::month(full_time[hist_days+1]))
  }else{
    file_name_H_end_month <- lubridate::month(full_time[hist_days+1])
  }

  time_of_forecast <- Sys.time()
  curr_day <- lubridate::day(time_of_forecast)
  curr_month <- lubridate::month(time_of_forecast)
  curr_year <- lubridate::year(time_of_forecast)
  curr_hour <- lubridate::hour(time_of_forecast)
  curr_minute <- lubridate::minute(time_of_forecast)
  curr_second <- round(lubridate::second(time_of_forecast),0)
  if(curr_day < 10){curr_day <- paste0("0",curr_day)}
  if(curr_month < 10){curr_month <- paste0("0",curr_month)}
  if(curr_hour < 10){curr_hour <- paste0("0",curr_hour)}
  if(curr_minute < 10){curr_minute <- paste0("0",curr_minute)}
  if(curr_second < 10){curr_second <- paste0("0",curr_second)}

  forecast_iteration_id <- paste0(curr_year,
                                  curr_month,
                                  curr_day,
                                  "T",
                                  curr_hour,
                                  curr_minute,
                                  curr_second)

  save_file_name <- paste0(config$run_config$sim_name, "_H_",
                           (lubridate::year(full_time[1])),"_",
                           file_name_H_month,"_",
                           file_name_H_day,"_",
                           (lubridate::year(full_time[hist_days+1])),"_",
                           file_name_H_end_month,"_",
                           file_name_H_end_day,"_F_",
                           forecast_days,"_",
                           forecast_iteration_id)



  if(lubridate::day(full_time[hist_days+1]) < 10){
    file_name_F_day <- paste0("0",lubridate::day(full_time[hist_days+1]))
  }else{
    file_name_F_day <- lubridate::day(full_time[hist_days+1])
  }
  if(lubridate::month(full_time[hist_days+1]) < 10){
    file_name_F_month <- paste0("0",lubridate::month(full_time[hist_days+1]))
  }else{
    file_name_F_month <- lubridate::month(full_time[hist_days+1])
  }

  if(length(full_time) >= hist_days+1){
    save_file_name_short <- paste0(config$location$site_id, "-",
                                   (lubridate::year(full_time[hist_days+1])),"-",
                                   file_name_F_month,"-",
                                   file_name_F_day,"-",
                                   config$run_config$sim_name)
  }else{
    save_file_name_short <- paste0(config$location$site_id, "-",
                                   (lubridate::year(full_time[hist_days+1])),"-",
                                   file_name_F_month,"-",
                                   file_name_F_day,"-",
                                   paste0(config$run_config$sim_name,"_spinup"))
  }

  #for(m in 1:nmembers){
  #  unlink(file.path(working_directory, m), recursive = TRUE)
  #}


  return(list(full_time = full_time,
              forecast_start_datetime = forecast_start_datetime,
              x = x,
              glm_native_x = glm_native_x,
              pars = pars,
              obs = obs,
              save_file_name = save_file_name,
              save_file_name_short = save_file_name_short,
              forecast_iteration_id = forecast_iteration_id,
              forecast_project_id = config$run_config$sim_name,
              time_of_forecast = time_of_forecast,
              mixing_vars =  mixing_vars,
              mixer_count = mixer_count,
              snow_ice_thickness = snow_ice_thickness,
              avg_surf_temp = avg_surf_temp,
              lake_depth = lake_depth,
              model_internal_depths = model_internal_depths,
              diagnostics = diagnostics,
              data_assimilation_flag = data_assimilation_flag,
              forecast_flag = forecast_flag,
              da_qc_flag = da_qc_flag,
              config = config,
              states_config = states_config,
              pars_config = pars_config,
              obs_config = obs_config,
              met_file_names = met_file_names))
}
