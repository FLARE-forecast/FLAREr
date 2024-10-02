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
#' @param da_method string; data assimilation method (enkf or pf; Default = enkf)
#' @param par_fit_method string; method for adding noise to parameters during calibration
#' @param obs_secchi list of secchi observations
#' @param obs_depth list of depth observations
#' @return a list
#'
#' @keywords internal


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
                            da_method = "enkf",
                            par_fit_method = "perturb",
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
  ndepths_modeled <- length(config$model_settings$modeled_depths)
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

  states_depth  <- array(NA, dim=c(nsteps, nstates, ndepths_modeled, nmembers))
  states_height <- array(NA, dim=c(nsteps, nstates, config$model_settings$max_model_layers, nmembers))

  for(m in 1:nmembers){
    for(s in 1:nstates){
      states_height[1,s , , m] <- states_init[s , ,m ]
      non_na_heights <- which(!is.na(aux_states_init$model_internal_heights[ ,m]))
      glm_depths <- aux_states_init$lake_depth[m] - aux_states_init$model_internal_heights[non_na_heights ,m]
      states_depth[1,s, , m] <- approx(glm_depths, states_height[1, s, non_na_heights, m], config$model_settings$modeled_depths, rule = 2)$y
    }
  }

  if(npars > 0){
    pars <- array(NA, dim=c(nsteps, npars, nmembers))
    pars[1, , ] <- pars_init
  }else{
    pars <- NULL
  }

  output_vars <- states_config$state_names

  if(config$include_wq){
    num_wq_vars <- dim(states_depth)[2] - 2
  }else{
    num_wq_vars <- 0
  }


  if(length(config$output_settings$diagnostics_names) > 0){
    diagnostics <- array(NA, dim=c(length(config$output_settings$diagnostics_names), nsteps, ndepths_modeled, nmembers))
  }else{
    diagnostics <- NA
  }

  if(length(config$output_settings$diagnostics_daily$names) > 0){
    diagnostics_daily <- array(NA, dim=c(length(config$output_settings$diagnostics_daily$names), nsteps, nmembers))
  }else{
    diagnostics_daily <- NA
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

  config$model_settings$ncore <- min(c(config$model_settings$ncore, future::availableCores()))
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
      set_up_model(config,
                   ens_working_directory = file.path(working_directory, m),
                   state_names = states_config$state_names,
                   inflow_file_names = inflow_file_names,
                   outflow_file_names = outflow_file_names)
    })
  }


  mixing_vars <- array(NA, dim = c(17, nsteps, nmembers))
  mixer_count <- array(NA, dim = c(nsteps, nmembers))
  model_internal_heights <- array(NA, dim = c(nsteps, config$model_settings$max_model_layers, nmembers))
  lake_depth <- array(NA, dim = c(nsteps, nmembers))
  snow_ice_thickness <- array(NA, dim = c(3, nsteps, nmembers))
  avg_surf_temp <- array(NA, dim = c(nsteps, nmembers))
  log_particle_weights <- array(NA, dim=c(nsteps, nmembers))

  mixing_vars[,1, ] <- aux_states_init$mixing_vars
  mixer_count[1, ] <- aux_states_init$mixer_count
  model_internal_heights[1, ,] <- aux_states_init$model_internal_heights
  lake_depth[1, ] <- aux_states_init$lake_depth
  snow_ice_thickness[,1 , ] <- aux_states_init$snow_ice_thickness
  avg_surf_temp[1, ] <- aux_states_init$avg_surf_temp
  log_particle_weights[1, ] <- aux_states_init$log_particle_weights

  if(config$da_setup$assimilate_first_step){
    start_step <- 1
  }else{
    start_step <- 2
  }

  # Print GLM version
  #glm_v <- suppressWarnings(GLM3r::glm_version())
  #glm_v <- substr(glm_v[3], 35, 58)
  #message("Using GLM ", glm_v)
  #config$metadata$model_description$version <- substr(glm_v, 9, 16)

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
    states_depth_wo_noise <- array(NA, dim = c(nstates, ndepths_modeled, nmembers))
    states_depth_w_noise <- array(NA, dim = c(nstates, ndepths_modeled, nmembers))
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

      orgin <- getwd()

      out <- furrr::future_map(1:nmembers, function(m) {

        ens_dir_index <- m
        if(config$model_settings$ncore == 1) ens_dir_index <- 1

        if(!config$uncertainty$weather & i >= (hist_days + 1)){
          curr_met_file <- met_file_names[met_index[1]]
        }else{
          curr_met_file <- met_file_names[met_index[m]]
        }

        curr_pars_ens <- propose_parameters(i, m,
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

        out <-  run_model(i,
                          m,
                          mixing_vars_start = mixing_vars[,i-1 , m],
                          mixer_count_start = mixer_count[i-1,m],
                          curr_start,
                          curr_stop,
                          par_names,
                          curr_pars = curr_pars_ens,
                          ens_working_directory = file.path(working_directory, ens_dir_index),
                          par_nml = par_file,
                          num_phytos,
                          glm_heights_start = model_internal_heights[i-1, ,m ],
                          lake_depth_start = lake_depth[i-1, m],
                          full_time,
                          hist_days,
                          modeled_depths = config$model_settings$modeled_depths,
                          ndepths_modeled,
                          curr_met_file,
                          inflow_file_name = inflow_file_name,
                          outflow_file_name = outflow_file_name,
                          glm_output_vars = output_vars,
                          diagnostics_names = config$output_settings$diagnostics_names,
                          diagnostics_daily_config = config$output_settings$diagnostics_daily,
                          npars,
                          num_wq_vars,
                          snow_ice_thickness_start = snow_ice_thickness[, i-1, m ],
                          avg_surf_temp_start = avg_surf_temp[i-1, m],
                          nstates,
                          state_names = states_config$state_names,
                          include_wq = config$include_wq,
                          max_layers = config$model_settings$max_model_layers,
                          states_heights_start = states_height[i-1, , ,m],
                          glm_path = config$model_settings$glm_path
        )

      }, .options = furrr::furrr_options(seed = TRUE))

      setwd(orgin)

      # Loop through output and assign to matrix
      for(m in 1:nmembers) {
        states_height[i, , , m] <- out[[m]]$x_star_end
        lake_depth[i ,m ] <- out[[m]]$lake_depth_end
        snow_ice_thickness[,i ,m] <- out[[m]]$snow_ice_thickness_end
        avg_surf_temp[i , m] <- out[[m]]$avg_surf_temp_end
        mixing_vars[, i, m] <- out[[m]]$mixing_vars_end
        mixer_count[i, m] <- out[[m]]$mixer_count_end

        curr_pars[, m] <- out[[m]]$curr_pars

        num_out_heights <- length(out[[m]]$model_internal_heights)
        model_internal_heights[i,1:num_out_heights ,m] <- out[[m]]$model_internal_heights
        non_na_heights_index <- 1:num_out_heights


        glm_depths <- lake_depth[i ,m ] - model_internal_heights[i,non_na_heights_index ,m]
        for(s in 1:nstates){
          states_depth_wo_noise[s, , m] <- approx(glm_depths,states_height[i,s , non_na_heights_index, m], config$model_settings$modeled_depths, rule = 2)$y
        }

        if(length(config$output_settings$diagnostics_names) > 0){
          for(d in 1:dim(diagnostics)[1]){
            diagnostics[d, i, , m] <- approx(glm_depths,out[[m]]$diagnostics_end[d,non_na_heights_index], config$model_settings$modeled_depths, rule = 2)$y
          }
        }

        if(length(config$output_settings$diagnostics_daily$names) > 0){
          for(d in 1:dim(diagnostics_daily)[1]){
            diagnostics_daily[d, i, m] <- out[[m]]$diagnostics_daily_end[d]
          }
        }


        if(config$uncertainty$process == FALSE & i > (hist_days + 1)){
          include_process_uncertainty <- FALSE
        }else{
          include_process_uncertainty <- TRUE
        }

        with_noise <- add_process_noise(states_height_ens = states_height[i, , , m],
                                        model_sd = model_sd,
                                        model_internal_heights_ens = model_internal_heights[i, ,m],
                                        lake_depth_ens = lake_depth[i,m],
                                        modeled_depths = config$model_settings$modeled_depths,
                                        vert_decorr_length = states_config$vert_decorr_length,
                                        include_uncertainty = include_process_uncertainty)
        states_depth_w_noise[, ,m] <- with_noise$states_depth_ens
        states_height[i, , , m] <- with_noise$states_height_ens

      } # END ENSEMBLE LOOP

      if(npars > 0){
        pars_corr <- curr_pars
        if(npars == 1){
          pars_corr <- matrix(pars_corr,nrow = length(pars_corr),ncol = 1)
        }
      }

    }else{

      for(m in 1:nmembers){
        non_na_heights_index <- which(!is.na(model_internal_heights[i, ,m]))
        glm_depths <-lake_depth[i ,m ] - model_internal_heights[i,non_na_heights_index ,m]

        for(s in 1:nstates){
          states_depth_wo_noise[s, , m] <- approx(glm_depths, states_height[i,s , non_na_heights_index, m], config$model_settings$modeled_depths, rule = 2)$y
          states_depth_w_noise[s, , m] <- states_depth_wo_noise[s, , m]
        }
      }

      if(npars > 0){
        pars_corr <- pars[i, ,]
        if(npars == 1){
          pars_corr <- matrix(pars_corr,nrow = length(pars_corr),ncol = 1)
        }
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

      states_depth[i, , , ] <- states_depth_w_noise

      log_particle_weights[i, ] <-   log_particle_weights[i-1, ]

      if(npars > 0) pars[i, , ] <- pars_corr

      if(i == (hist_days + 1) & config$uncertainty$initial_condition == FALSE){
        if(npars > 0) pars[i, , ] <- pars_corr
        for(s in 1:nstates){
          for(k in 1:ndepths_modeled){
            states_depth[i, s, k , ] <- mean(states_depth_wo_noise[s, k, ])
          }
        }
      }

      for(s in 1:nstates){
        for(m in 1:nmembers){
          depth_index <- which(config$model_settings$modeled_depths > lake_depth[i, m])
          states_depth[i, s, depth_index, m ] <- NA
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

      x_matrix <- apply(aperm(states_depth_w_noise[,1:ndepths_modeled,], perm = c(2,1,3)), 3, rbind)

      # Add depth to the x_matrix if in observations
      if(!is.null(obs_depth)){
        x_matrix <- rbind(x_matrix, lake_depth[i, ])
      }

      # Add secchi depth to the x_matrix if in observations
      if(length(config$output_settings$diagnostics_names) > 0 & i > 1){
        modeled_secchi <- 1.7 / diagnostics[1, i, which.min(abs(config$model_settings$modeled_depths-1.0)), ]
        if(!is.null(obs_secchi)){
          x_matrix <- rbind(x_matrix, modeled_secchi)
        }
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

      depth_index <- 0
      if(!is.null(obs_depth)){
        depth_index <- 1
        if(!is.na(obs_depth$obs[i])){
          zt <- c(zt, obs_depth$obs[i])
          depth_obs <- obs_depth$obs[i]
          depth_sd <- obs_depth$sd
        }
      }else{
        depth_obs <- NA
        depth_sd <- NA
      }

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

      if(!is.null(obs_depth) & depth_index > 0){
        if(!is.na(obs_depth$obs[i])){
          h[dim(h)[1],dim(h)[2]] <- 1
        }
      }

      if(!is.null(obs_secchi)){
        if(!is.na(obs_secchi$obs[i])){
          h[dim(h)[1] - depth_index, dim(h)[2] - depth_index] <- 1
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

      psi <- rep(NA, vertical_obs * ndepths_modeled + depth_index + secchi_index)
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

      if(depth_index > 0){
        psi[vertical_obs * ndepths_modeled + depth_index] <- obs_depth$depth_sd
      }


      if(secchi_index > 0){
        psi[vertical_obs * ndepths_modeled + depth_index +secchi_index] <- obs_secchi$secchi_sd
      }

      if(length(config$output_settings$diagnostics_names) > 0){
        diagnostics_start <- diagnostics[ ,i, , ]
      }else{
        diagnostics <- NA
      }

      if(length(config$output_settings$diagnostics_daily$names) > 0){
        diagnostics_daily_start <- diagnostics_daily[ ,i, ]
      }else{
        diagnostics_daily_start <- NA
      }

      if(da_method == "enkf"){

        updates <- run_enkf(x_matrix,
                            h,
                            pars_corr,
                            zt,
                            psi,
                            z_index,
                            states_depth_start = states_height[i, , , ],
                            states_height_start = states_height[i, , ,],
                            model_internal_heights_start = model_internal_heights[i, , ],
                            lake_depth_start = lake_depth[i, ],
                            log_particle_weights_start = log_particle_weights[i-1, ],
                            snow_ice_thickness_start =  snow_ice_thickness[ ,i, ],
                            avg_surf_temp_start = avg_surf_temp[i, ],
                            mixer_count_start = mixer_count[i, ],
                            mixing_vars_start = mixing_vars[, i, ],
                            diagnostics_start = diagnostics_start,
                            diagnostics_daily_start = diagnostics_daily_start,
                            pars_config,
                            config,
                            depth_index,
                            secchi_index,
                            depth_obs,
                            depth_sd,
                            par_fit_method)

      }else if(da_method == "pf"){

        updates <- run_particle_filter(x_matrix,
                                       h,
                                       pars_corr,
                                       zt,
                                       psi,
                                       z_index,
                                       states_depth_start = states_height[i, , , ],
                                       states_height_start = states_height[i, , ,],
                                       model_internal_heights_start = model_internal_heights[i, , ],
                                       lake_depth_start = lake_depth[i, ],
                                       log_particle_weights_start = log_particle_weights[i-1, ],
                                       snow_ice_thickness_start =  snow_ice_thickness[ ,i, ],
                                       avg_surf_temp_start = avg_surf_temp[i, ],
                                       mixer_count_start = mixer_count[i, ],
                                       mixing_vars_start = mixing_vars[, i, ],
                                       diagnostics_start = diagnostics_start,
                                       diagnostics_daily_start = diagnostics_daily_start,
                                       pars_config,
                                       config,
                                       depth_index,
                                       secchi_index,
                                       depth_obs,
                                       depth_sd,
                                       par_fit_method,
                                       vertical_obs,
                                       working_directory,
                                       obs_config)

      }else{
        stop("da_method not supported; select enkf or pf or none")
      }

      #Update states and parameters
      pars[i, , ] <- updates$pars_updated
      model_internal_heights[i, ,] <- updates$model_internal_heights_updated
      states_height[i,,,] <- updates$states_height_updated
      states_depth[i, , ,  ] <- updates$states_depth_updated

      if(length(config$output_settings$diagnostics_names) > 0){
        diagnostics[,i, , ] <-  updates$diagnostics_updated
      }else{
        diagnostics <-  updates$diagnostics_updated
      }

      if(length(config$output_settings$diagnostics_daily$names) > 0){
        diagnostics_daily[ ,i, ] <- updates$diagnostics_daily_updated
      }else{
        diagnostics_daily <- updates$diagnostics_daily_updated

      }
      lake_depth[i, ] <-  updates$lake_depth_updated
      log_particle_weights[i, ] <-  updates$log_particle_weights_updated
      snow_ice_thickness[,i ,] <-  updates$snow_ice_thickness_updated
      avg_surf_temp[i , ] <-  updates$avg_surf_temp_updated
      mixing_vars[, i, ] <-  updates$mixing_vars_updated
      mixer_count[i, ] <-  updates$mixer_count_updated

      for(s in 1:nstates){
        for(m in 1:nmembers){
          depth_index <- which(config$model_settings$modeled_depths > lake_depth[i, m])
          states_depth[i, s, depth_index, m ] <- NA
        }
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

    ###############

    #Print parameters to screen
    if(npars > 0){
      for(par in 1:npars){
        if(pars_config$fix_par[par] == 0){
          message(paste0(pars_config$par_names_save[par],": mean ",
                         round(mean(pars[i,par ,]),4)," sd ",
                         round(sd(pars[i,par ,]),4)))
        }
      }
    }
  }

  file_names <- create_filenames(full_time, hist_days, forecast_days, config)

  return(list(full_time = full_time,
              forecast_start_datetime = forecast_start_datetime,
              states_depth = states_depth,
              states_height = states_height,
              pars = pars,
              obs = obs,
              save_file_name = file_names$save_file_name,
              save_file_name_short = file_names$save_file_name_short,
              forecast_iteration_id = file_names$forecast_iteration_id,
              forecast_project_id = config$run_config$sim_name,
              time_of_forecast = file_names$time_of_forecast,
              mixing_vars =  mixing_vars,
              mixer_count = mixer_count,
              snow_ice_thickness = snow_ice_thickness,
              avg_surf_temp = avg_surf_temp,
              lake_depth = lake_depth,
              model_internal_heights = model_internal_heights,
              diagnostics = diagnostics,
              diagnostics_daily = diagnostics_daily,
              data_assimilation_flag = data_assimilation_flag,
              forecast_flag = forecast_flag,
              da_qc_flag = da_qc_flag,
              config = config,
              states_config = states_config,
              pars_config = pars_config,
              obs_config = obs_config,
              met_file_names = met_file_names,
              log_particle_weights = log_particle_weights))
}
