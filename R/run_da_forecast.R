#' @title Run ensemble Kalman filter to assimilate observations and/or produce
#' forecasts
#'
#' @details Uses the ensemble Kalman filter to predict water quality for a lake
#' or reservior.  The function requires the initial conditions (`states_init`) for each
#' state and ensemble member using an array with the following dimension order:
#' states, depth, ensembles member.  If you are fitting parameters, it also requires
#' initial conditions for each parameter and ensemble member using an array (`par_init`) with the
#' following dimension order: parameters, ensemble member.  The arrays for states_init
#' and pars_init can be created using the `generate_initial_conditions()` function, if
#' starting from initial conditions in the  `states_config` data frame or from observations
#' in first time column of the `obs` array.  The arrays for `states_init` and `par_init`
#' can be created from the output from a previous run using the `generate_restart_initial_conditions()`
#' array.
#'
#' The required columns the `states_config` data frame with the following columns:
#' - `state_names`: the name in the GLM model for the state
#' - `initial_conditions`: the default initial condition for the state if an observation is lacking. Used in `generate_initial_conditions()`.  Note:
#' the `config` list should have a variables called `default_temp_init` and `default_temp_init_depths` that allow for depth variation in the initial
#' conditions for temperature.
#' - `model_sd`: the standard deviation of the model error for the state.  Matrix with dimensions rows = length(states_names), columns = length(config$model_settings$modeled_depths)
#' - `initial_model_sd`: the standard deviation of the initial conditions for the state. Used in `generate_initial_conditions()`
#' - `states_to_obs1`: the name of the observation that matches the model state
#' - `states_to_obs_mapping_1`: the multipler that converts the state to the observation (1 will be the most common)
#' - `init_obs_name`: the name of the observations that is used to generate `states_init`.  Used in `generate_initial_conditions()`
#' - `init_obs_mapping`: the multipler that converts the state to the observation (1 will be the most common). Used in `generate_initial_conditions()`
#'
#' The required columns in the `pars_config` are:
#' - `par_names`:  the name of the parameter in the GLM nml file
#' - `par_names_save`: the name of the parameter that will  be written in the output file.  This is different that
#' `par_names` because a single parameter can have multiple zones.  Therefore, `par_names` will be the same but
#' `par_names_save` will be different so that you know which zone they are associated with
#' - `par_nml`: GLM namelist that the `par_name` can be found: `glm.nml`, `aed2.nml`,`aed2_phyto_pars.nml`,`aed2_zoop_pars.nml`
#' - `par_init`:  Initial value for the parameter
#' - `par_init_lowerbound`: Lower bound when initilizing with a uniform distribution
#' - `par_init_upperbound`: Upper bound when initilizing with a uniform distribution
#' - `par_lowerbound`: Lower bound that the parameter can physically have
#' - `par_upperbound`: Upper bound that the parameter can physically have
#' - `inflat_pars`: Variance inflation factor for parameters (values are >= 1)
#' - `par_units`: Units of parameter
#'
#'  The required columns in the `obs_config` are:
#'  - `states_names_obs`: the name of the model state that the obervation represents
#'  - `obs_units`: unit of the observations
#'  - `obs_sd`:  the standard deviation of the normal distribution describing the
#'  uncertainty in the observation.
#'  - `target_variable`: the name of the observation in the in long format observation
#'  file.  Used by `create_obs_matrix()`
#'  - `distance_threshold`: the distance in meters that an observation is associated
#'  with a particular depth
#'
#'
#'
#'  The required variables in the `config` list are:
#'  - `lake_name_code`:
#'  - `lake_name`:
#'  - `lake_latitude`:
#'  - `lake_longitude`:
#'  - `local_tzone`:
#'  - `metadata`:
#'     - `generate_eml`:
#'     - `abstract`:
#'     - `forecast_title`:
#'     - `intellectualRights`:
#'     - `model_description`:
#'          - `name`:
#'          - `type`:
#'          - `repository`:
#'     - `me`:
#'          - `individualName`:
#'               - `givenName`:
#'               - `surName`:
#'          - `electronicMailAddress`:
#'          - `id`:
#'    - `forecast_project_id`
#'  - `model_name`:
#'  - `base_GLM_nml`:
#'  - `base_AED_nml`:
#'  - `base_AED_phyto_pars_nml`:
#'  - `base_AED_zoop_pars_nml`:
#'  - `use_obs_constraint`:
#'  - `observation_uncertainty`:
#'  - `process_uncertainty`:
#'  - `weather_uncertainty`:
#'  - `initial_condition_uncertainty`:
#'  - `parameter_uncertainty`:
#'  - `met_downscale_uncertainty`:
#'  - `inflow_process_uncertainty`:
#'  - `modeled_depths:`
#'  - `ensemble_size`:
#'  - `localization_distance`:
#'  - `vert_decorr_length`:
#'  - `no_negative_states`:
#'  - `diagnostics_names`:
#'
#' The `management` list is used to define the Side Stream Saturation oxygen system.  It is
#' not required (defaults to `NULL`).  If included it requires:
#' - `management_input`: A two column data frame: FLOW, OXY_oxy
#' - `simulate_sss`: logical whether to simulate the sss
#' - `forecast_sss_on`: logical whether to have sss on in forecast
#' - `sss_depth`: Depth (m) of the SSS inflow and outflow
#' - `use_specified_sss`: logical whether to use the data from `management_input`
#' or from `forecast_sss_flow` and `forecast_sss_oxy` when forecasting with sss on
#' - `specified_sss_inflow_file`: file path of the inflow sss file is supplied rather than interally generated
#' - `specified_sss_outflow_file`: file path of the outflow sss file is supplied rather than interally generated
#' - `forecast_sss_flow`: m3/day of water entering via sss
#' - `forecast_sss_oxy`: oxygen concentration (mmmol/m3) entered in the sss
#'
#'
#' @param states_init array of the initial states.  Required dimensions are `[states, depths, ensemble]`
#' @param pars_init array of the initial states.  Required dimensions are `[pars, depths, ensemble]`.  (Default = NULL)
#' @param aux_states_init list of initial conditions for auxillary states.  These are states in the GLM that
#' are require for restarting the model but are not included in data assimilation.  These are states that are not associated
#' with a value in `model_sd`.
#' @param obs array of the observaitons. Required dimensions are `[nobs, time, depth]`
#' @param obs_sd
#' @param model_sd vector of standard deviations describing the model error for each state
#' @param working_directory directory model executes
#' @param met_file_names vector of meterology file names
#' @param inflow_file_names vector of inflow file names
#' @param outflow_file_names vector of outflow file names
#' @param config list of configurations
#' @param pars_config list of parameter configurations  (Default = NULL)
#' @param states_config list of state configurations
#' @param obs_config list of observation configurations
#' @param management list of management inputs and configuration  (Default = NULL)
#' @param da_method data assimilation method (enkf or pf; Default = enkf)
#' @return enkf_output a list is passed to `write_forecast_netcdf()` to write the
#' netcdf output and `create_flare_eml()` to generate the EML metadata
#' @export
#' @importFrom parallel clusterExport detectCores clusterEvalQ parLapply stopCluster
#' @importFrom GLM3r glm_version
#' @example
#'
#'
#'  start_datetime_local <- lubridate(as_datetime("2018-07-12 07:00:00, tz = "EST"))
#'  end_datetime_local <- lubridate(as_datetime("2018-07-15 07:00:00, tz = "EST"))
#'  forecast_start_datetime <- lubridate(as_datetime("2018-07-13 07:00:00, tz = "EST"))
#'
#'enkf_output <- FLAREr::run_data_forecast(states_init = init$states,
#'               pars_init = init$pars,
#'               aux_states_init = aux_states_init,
#'               obs = obs,
#'               obs_sd = obs_config$obs_sd,
#'               model_sd = states_config$model_sd,
#'               working_directory = config$run_config$execute_location,
#'               met_file_names = met_file_names,
#'               inflow_file_names = inflow_file_names,
#'               outflow_file_names = outflow_file_names,
#'               start_datetime = start_datetime,
#'               end_datetime = end_datetime,
#'               forecast_start_datetime = forecast_start_datetime,
#'               config = config,
#'               pars_config = pars_config,
#'               states_config = states_config,
#'               obs_config = obs_config)
#'

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
                              par_fit_method = "inflate"){



  if(length(states_config$state_names) > 1){
    config$include_wq <- TRUE
  }else{
    config$include_wq <- FALSE
  }

  nstates <- dim(states_init)[1]
  ndepths_modeled <- dim(states_init)[2]
  nmembers <- dim(states_init)[3]
  n_met_members <- length(met_file_names)
  if(!is.null(pars_config)){
    npars <- nrow(pars_config)
    par_names <- pars_config$par_names
    par_nml <- pars_config$par_nml
  }else{
    npars <- 0
    par_names <- NA
    par_nml <- NA
  }

  x_init <- array(NA, dim = c(nmembers, nstates * ndepths_modeled + npars))
  for(m in 1:nmembers){
    if(nstates > 1){
      x_init[m,1:(nstates * ndepths_modeled)] <- c(aperm(states_init[, ,m ], perm = c(2,1)))
    }else{
      x_init[m,1:(nstates * ndepths_modeled)] <- states_init[1, ,m]
    }
    if(!is.null(pars_init) & npars > 0){
      x_init[m,(nstates * ndepths_modeled + 1):(nstates * ndepths_modeled + npars)] <- pars_init[, m]
    }
  }

  psi <- rep(NA, length(obs_sd) * ndepths_modeled)
  index <- 0
  for(i in 1:length(obs_sd)){
    for(j in 1:ndepths_modeled){
      index <- index + 1
      psi[index] <- obs_sd[i]
    }
  }

  wq_start <- rep(NA, nstates)
  wq_end <- rep(NA, nstates)
  for(wq in 1:nstates){
    if(wq == 1){
      wq_start[wq] <- 1
      wq_end[wq] <- ndepths_modeled
    }else{
      wq_start[wq] <- wq_end[wq-1]+1
      wq_end[wq] <- wq_end[wq-1] + (ndepths_modeled)
    }
  }

  states_config$wq_start <- wq_start
  states_config$wq_end <- wq_end

  FLAREr:::check_enkf_inputs(states_init,
                            pars_init,
                            obs,
                            psi,
                            model_sd,
                            config,
                            pars_config,
                            states_config,
                            obs_config)

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

  if(!is.null(pars_config)){
    npars <- nrow(pars_config)
    par_names <- pars_config$par_names
    par_nml <- pars_config$par_nml
  }else{
    npars <- 0
    par_names <- NA
    par_nml <- NA
  }

  nstates <- dim(x_init)[2] -  npars
  nsteps <- length(full_time)
  nmembers <- dim(x_init)[1]
  n_met_members <- length(met_file_names)
  ndepths_modeled <- length(config$model_settings$modeled_depths)

  data_assimilation_flag <- rep(NA, nsteps)
  forecast_flag <- rep(NA, nsteps)
  da_qc_flag <- rep(NA, nsteps)

  x <- array(NA, dim=c(nsteps, nmembers, nstates + npars))

  x[1, ,]  <- x_init

  q_v <- rep(NA, ndepths_modeled)
  w <- rep(NA, ndepths_modeled)
  w_new <- rep(NA, ndepths_modeled)

  alpha_v <- 1 - exp(-states_config$vert_decorr_length)


  glm_output_vars <- states_config$state_names


  if(config$include_wq){
    num_wq_vars <- length(states_config$wq_start) - 1
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

  x_prior <- array(NA, dim = c(nsteps, nmembers, nstates + npars))

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
    FLAREr:::set_up_model(config,
                         ens_working_directory = file.path(working_directory,"1"),
                         state_names = states_config$state_names,
                         inflow_file_names = inflow_file_names,
                         outflow_file_names = outflow_file_names)
  } else {
    lapply(1:nmembers, function(m){
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
  model_internal_depths <- array(NA, dim = c(nsteps, 500, nmembers))
  lake_depth <- array(NA, dim = c(nsteps, nmembers))
  snow_ice_thickness <- array(NA, dim = c(3, nsteps, nmembers))
  avg_surf_temp <- array(NA, dim = c(nsteps, nmembers))
  salt <- array(NA, dim = c(nsteps, ndepths_modeled, nmembers))

  mixing_vars[,1 ,] <- aux_states_init$mixing_vars
  model_internal_depths[1, ,] <- aux_states_init$model_internal_depths
  lake_depth[1, ] <- aux_states_init$lake_depth
  snow_ice_thickness[,1 , ] <- aux_states_init$snow_ice_thickness
  avg_surf_temp[1, ] <- aux_states_init$avg_surf_temp
  salt[1, , ] <- aux_states_init$salt

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

    curr_start <- strftime(full_time[i - 1],
                           format="%Y-%m-%d %H:%M",
                           tz = "UTC")
    curr_stop <- strftime(full_time[i],
                          format="%Y-%m-%d %H:%M",
                          tz = "UTC")

    message(paste0("Running time step ", i-1, "/", (nsteps - 1), " : ",
                   curr_start, " - ",
                   curr_stop, " [", Sys.time(), "]"))

    setwd(working_directory)

    met_index <- rep(1:length(met_file_names), times = nmembers)
    if(!is.null(ncol(inflow_file_names))) {
      inflow_outflow_index <- rep(1:nrow(inflow_file_names), times = nmembers)
    } else {
      inflow_outflow_index <- NULL
    }

    #Create array to hold GLM predictions for each ensemble
    x_star <- array(NA, dim = c(nmembers, nstates))
    x_corr <- array(NA, dim = c(nmembers, nstates))
    curr_pars <- array(NA, dim = c(nmembers, npars))

    #Matrix to store calculated ensemble specific deviations and innovations
    dit <- array(NA, dim = c(nmembers, nstates))

    if(npars > 0){
      pars_corr <-  array(NA, dim = c(nmembers, npars))
      dit_pars<- array(NA, dim = c(nmembers, npars))
    }

    # if i = start_step set up cluster for parallelization
    # Switch for
    switch(Sys.info() [["sysname"]],
           Linux = { machine <- "unix" },
           Darwin = { machine <- "mac" },
           Windows = { machine <- "windows"})
    if(i == start_step) {
      if(machine == "windows") {
        cl <- parallel::makeCluster(config$model_settings$ncore, setup_strategy = "sequential")
        parallel::clusterEvalQ(cl, library(FLAREr))
      } else {
        cl <- parallel::makeCluster(config$model_settings$ncore, setup_strategy = "sequential")
      }
      # Close parallel sockets on exit even if function is crashed or cancelled
      on.exit({
        tryCatch({parallel::stopCluster(cl)},
                 error = function(e) {
                   return(NA)
                 })
      })

      parallel::clusterExport(cl, varlist = list("working_directory", "met_file_names", "met_index",
                                                 "par_fit_method", "da_method", "nstates", "npars",
                                                 "pars_config", "inflow_file_names", "inflow_outflow_index",
                                                 "outflow_file_names", "curr_start",
                                                 "curr_stop", "par_names", "par_nml",
                                                 "num_phytos", "full_time", "management",
                                                 "hist_days", "config", "states_config",
                                                 "ndepths_modeled", "glm_output_vars", "num_wq_vars"),
                              envir = environment())
    }

    # Variables that need to be exported at each timestep
    parallel::clusterExport(cl, varlist = list("x", "i", "mixing_vars", "model_internal_depths", "lake_depth",
                                               "snow_ice_thickness", "avg_surf_temp", "salt"),
                            envir = environment())


    #If i == 1 then assimilate the first time step without running the process
    #model (i.e., use yesterday's forecast of today as initial conditions and
    #assimilate new observations)
    if(i > 1){

      out <- parallel::parLapply(cl, 1:nmembers, function(m) {
      #out <- lapply(1:nmembers, function(m) { # Commented out for debugging

        if(config$model_settings$ncore == 1){
          ens_dir_index <- 1
        }else{
          ens_dir_index <- m
        }

        setwd(file.path(working_directory, ens_dir_index))

        curr_met_file <- met_file_names[met_index[m]]

        if(npars > 0){
          if(par_fit_method == "inflate" & da_method == "enkf"){
            curr_pars_ens <- x[i - 1, m , (nstates+1):(nstates+ npars)]
          }else if(par_fit_method == "perturb" & da_method != "none"){
            if(i < (hist_days + 1)){
              curr_pars_ens <- x[i - 1, m , (nstates+1):(nstates+ npars)] + rnorm(npars, mean = rep(0, npars), sd = pars_config$perturb_par)
            }else{
              curr_pars_ens <- x[i - 1, m , (nstates+1):(nstates+ npars)]
            }
          }else if(da_method == "none"){
            curr_pars_ens <- x[i - 1, m , (nstates+1):(nstates+ npars)]
          }else{
            message("parameter fitting method not supported.  inflate or perturb are supported. only inflate is supported for enkf")

          }
        }else{
          curr_pars_ens <- NULL
        }

        if(!is.null(ncol(inflow_file_names))){
          inflow_file_name <- inflow_file_names[inflow_outflow_index[m], ]
          outflow_file_name <- outflow_file_names[inflow_outflow_index[m], ]
        }else{
          inflow_file_name <- NULL
          outflow_file_name <- NULL
        }

        out <-FLAREr:::run_model(i,
                                m,
                                mixing_vars_start = mixing_vars[,i-1 , m],
                                curr_start,
                                curr_stop,
                                par_names,
                                curr_pars = curr_pars_ens,
                                working_directory = file.path(working_directory, ens_dir_index),
                                par_nml,
                                num_phytos,
                                glm_depths_start = model_internal_depths[i-1, ,m ],
                                lake_depth_start = lake_depth[i-1, m],
                                x_start = x[i-1, m, ],
                                full_time,
                                wq_start = states_config$wq_start,
                                wq_end = states_config$wq_end,
                                management = management,
                                hist_days,
                                modeled_depths = config$model_settings$modeled_depths,
                                ndepths_modeled,
                                curr_met_file,
                                inflow_file_name = inflow_file_name,
                                outflow_file_name = outflow_file_name,
                                glm_output_vars = glm_output_vars,
                                diagnostics_names = config$output_settings$diagnostics_names,
                                npars,
                                num_wq_vars,
                                snow_ice_thickness_start = snow_ice_thickness[, i-1, m ],
                                avg_surf_temp_start = avg_surf_temp[i-1, m],
                                salt_start = salt[i-1, , m],
                                nstates,
                                state_names = states_config$state_names,
                                include_wq = config$include_wq)
      })

      # Loop through output and assign to matrix
      for(m in 1:nmembers) {
        x_star[m, ] <- out[[m]]$x_star_end
        lake_depth[i ,m ] <- out[[m]]$lake_depth_end
        snow_ice_thickness[,i ,m] <- out[[m]]$snow_ice_thickness_end
        avg_surf_temp[i , m] <- out[[m]]$avg_surf_temp_end
        mixing_vars[, i, m] <- out[[m]]$mixing_vars_end
        diagnostics[, i, , m] <- out[[m]]$diagnostics_end
        model_internal_depths[i, ,m] <- out[[m]]$model_internal_depths
        salt[i, , m]  <- out[[m]]$salt_end
        curr_pars[m, ] <- out[[m]]$curr_pars


        #Add process noise
        q_v[] <- NA
        w[] <- NA
        w_new[] <- NA
        for(jj in 1:nrow(model_sd)){
          w[] <- rnorm(ndepths_modeled, 0, 1)
          w_new[1] <- w[1]
          q_v[1] <- model_sd[jj, 1] * w_new[1]
          for(kk in 2:ndepths_modeled){
            #q_v[kk] <- alpha_v * q_v[kk-1] + sqrt(1 - alpha_v^2) * model_sd[jj, kk] * w[kk]

            w_new[kk] <- (alpha_v[jj] * w_new[kk-1] + sqrt(1 - alpha_v[jj]^2) * w[kk])
            q_v[kk] <- w_new[kk] * model_sd[jj, kk]
          }

          x_corr[m, (((jj-1)*ndepths_modeled)+1):(jj*ndepths_modeled)] <-
            x_star[m, (((jj-1)*ndepths_modeled)+1):(jj*ndepths_modeled)] + q_v
        }
      } # END ENSEMBLE LOOP

      #Correct any negative water quality states
      if(config$include_wq & config$da_setup$no_negative_states){
        for(m in 1:nmembers){
          index <- which(x_corr[m,] < 0.0)
          x_corr[m, index[which(index <= states_config$wq_end[num_wq_vars + 1] & index >= states_config$wq_start[2])]] <- 0.0
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
      x_star <- x[i, ,1:nstates]
      x_corr <- x_star
      if(npars > 0){
        pars_corr <- curr_pars
        if(npars == 1){
          pars_corr <- matrix(pars_corr,nrow = length(pars_corr),ncol = 1)
        }
        pars_star <- pars_corr
      }
    }

    if(npars > 0){
      x_prior[i, , ] <- cbind(x_corr, pars_corr)
    }else{
      x_prior[i, , ] <- x_corr
    }

    if(dim(obs)[1] > 1){
      z_index <- which(!is.na(c(aperm(obs[,i , ], perm = c(2,1)))))
    }else{
      z_index <- which(!is.na(c(obs[1,i , ])))
    }

    #if no observations at a time step then just propogate model uncertainity

    if(length(z_index) == 0 | da_method == "none"){

      if(i > (hist_days + 1)){
        data_assimilation_flag[i] <- 0
        forecast_flag[i] <- 1
        da_qc_flag[i] <- 0
      }else if(i <= (hist_days + 1) & config$use_obs_constraint){
        data_assimilation_flag[i] <- 1
        forecast_flag[i] <- 0
        da_qc_flag[i] <- 1
      }else{
        data_assimilation_flag[i] <- 0
        forecast_flag[i] <- 0
        da_qc_flag[i] <- 0
      }

      if(npars > 0){

        x[i, , ] <- cbind(x_corr, pars_star)

        if(config$uncertainty$process_uncertainty == FALSE & i > (hist_days + 1)){
          #don't add process noise if process uncertainty is false (x_star doesn't have noise)
          #don't add the noise to parameters in future forecast mode ()
          x[i, , ] <- cbind(x_star, pars_star)
        }

        if(i == (hist_days + 1) & config$uncertainty$initial_condition_uncertainty == FALSE){
          for(m in 1:nmembers){
            x[i, m, ] <- c(colMeans(x_star), pars_star[m, ])
          }
        }

      }else{
        x[i, , ] <- cbind(x_corr)

        if(config$uncertainty$process_uncertainty == FALSE & i > (hist_days + 1)){
          x[i, , ] <- x_star
        }

      }
    }else{

      data_assimilation_flag[i] <- 1
      forecast_flag[i] <- 0
      da_qc_flag[i] <- 0

      #if observation then calucate Kalman adjustment
      if(dim(obs)[1] > 1){
        zt <- c(aperm(obs[,i , ], perm = c(2,1)))
      }else{
        zt <- c(obs[1,i , ])
      }
      zt <- zt[which(!is.na(zt))]

      #Assign which states have obs in the time step
      h <- matrix(0, nrow = length(obs_sd) * ndepths_modeled, ncol = nstates)

      index <- 0
      for(k in 1:((nstates/ndepths_modeled))){
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

      if(da_method == "enkf"){

        #Extract the data uncertainity for the data
        #types present during the time-step

        curr_psi <- psi[z_index]  ^ 2

        if(length(z_index) > 1){
          psi_t <- diag(curr_psi)
        }else{
          #Special case where there is only one data
          #type during the time-step
          psi_t <- curr_psi
        }

        d_mat <- t(mvtnorm::rmvnorm(n = nmembers, mean = zt, sigma=as.matrix(psi_t)))

        #Set any negative observations of water quality variables to zero
        d_mat[which(z_index > length(config$model_settings$modeled_depths) & d_mat < 0.0)] <- 0.0

        #Ensemble mean
        ens_mean <- apply(x_corr[,], 2, mean)

        if(npars > 0){
          par_mean <- apply(pars_corr, 2, mean)
          if(par_fit_method == "inflate"){
            for(m in 1:nmembers){
              pars_corr[m, ] <- pars_config$inflat_pars * (pars_corr[m,] - par_mean) + par_mean
            }
            par_mean <- apply(pars_corr, 2, mean)
          }
        }


        #Loop through ensemble members
        for(m in 1:nmembers){
          #  #Ensemble specific deviation
          dit[m, ] <- x_corr[m, ] - ens_mean
          if(npars > 0){
            dit_pars[m, ] <- pars_corr[m, ] - par_mean
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

        #estimate covariance
        p_t <- p_it / (nmembers - 1)
        if(npars > 0){
          p_t_pars <- p_it_pars / (nmembers - 1)
        }

        if(!is.na(config$da_setup$localization_distance)){
          p_t <- localization(p_t,
                              nstates,
                              modeled_depths = config$model_settings$modeled_depths,
                              localization_distance = config$da_setup$localization_distance)
        }
        #Kalman gain
        k_t <- p_t %*% t(h) %*% solve(h %*% p_t %*% t(h) + psi_t, tol = 1e-17)
        if(npars > 0){
          k_t_pars <- p_t_pars %*% t(h) %*% solve(h %*% p_t %*% t(h) + psi_t, tol = 1e-17)
        }

        #Update states array (transposes are necessary to convert
        #between the dims here and the dims in the EnKF formulations)
        update_increment <-  k_t %*% (d_mat - h %*% t(x_corr))
        x[i, , 1:nstates] <- t(t(x_corr) + update_increment)
        if(npars > 0){
          x[i, , (nstates+1):(nstates+npars)] <- t(t(pars_corr) +
                                                     k_t_pars %*% (d_mat - h %*% t(x_corr)))
        }

      }else if(da_method == "pf"){


        obs_states <- t(h %*% t(x_corr))


        LL <- rep(NA, length(nmembers))
        for(m in 1:nmembers){
          LL[m] <- sum(dnorm(zt, mean = obs_states[m, ], sd = psi[z_index], log = TRUE))
        }

        sample <- sample.int(nmembers, replace = TRUE, prob = exp(LL))

        if(npars > 0){
        x[i, , ] <- cbind(x_corr, pars_star)[sample, ]
        }else{
          x[i, , ] <- cbind(x_corr)[sample, ]
        }

        snow_ice_thickness[ ,i, ] <- snow_ice_thickness[ ,i, sample]
        avg_surf_temp[i, ] <- avg_surf_temp[i, sample]
        lake_depth[i, ] <- lake_depth[i, sample]
        salt[i, , ] <- salt[i, ,sample]
        model_internal_depths[i, , ] <- model_internal_depths[i, , sample]
        diagnostics[ ,i, , ] <- diagnostics[ ,i, ,sample]

      }else{
        message("da_method not supported; select enkf or pf or none")
      }
    }

    #IF NO INITIAL CONDITION UNCERTAINITY THEN SET EACH ENSEMBLE MEMBER TO THE MEAN
    #AT THE INITIATION OF ThE FUTURE FORECAST
    if(i == (hist_days + 1)){

      if(config$uncertainty$initial_condition_uncertainty == FALSE){
        state_means <- colMeans(x[i, ,1:nstates])
        for(m in 1:nmembers){
          x[i, m, 1:nstates]  <- state_means
        }
      }
      if(npars > 0){
        if(config$uncertainty$parameter_uncertainty == FALSE){
          par_means <- colMeans(x[i, ,(nstates + 1):(nstates + npars)])
          for(m in 1:nmembers){
            x[i, m, (nstates + 1):(nstates + npars)] <- par_means
          }
        }
      }
    }

    ###################
    ## Quality Control Step
    ##################

    #Correct any negative water quality states
    if(config$include_wq & config$da_setup$no_negative_states){
      for(m in 1:nmembers){
        index <- which(x[i,m,] < 0.0)
        x[i, m, index[which(index <= states_config$wq_end[num_wq_vars + 1] & index >= states_config$wq_start[2])]] <- 0.0
      }
    }

    #Correct any parameter values outside bounds
    if(npars > 0){
      for(par in 1:npars){
        low_index <- which(x[i, ,nstates + par] < pars_config$par_lowerbound[par])
        high_index <- which(x[i, ,nstates + par] > pars_config$par_upperbound[par])
        x[i,low_index ,nstates + par] <- pars_config$par_lowerbound[par]
        x[i,high_index ,nstates + par] <- pars_config$par_upperbound[par]
      }
    }

    ###############

    #Print parameters to screen
    if(npars > 0){
      for(par in 1:npars){
        message(paste0(pars_config$par_names_save[par],": mean ",
                       round(mean(pars_corr[,par]),4)," sd ",
                       round(sd(pars_corr[,par]),4)))
      }
    }
  }

  if(lubridate::day(full_time[1]) < 10){
    file_name_H_day <- paste0("0",lubridate::day(full_time[1]))
  }else{
    file_name_H_day <- lubridate::day(full_time[1])
  }
  if(lubridate::day(full_time[hist_days+1]) < 10){
    file_name_F_day <- paste0("0",lubridate::day(full_time[hist_days+1]))
  }else{
    file_name_F_day <- lubridate::day(full_time[hist_days+1])
  }
  if(lubridate::month(full_time[1]) < 10){
    file_name_H_month <- paste0("0",lubridate::month(full_time[1]))
  }else{
    file_name_H_month <- lubridate::month(full_time[1])
  }
  if(lubridate::month(full_time[hist_days+1]) < 10){
    file_name_F_month <- paste0("0",lubridate::month(full_time[hist_days+1]))
  }else{
    file_name_F_month <- lubridate::month(full_time[hist_days+1])
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
                           file_name_F_month,"_",
                           file_name_F_day,"_F_",
                           forecast_days,"_",
                           forecast_iteration_id)

  for(m in 1:nmembers){
    unlink(file.path(working_directory, m), recursive = TRUE)
  }


  return(list(full_time = full_time,
              forecast_start_datetime = forecast_start_datetime,
              x = x,
              obs = obs,
              save_file_name = save_file_name,
              forecast_iteration_id = forecast_iteration_id,
              forecast_project_id = config$run_config$sim_name,
              time_of_forecast = time_of_forecast,
              mixing_vars =  mixing_vars,
              snow_ice_thickness = snow_ice_thickness,
              avg_surf_temp = avg_surf_temp,
              lake_depth = lake_depth,
              salt = salt,
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
