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
#' @return a named list with the following elements:
#'   \describe{
#'     \item{full_time}{vector of all modeled datetimes}
#'     \item{forecast_start_datetime}{datetime when the forecast period begins}
#'     \item{states_depth}{array \[states, depths, time, ensemble\] of DA-updated model states indexed by depth}
#'     \item{states_height}{array \[states, heights, time, ensemble\] of DA-updated model states indexed by GLM internal height}
#'     \item{pars}{array \[pars, ensemble\] of DA-updated parameter values}
#'     \item{obs}{observation array passed through unchanged}
#'     \item{save_file_name}{full output filename stem (includes history period)}
#'     \item{save_file_name_short}{short output filename stem (forecast start date only)}
#'     \item{forecast_iteration_id}{timestamp string identifying this forecast run}
#'     \item{forecast_project_id}{sim_name from run config}
#'     \item{time_of_forecast}{POSIXct timestamp when forecast was generated}
#'     \item{snow_ice_thickness}{GLM restart variable}
#'     \item{lake_depth}{array \[time, ensemble\] of lake depths}
#'     \item{model_internal_heights}{array of GLM internal layer heights}
#'     \item{diagnostics}{array of per-timestep diagnostic variables}
#'     \item{diagnostics_daily}{array of daily diagnostic variables}
#'     \item{data_assimilation_flag, forecast_flag, da_qc_flag}{integer vectors flagging DA/forecast/QC status per timestep}
#'     \item{config, states_config, pars_config, obs_config}{configuration lists passed through}
#'     \item{met_file_names}{meteorology file paths used}
#'     \item{log_particle_weights}{log particle weights (particle filter only; NULL for EnKF)}
#'     \item{inflation}{covariance inflation factor}
#'     \item{glm_restart_staged}{path to the staged GLM restart file}
#'   }
#'
#' @keywords internal

# Allocate and fill all output arrays with initial conditions before the main
# data-assimilation loop.  Keeping this separate lets the loop body focus on
# updating state rather than housekeeping.
#
# @return named list of pre-allocated arrays ready for the DA loop
# @noRd
initialize_forecast_arrays <- function(nsteps, nstates, ndepths_modeled,
                                        nmembers, npars, pars_init,
                                        config, aux_states_init, states_init) {

  states_height <- array(
    NA,
    dim = c(nsteps, nstates, config$model_settings$max_model_layers, nmembers)
  )
  states_depth <- array(NA, dim = c(nsteps, nstates, ndepths_modeled, nmembers))

  for (m in 1:nmembers) {
    for (s in 1:nstates) {
      states_height[1, s, , m] <- states_init[s, , m]
      non_na_heights <- which(!is.na(aux_states_init$model_internal_heights[, m]))
      glm_depths <- aux_states_init$lake_depth[m] -
        aux_states_init$model_internal_heights[non_na_heights, m]
      states_depth[1, s, , m] <- approx(
        glm_depths, states_height[1, s, non_na_heights, m],
        config$model_settings$modeled_depths, rule = 2
      )$y
    }
  }

  pars <- if (npars > 0) {
    p <- array(NA, dim = c(nsteps, npars, nmembers))
    p[1, , ] <- pars_init
    p
  } else {
    NULL
  }

  num_wq_vars <- if (config$include_wq) dim(states_depth)[2] - 2L else 0L

  diagnostics <- if (length(config$output_settings$diagnostics_names) > 0) {
    array(
      NA,
      dim = c(length(config$output_settings$diagnostics_names),
              nsteps, ndepths_modeled, nmembers)
    )
  } else {
    NA
  }

  diagnostics_daily <- if (length(config$output_settings$diagnostics_daily$names) > 0) {
    array(
      NA,
      dim = c(length(config$output_settings$diagnostics_daily$names), nsteps, nmembers)
    )
  } else {
    NA
  }

  model_internal_heights <- array(
    NA, dim = c(nsteps, config$model_settings$max_model_layers, nmembers)
  )
  lake_depth             <- array(NA, dim = c(nsteps, nmembers))
  snow_ice_thickness     <- array(NA, dim = c(3, nsteps, nmembers))
  log_particle_weights   <- array(NA, dim = c(nsteps, nmembers))
  inflation              <- rep(NA, nsteps)

  model_internal_heights[1, , ] <- aux_states_init$model_internal_heights
  lake_depth[1, ]             <- aux_states_init$lake_depth
  snow_ice_thickness[, 1, ]   <- aux_states_init$snow_ice_thickness
  log_particle_weights[1, ]   <- aux_states_init$log_particle_weights
  inflation[1]                <- aux_states_init$inflation

  list(
    states_depth           = states_depth,
    states_height          = states_height,
    pars                   = pars,
    num_wq_vars            = num_wq_vars,
    diagnostics            = diagnostics,
    diagnostics_daily      = diagnostics_daily,
    model_internal_heights = model_internal_heights,
    lake_depth             = lake_depth,
    snow_ice_thickness     = snow_ice_thickness,
    log_particle_weights   = log_particle_weights,
    inflation              = inflation
  )
}


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
                            obs_non_vertical = NULL,
                            states_non_vertical = NULL){

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

  # --- Array initialisation ---
  arrs <- initialize_forecast_arrays(
    nsteps, nstates, ndepths_modeled, nmembers, npars, pars_init,
    config, aux_states_init, states_init
  )
  states_depth           <- arrs$states_depth
  states_height          <- arrs$states_height
  pars                   <- arrs$pars
  num_wq_vars            <- arrs$num_wq_vars
  diagnostics            <- arrs$diagnostics
  diagnostics_daily      <- arrs$diagnostics_daily
  model_internal_heights <- arrs$model_internal_heights
  lake_depth             <- arrs$lake_depth
  snow_ice_thickness     <- arrs$snow_ice_thickness
  log_particle_weights   <- arrs$log_particle_weights
  inflation              <- arrs$inflation

  output_vars <- states_config$state_names

  num_phytos <- length(which(
    stringr::str_detect(states_config$state_names, "PHY_") &
      !stringr::str_detect(states_config$state_names, "_IP") &
      !stringr::str_detect(states_config$state_names, "_IN")
  ))

  full_time_char <- strftime(full_time, format = "%Y-%m-%d %H:%M", tz = "UTC")

  if (!is.null(inflow_file_names)) {
    inflow_file_names  <- as.matrix(inflow_file_names)
    outflow_file_names <- as.matrix(outflow_file_names)
  } else {
    inflow_file_names  <- NULL
    outflow_file_names <- NULL
  }

  # --- Per-member working directory setup ---
  config$model_settings$ncore <- min(
    c(config$model_settings$ncore, future::availableCores())
  )
  # Preserve glm_restart.nc files placed by a zip restart; discard stale ones
  has_zip_restart <- !is.null(config$run_config$restart_file) &&
    !is.na(config$run_config$restart_file)
  purrr::walk(1:nmembers, function(m) {
    dir_path <- file.path(working_directory, m)
    rst_path <- file.path(dir_path, paste0("glm_restart_", m, ".nc"))
    rst_tmp <- if (has_zip_restart && file.exists(rst_path)) {
      tmp <- tempfile(fileext = ".nc")
      file.copy(rst_path, tmp)
      tmp
    } else NULL
    if (dir.exists(dir_path)) unlink(dir_path, recursive = TRUE)
    dir.create(dir_path, showWarnings = FALSE)
    if (!is.null(rst_tmp)) file.copy(rst_tmp, rst_path)
    FLAREr:::set_up_model(config,
                          ens_working_directory = dir_path,
                          state_names = states_config$state_names,
                          inflow_file_names = inflow_file_names,
                          outflow_file_names = outflow_file_names)
  })

  if(config$da_setup$assimilate_first_step){
    start_step <- 1
  }else{
    start_step <- 2
  }

  if(is.null(config$da_setup$use_inflation_factor)){
    config$da_setup$use_inflation_factor <- FALSE
  }

  if(is.null(config$da_setup$log_transform_wq_obs)){
    config$da_setup$log_transform_wq_obs <- FALSE
  }

  if(is.null(config$da_setup$log_transform_wq_zero_collapse)){
    config$da_setup$log_transform_wq_zero_collapse <- FALSE
  }

  if (is.null(config$da_setup$esmda_iterations)) {
    config$da_setup$esmda_iterations <- 4L
  }





  # Print GLM version
  #glm_v <- suppressWarnings(GLM3r::glm_version())
  #glm_v <- substr(glm_v[3], 35, 58)
  #message("Using GLM ", glm_v)
  #config$metadata$model_description$version <- substr(glm_v, 9, 16)

  ###START EnKF

  nml_glm <- FLAREr:::read_nml(file.path(config$file_path$configuration_directory, config$model_settings$base_GLM_nml))
  lake_max_depth <- nml_glm$morphometry$H[length(nml_glm$morphometry$H)] - nml_glm$morphometry$H[1]

  glm_restart_staged <- list()

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
        future::plan("future::sequential")
      }else{
        future::plan("future::multisession", workers = config$model_settings$ncore)
      }

      orgin <- getwd()

      out <- furrr::future_map(1:nmembers, function(m) {

        if(!config$uncertainty$weather & i >= (hist_days + 1)){
          curr_met_file <- met_file_names[met_index[1]]
        }else{
          curr_met_file <- met_file_names[met_index[m]]
        }

        curr_pars_ens <- FLAREr:::propose_parameters(i, m,
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

        out <-  FLAREr:::run_model(i,
                                   m,
                                   curr_start,
                                   curr_stop,
                                   par_names,
                                   curr_pars_ens = curr_pars_ens,
                                   ens_working_directory = file.path(working_directory, m),
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
                                   nstates,
                                   state_names = states_config$state_names,
                                   include_wq = config$include_wq,
                                   max_layers = config$model_settings$max_model_layers,
                                   states_heights_start = states_height[i-1, , ,m],
                                   glm_path = config$model_settings$glm_path,
                                   use_glm_restart = i > start_step
        )

      }, .options = furrr::furrr_options(seed = TRUE))

      setwd(orgin)

      # Capture GLM restart files for this timestep (keyed by date string)
      date_label <- format(as.Date(full_time[i]), "%Y-%m-%d")
      glm_restart_staged[[date_label]] <- list()
      for(m in seq_len(nmembers)) {
        rst_name <- paste0("glm_restart_", m, ".nc")
        rst_src <- file.path(working_directory, m, rst_name)
        if(file.exists(rst_src)) {
          glm_restart_staged[[date_label]][[rst_name]] <- readBin(
            rst_src, "raw", file.info(rst_src)$size
          )
        }
      }

      # Loop through output and assign to matrix
      for(m in 1:nmembers) {
        states_height[i, , , m] <- out[[m]]$x_star_end
        lake_depth[i ,m ] <- out[[m]]$lake_depth_end
        snow_ice_thickness[,i ,m] <- out[[m]]$snow_ice_thickness_end
        curr_pars[, m] <- out[[m]]$curr_pars_ens

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

        if(config$da_setup$add_random_noise == 2) {

          lake_depth[i,m] <- rnorm(1, lake_depth[i,m], states_non_vertical$depth_sd)
          with_noise <- FLAREr:::add_process_noise(states_height_ens = states_height[i, , , m],
                                                   model_sd = model_sd,
                                                   model_internal_heights_ens = model_internal_heights[i, ,m],
                                                   lake_depth_ens = lake_depth[i,m],
                                                   modeled_depths = config$model_settings$modeled_depths,
                                                   vert_decorr_length = states_config$vert_decorr_length,
                                                   include_uncertainty = include_process_uncertainty)
          states_depth_w_noise[, ,m] <- with_noise$states_depth_ens
          states_height[i, , , m] <- with_noise$states_height_ens
        }

      } # END ENSEMBLE LOOP

      if(config$da_setup$add_random_noise == 1) {
        state_matrix <- matrix(NA, nrow = nmembers, ncol = length(c(states_depth_wo_noise[, ,1])))
        for(m in 1:nmembers) {
          curr_states <- states_depth_wo_noise[, ,m ]
          for(s in 2:nstates){
            curr_states[s,which(curr_states[s, ] <= 0)] <- runif(length(which(curr_states[s, ] <= 0)), 0, 0.0001)
          }

          state_matrix[m , ] = c(t(curr_states[, ]))
        }

        means <- apply(state_matrix, 2, mean)
        sds <- apply(state_matrix, 2, sd)

        zero_sds <- which(sds == 0)
        for(bval in zero_sds){
          if(means[bval] > 0){
            state_matrix[ ,bval] <- rnorm(length(state_matrix[, bval]), means[bval], sd = 0.0001)
          }else{
            state_matrix[ ,bval] <- runif(length(state_matrix[, bval]), 0, 0.0001)
          }
        }



        state_cor <- cor(state_matrix)
        state_sd <- c(t(model_sd))

        state_cov <- diag(state_sd) %*% state_cor %*% t(diag(state_sd))


        #state_cov <- FLAREr:::localization(state_cov, nstates,
        #                                   config$model_settings$modeled_depths,
        #                                   config$da_setup$localization_distance, num_single_states = 0)

        for(m in 1:nmembers) {
          states_depth_w_noise[, ,m] <- t(matrix(mvtnorm::rmvnorm(1,
                                                                  mean = c(t(states_depth_wo_noise[, ,m ])),
                                                                  state_cov),
                                                 ncol = nstates))
          for(s in 2:nstates){
            neg <- which(states_depth_w_noise[s, , m] < 0)
            states_depth_w_noise[s, neg, m] <- -states_depth_w_noise[s, neg, m]
            still_neg <- which(states_depth_w_noise[s, , m] < 0)
            states_depth_w_noise[s, still_neg, m] <- 0.0
          }

          lake_depth[i, m] <- rnorm(1, lake_depth[i, ], states_non_vertical$depth_sd)
        }


      }else if(config$da_setup$add_random_noise == 0){
        states_depth_w_noise <- states_depth_wo_noise
      }

      ### SETTING OBSERVATIONS FOR POTENTAIL DATA ASSIMILATION

      if(dim(obs)[1] > 1){
        obs_count <- length(which(!is.na(c(aperm(obs[,i , ], perm = c(2,1))))))
      }else{
        obs_count <- length(which(!is.na(c(obs[1,i , ]))))
      }

      if(i > 1){
        #DON"T USE SECCHI ON DAY 1 BECAUSE THE DIAGONOSTIC OF LIGHT EXTINCTION
        #IS NOT IN THE RESTART FILE
        if(!is.null(obs_non_vertical$obs_secchi$obs)){
          if(!is.na(obs_non_vertical$obs_secchi$obs[i])){
            obs_count <- obs_count + 1
          }
        }
      }

      if(!is.null(obs_non_vertical$obs_depth)){
        if(!is.na(obs_non_vertical$obs_depth$obs[i])){
          obs_count <- obs_count + 1
        }
      }

      #if no observations at a time step then just propagate model uncertainty

      if(config$da_setup$use_inflation_factor){

        if(config$da_setup$add_random_noise == 1){
          states_depth_wo_noise <- states_depth_w_noise
        }

        if(config$da_setup$inflation_only_at_da & (obs_count == 0 | config$da_setup$da_method == "none" | !config$da_setup$use_obs_constraint)){
          curr_inflation <- 1.0
          curr_par_inflation <- 1.0
        }else{
          curr_inflation <- inflation[i-1]
          curr_par_inflation <- pars_config$inflation
        }

        ens_mean <- mean(lake_depth[i, ], na.rm = TRUE)
        lake_depth[i, ] <- sqrt(curr_inflation) * (lake_depth[i, ]  - ens_mean) + ens_mean

        for(s in 1:nstates){
          ens_mean <- apply(states_depth_wo_noise[s, , ], 1, mean, na.rm = TRUE)
          for(m in 1:nmembers){
            states_depth_w_noise[s, , m] <- sqrt(curr_inflation) * (states_depth_wo_noise[s, , m] - ens_mean) + ens_mean
            if(s > 1){
              index <- which(states_depth_w_noise[s, , m] < 0.0)
              states_depth_w_noise[s, index, m] <- -states_depth_w_noise[s, index, m]
              still_neg <- which(states_depth_w_noise[s, , m] < 0.0)
              states_depth_w_noise[s, still_neg, m] <- 0.0
            }
            non_na_heights_index <- which(!is.na(model_internal_heights[i, ,m]))
            states_height[i,s,non_na_heights_index,m] <- approx(lake_depth[i ,m ] - config$model_settings$modeled_depths,
                                                                states_depth_w_noise[s, , m ],
                                                                model_internal_heights[i, non_na_heights_index , m],
                                                                rule = 2)$y
          }
        }

        if(length(config$output_settings$diagnostics_names) > 0){
          ens_mean <- apply(diagnostics[d,i , ,], 1, mean)
          for(d in 1:dim(diagnostics)[1]){
            diagnostics[d, i, , ] <- sqrt(curr_inflation) * (diagnostics[d,i , ,] - ens_mean) + ens_mean
          }
        }

        if(length(config$output_settings$diagnostics_daily$names) > 0){
          ens_mean <- mean(diagnostics_daily[d,i, ])
          for(d in 1:dim(diagnostics_daily)[1]){
            diagnostics_daily[d, i, ] <-sqrt(curr_inflation) * (diagnostics_daily[d,i, ] - ens_mean) + ens_mean
          }
        }
        if(npars > 0){
          pars_mean <- apply(curr_pars, 1, mean)
          pars_corr <- sqrt(curr_par_inflation) * (curr_pars - pars_mean) + pars_mean
        }
      }else{

        if(npars > 0){
          pars_corr <- curr_pars
          if(npars == 1){
            pars_corr <- matrix(pars_corr,nrow = length(pars_corr),ncol = 1)
          }
        }

      }

    }else{ #time step is 0.

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

      inflation[i] <- inflation[i-1]


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

      # One-step lag: state and parameter filters run separately within each cycle.
      # Parameters are stripped from x_matrix so the state filter has no
      # parameter-state cross-covariance; a dedicated parameter EnKF runs
      # afterward using the forecast predicted observations.
      use_one_step_lag <- isTRUE(config$da_setup$use_one_step_lag) && npars > 0

      x_matrix <- apply(aperm(states_depth_w_noise[,1:ndepths_modeled,], perm = c(2,1,3)), 3, rbind)

      # Add depth to the x_matrix if in observations
      if(!is.null(obs_non_vertical$obs_depth)){
        x_matrix <- rbind(x_matrix, lake_depth[i, ])
      }

      # Add secchi depth to the x_matrix if in observations
      if(length(config$output_settings$diagnostics_names) > 0 & i > 1){
        modeled_secchi <- 1.7 / diagnostics[1, i, which.min(abs(config$model_settings$modeled_depths-1.0)), ]
        if(!is.null(obs_non_vertical$obs_secchi)){
          x_matrix <- rbind(x_matrix, modeled_secchi)
        }
      }

      # Capture state-only forecast before appending parameters; the parameter
      # filter needs these to compute its predicted observations.
      if(use_one_step_lag){
        x_forecast_states <- x_matrix
      }

      if(npars > 0 && !use_one_step_lag){
        x_matrix <- rbind(x_matrix, curr_pars)
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
      if(!is.null(obs_non_vertical$obs_depth)){
        depth_index <- 1
        if(!is.na(obs_non_vertical$obs_depth$obs[i])){
          zt <- c(zt, obs_non_vertical$obs_depth$obs[i])
          depth_obs <- obs_non_vertical$obs_depth$obs[i]
          depth_sd <- obs_non_vertical$obs_depth$sd
        }
      }else{
        depth_obs <- NA
        depth_sd <- NA
      }

      secchi_index <- 0
      if(i > 1){
        if(!is.null(obs_non_vertical$obs_secchi)){
          secchi_index <- 1
          if(!is.na(obs_non_vertical$obs_secchi$obs[i])){
            if(!is.na(obs_non_vertical$obs_secchi$obs[i])){
              zt <- c(zt, obs_non_vertical$obs_secchi$obs[i])
            }
          }
        }
      }

      #Assign which states have obs in the time step
      # For one_step_lag the parameter columns are absent from h; use npars_in_h
      # as a drop-in replacement so the depth/secchi offset expressions below
      # remain correct in both modes.
      npars_in_h <- if(use_one_step_lag) 0L else npars
      h <- matrix(0, nrow = vertical_obs * ndepths_modeled + depth_index + secchi_index,
                     ncol = nstates * ndepths_modeled + depth_index + secchi_index + npars_in_h)

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

      if(!is.null(obs_non_vertical$obs_depth) & depth_index > 0){
        if(!is.na(obs_non_vertical$obs_depth$obs[i])){
          h[dim(h)[1] - npars_in_h, dim(h)[2] - npars_in_h] <- 1
        }
      }

      if(!is.null(obs_non_vertical$obs_secchi)){
        if(!is.na(obs_non_vertical$obs_secchi$obs[i])){
          h[dim(h)[1] - depth_index - npars_in_h, dim(h)[2] - depth_index - npars_in_h] <- 1
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
          psi[index] <- obs_sd[k]
        }
      }

      if(depth_index > 0){
        psi[vertical_obs * ndepths_modeled + depth_index] <- obs_non_vertical$obs_depth$depth_sd
      }


      if(secchi_index > 0){
        psi[vertical_obs * ndepths_modeled + depth_index + secchi_index] <- obs_non_vertical$obs_secchi$secchi_sd
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

        updates <- FLAREr:::run_enkf(x_matrix,
                                     h,
                                     pars_corr,
                                     zt,
                                     psi,
                                     z_index,
                                     states_depth_start = states_depth[i, , , ],
                                     states_height_start = states_height[i, , ,],
                                     model_internal_heights_start = model_internal_heights[i, , ],
                                     lake_depth_start = lake_depth[i, ],
                                     log_particle_weights_start = log_particle_weights[i-1, ],
                                     snow_ice_thickness_start =  snow_ice_thickness[ ,i, ],
                                     diagnostics_start,
                                     diagnostics_daily_start,
                                     pars_config,
                                     config,
                                     depth_index,
                                     secchi_index,
                                     depth_obs,
                                     depth_sd,
                                     par_fit_method,
                                     inflation_start = inflation[i-1],
                                     lake_max_depth = lake_max_depth)

      }else if(da_method == "pf"){

        updates <- FLAREr:::run_particle_filter(x_matrix,
                                                h,
                                                pars_corr,
                                                zt,
                                                psi,
                                                z_index,
                                                states_depth_start = states_depth_w_noise,
                                                states_height_start = states_height[i, , ,],
                                                model_internal_heights_start = model_internal_heights[i, , ],
                                                lake_depth_start = lake_depth[i, ],
                                                log_particle_weights_start = log_particle_weights[i-1, ],
                                                snow_ice_thickness_start =  snow_ice_thickness[ ,i, ],
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
                                                obs_config,
                                                inflation_start = inflation[i-1])

      }else if(da_method == "etkf"){

        updates <- FLAREr:::run_etkf(x_matrix,
                                     h,
                                     pars_corr,
                                     zt,
                                     psi,
                                     z_index,
                                     states_depth_start = states_depth[i, , , ],
                                     states_height_start = states_height[i, , ,],
                                     model_internal_heights_start = model_internal_heights[i, , ],
                                     lake_depth_start = lake_depth[i, ],
                                     log_particle_weights_start = log_particle_weights[i-1, ],
                                     snow_ice_thickness_start =  snow_ice_thickness[ ,i, ],
                                     diagnostics_start,
                                     diagnostics_daily_start,
                                     pars_config,
                                     config,
                                     depth_index,
                                     secchi_index,
                                     depth_obs,
                                     depth_sd,
                                     par_fit_method,
                                     inflation_start = inflation[i-1],
                                     lake_max_depth = lake_max_depth)

      }else if(da_method == "esmda"){

        updates <- FLAREr:::run_esmda(x_matrix,
                                      h,
                                      pars_corr,
                                      zt,
                                      psi,
                                      z_index,
                                      states_depth_start = states_depth[i, , , ],
                                      states_height_start = states_height[i, , ,],
                                      model_internal_heights_start = model_internal_heights[i, , ],
                                      lake_depth_start = lake_depth[i, ],
                                      log_particle_weights_start = log_particle_weights[i-1, ],
                                      snow_ice_thickness_start =  snow_ice_thickness[ ,i, ],
                                      diagnostics_start,
                                      diagnostics_daily_start,
                                      pars_config,
                                      config,
                                      depth_index,
                                      secchi_index,
                                      depth_obs,
                                      depth_sd,
                                      par_fit_method,
                                      inflation_start = inflation[i-1],
                                      lake_max_depth = lake_max_depth)

      }else if(da_method == "letkf"){

        updates <- FLAREr:::run_letkf(x_matrix,
                                      h,
                                      pars_corr,
                                      zt,
                                      psi,
                                      z_index,
                                      states_depth_start = states_depth[i, , , ],
                                      states_height_start = states_height[i, , ,],
                                      model_internal_heights_start = model_internal_heights[i, , ],
                                      lake_depth_start = lake_depth[i, ],
                                      log_particle_weights_start = log_particle_weights[i-1, ],
                                      snow_ice_thickness_start =  snow_ice_thickness[ ,i, ],
                                      diagnostics_start,
                                      diagnostics_daily_start,
                                      pars_config,
                                      config,
                                      depth_index,
                                      secchi_index,
                                      depth_obs,
                                      depth_sd,
                                      par_fit_method,
                                      inflation_start = inflation[i-1],
                                      lake_max_depth = lake_max_depth)

      }else{
        stop("da_method not supported; select enkf, etkf, esmda, letkf, or pf or none")
      }

      #Update states and parameters
      if(npars > 0){
        if(use_one_step_lag && length(z_index) > 0 && da_method != "pf"){
          # Parameter filter: Kalman update using forecast predicted observations.
          # The state filter ran without parameters, so x_forecast_states holds
          # the pre-DA state ensemble; h (already row-subsetted by z_index) maps
          # those states to the active observations.
          predicted_obs_forecast <- h %*% x_forecast_states
          pars[i, , ] <- FLAREr:::update_parameters_enkf(
            pars          = pars_corr,
            predicted_obs = predicted_obs_forecast,
            zt            = zt,
            psi           = psi,
            z_index       = z_index,
            pars_config   = pars_config
          )
        } else {
          pars[i, , ] <- updates$pars_updated
        }
      }
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

      inflation[i] <- updates$inflation_update

      for(s in 1:nstates){
        for(m in 1:nmembers){
              depth_index <- which(config$model_settings$modeled_depths > lake_depth[i, m])
              states_depth[i, s, depth_index, m ] <- NA
              non_na_heights <- which(!is.na(model_internal_heights[i, ,m]))
              glm_depths <- lake_depth[i, m] - model_internal_heights[i, non_na_heights ,m]
              states_depth[i,s, , m] <- approx(glm_depths, states_height[i, s, non_na_heights, m], config$model_settings$modeled_depths, rule = 2)$y
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
              snow_ice_thickness = snow_ice_thickness,
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
              log_particle_weights = log_particle_weights,
              inflation = inflation,
              glm_restart_staged = glm_restart_staged))
}
