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
    non_na_heights <- which(!is.na(aux_states_init$model_internal_heights[, m]))
    glm_depths <- aux_states_init$lake_depth[m] -
      aux_states_init$model_internal_heights[non_na_heights, m]
    for (s in 1:nstates) {
      states_height[1, s, , m] <- states_init[s, , m]
      states_depth[1, s, , m] <- approx(
        glm_depths, states_height[1, s, non_na_heights, m],
        config$model_settings$modeled_depths,
        rule = 2
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
      dim = c(
        length(config$output_settings$diagnostics_names),
        nsteps, ndepths_modeled, nmembers
      )
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
    NA,
    dim = c(nsteps, config$model_settings$max_model_layers, nmembers)
  )
  lake_depth <- array(NA, dim = c(nsteps, nmembers))
  snow_ice_thickness <- array(NA, dim = c(3, nsteps, nmembers))
  log_particle_weights <- array(NA, dim = c(nsteps, nmembers))
  inflation <- rep(NA, nsteps)

  model_internal_heights[1, , ] <- aux_states_init$model_internal_heights
  lake_depth[1, ] <- aux_states_init$lake_depth
  snow_ice_thickness[, 1, ] <- aux_states_init$snow_ice_thickness
  log_particle_weights[1, ] <- aux_states_init$log_particle_weights
  inflation[1] <- aux_states_init$inflation

  if (!is.null(aux_states_init$diagnostics) && is.array(diagnostics)) {
    diagnostics[, 1, , ] <- aux_states_init$diagnostics
  }
  if (!is.null(aux_states_init$diagnostics_daily) && is.array(diagnostics_daily)) {
    diagnostics_daily[, 1, ] <- aux_states_init$diagnostics_daily
  }

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
#' @param da_method string; data assimilation method (one of "enkf", "etkf",
#'   "esmda", "letkf", "pf", or "none"; Default = "enkf"). NOTE: only "enkf" has
#'   been extensively tested. All other methods ("etkf", "esmda", "letkf", "pf")
#'   are experimental and should be used with caution.
#' @param par_fit_method string; method for adding noise to parameters during calibration
#' @param obs_non_vertical named list of non-vertical observations (from create_obs_non_vertical)
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
                            non_vertical_noise_config = NULL) {

  # Only the Ensemble Kalman Filter ("enkf") has been extensively tested.
  # Warn users that the remaining data assimilation methods are experimental.
  if (da_method %in% c("etkf", "esmda", "letkf", "pf")) {
    warning(paste0("da_method = '", da_method, "' is experimental and has not ",
                   "been extensively tested. Only 'enkf' is recommended for ",
                   "production use; use other methods with caution."),
            call. = FALSE)
  }

  # States beyond temp and salinity (index > 2) are water-quality variables.
  if (length(states_config$state_names) > 2) {
    config$include_wq <- TRUE
  } else {
    config$include_wq <- FALSE
  }

  # Guard for older obs_config tables that predate the multi_depth column.
  if (!("multi_depth" %in% names(obs_config))) {
    obs_config <- obs_config |> dplyr::mutate(multi_depth = 1)
  }

  nstates <- dim(states_init)[1]
  ndepths_modeled <- length(config$model_settings$modeled_depths)
  nmembers <- dim(states_init)[3]
  model <- config$model_settings$model
  if (!is.null(pars_config)) {
    if ("model" %in% names(pars_config)) {
      pars_config <- pars_config[pars_config$model == model, ]
    }
    npars <- nrow(pars_config)
    par_names <- pars_config$par_names
    par_file <- pars_config$par_file
  } else {
    npars <- 0
    par_names <- NA
    par_file <- NA
  }

  start_datetime <- lubridate::as_datetime(config$run_config$start_datetime)
  if (is.na(config$run_config$forecast_start_datetime)) {
    # No forecast horizon: treat the full run as hindcast only.
    end_datetime <- lubridate::as_datetime(config$run_config$end_datetime)
    forecast_start_datetime <- end_datetime
  } else {
    forecast_start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime)
    end_datetime <- forecast_start_datetime + lubridate::days(config$run_config$forecast_horizon)
  }

  hist_days <- as.numeric(forecast_start_datetime - start_datetime)
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
  states_depth <- arrs$states_depth
  states_height <- arrs$states_height
  pars <- arrs$pars
  num_wq_vars <- arrs$num_wq_vars
  diagnostics <- arrs$diagnostics
  diagnostics_daily <- arrs$diagnostics_daily
  model_internal_heights <- arrs$model_internal_heights
  lake_depth <- arrs$lake_depth
  snow_ice_thickness <- arrs$snow_ice_thickness
  log_particle_weights <- arrs$log_particle_weights
  inflation <- arrs$inflation

  output_vars <- states_config$state_names

  # Count phytoplankton functional groups; exclude internal-pool suffixes
  # (_IP = intracellular phosphorus, _IN = intracellular nitrogen).
  num_phytos <- length(which(
    stringr::str_detect(states_config$state_names, "PHY_") &
      !stringr::str_detect(states_config$state_names, "_IP") &
      !stringr::str_detect(states_config$state_names, "_IN")
  ))

  if (!is.null(inflow_file_names)) {
    inflow_file_names <- as.matrix(inflow_file_names)
    outflow_file_names <- as.matrix(outflow_file_names)
  } else {
    inflow_file_names <- NULL
    outflow_file_names <- NULL
  }

  # Build a persistent worker cluster so stable globals are exported once and
  # only tiny per-member per-step slices are transferred on each iteration.
  ncore <- min(config$model_settings$ncore, parallel::detectCores(logical = FALSE))
  if (ncore > 1) {
    cl <- parallel::makeCluster(ncore)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterSetRNGStream(cl)
  } else {
    cl <- NULL
  }
  .par_apply <- if (!is.null(cl)) {
    function(x, fn) parallel::parLapply(cl, x, fn)
  } else {
    lapply
  }
  # Stash any restart files that were unpacked from the input zip before wiping
  # each member directory; they must survive the directory recreation below.
  has_zip_restart <- !is.null(config$run_config$restart_file) &&
    !is.na(config$run_config$restart_file)

  # Export all stable (non-step-varying) objects to workers once so they are
  # not re-serialized on every parLapply call inside the DA loop.
  # Only variables actually referenced inside worker function bodies are listed;
  # per-step data (met files, state slices, pars) are passed via the input list.
  if (!is.null(cl)) {
    parallel::clusterCall(cl, function() requireNamespace("FLAREr", quietly = TRUE))
    parallel::clusterExport(cl, varlist = c(
      "config", "working_directory", "has_zip_restart",
      "par_names", "par_file", "num_phytos",
      "full_time", "hist_days", "ndepths_modeled",
      "output_vars", "num_wq_vars", "nstates", "npars",
      "states_config"
    ), envir = environment())
  }

  # Directory setup is sequential — filesystem ops are order-sensitive and fast.
  lapply(seq_len(nmembers), function(m) {
    dir_path <- file.path(working_directory, m)
    rst_path <- file.path(dir_path, paste0("glm_restart_", m, ".nc"))
    rst_tmp <- if (has_zip_restart && file.exists(rst_path)) {
      tmp <- tempfile(fileext = ".nc")
      file.copy(rst_path, tmp)
      tmp
    } else {
      NULL
    }
    if (dir.exists(dir_path)) unlink(dir_path, recursive = TRUE)
    dir.create(dir_path, showWarnings = FALSE)
    if (!is.null(rst_tmp)) file.copy(rst_tmp, rst_path)
    FLAREr:::set_up_model(config,
      ens_working_directory = dir_path,
      state_names = states_config$state_names,
      inflow_file_names = inflow_file_names,
      outflow_file_names = outflow_file_names
    )
  })

  # When assimilate_first_step is TRUE, observations at t=1 update the
  # initial conditions before the first model run.
  if (config$da_setup$assimilate_first_step) {
    start_step <- 1
  } else {
    start_step <- 2
  }

  # Defensive defaults for fields absent in older config files.
  if (is.null(config$da_setup$use_inflation_factor)) {
    config$da_setup$use_inflation_factor <- FALSE
  }

  if (is.null(config$da_setup$log_transform_wq_obs)) {
    config$da_setup$log_transform_wq_obs <- FALSE
  }

  if (is.null(config$da_setup$log_transform_wq_zero_collapse)) {
    config$da_setup$log_transform_wq_zero_collapse <- FALSE
  }

  if (is.null(config$da_setup$esmda_iterations)) {
    config$da_setup$esmda_iterations <- 4L
  }


  ### START EnKF

  nml_glm <- FLAREr:::read_nml(file.path(config$file_path$configuration_directory, config$model_settings$base_GLM_nml))
  lake_max_depth <- nml_glm$morphometry$H[length(nml_glm$morphometry$H)] - nml_glm$morphometry$H[1]

  glm_restart_staged <- list()

  restart_save_timesteps <- config$output_settings$restart_save_timesteps
  if (is.null(restart_save_timesteps)) restart_save_timesteps <- 0L
  if (identical(restart_save_timesteps, "all") ||
        (length(restart_save_timesteps) == 1 && as.character(restart_save_timesteps) == "all")) {
    keep_restart_dates <- NULL
  } else {
    keep_restart_dates <- format(
      as.Date(forecast_start_datetime) + as.integer(restart_save_timesteps),
      "%Y-%m-%d"
    )
  }

  # Per-member parsed NML cache. Populated after each run_model call so the
  # next step skips the disk read and only writes the modified NML.
  member_glm_nml <- vector("list", nmembers)
  member_aed_nml <- vector("list", nmembers)

  da_diag_steps <- list()

  for (i in start_step:nsteps) {
    if (i > 1) {
      curr_start <- strftime(full_time[i - 1],
        format = "%Y-%m-%d %H:%M",
        tz = "UTC"
      )
    } else {
      curr_start <- "Restart"
    }
    curr_stop <- strftime(full_time[i],
      format = "%Y-%m-%d %H:%M",
      tz = "UTC"
    )

    message(paste0(
      "Running time step ", i - 1, "/", (nsteps - 1), " : ",
      curr_start, " - ",
      curr_stop, " [", Sys.time(), "]"
    ))

    # Cycle file indices round-robin across ensemble members; when there are
    # fewer scenario files than members, members share files evenly.
    met_index <- rep(seq_along(met_file_names), times = nmembers)
    if (!is.null(ncol(inflow_file_names))) {
      inflow_outflow_index <- rep(seq_len(nrow(inflow_file_names)), times = nmembers)
    } else {
      inflow_outflow_index <- NULL
    }

    # Scratch arrays reset each step: _wo_noise = raw GLM output,
    # _w_noise = after process noise is added.
    states_depth_wo_noise <- array(NA, dim = c(nstates, ndepths_modeled, nmembers))
    states_depth_w_noise <- array(NA, dim = c(nstates, ndepths_modeled, nmembers))
    curr_pars <- array(NA, dim = c(npars, nmembers))
    pars_corr <- if (isTRUE(npars > 0)) pars[i, , ] else NULL

    # Run GLM one step from the initial conditions to populate the diagnostics
    # array at i==1.  Only diagnostics_end is kept; x_star_end is discarded so
    # the initial conditions are preserved intact for the DA step below.  Any
    # restart files written here are overwritten with DA-updated states at i==2
    # by update_glm_restart_file(), so they do not corrupt the main forecast.
    # Note: diagnostics are extracted from the end of the full_time[1] ->
    # full_time[2] run, so they reflect the model state at approximately
    # full_time[2] rather than exactly full_time[1].  For slowly-varying
    # diagnostics such as light extinction this approximation is negligible.
    needs_diag_spinup <-
      (length(config$output_settings$diagnostics_names) > 0 &&
       is.null(aux_states_init$diagnostics)) ||
      (length(config$output_settings$diagnostics_daily$names) > 0 &&
       is.null(aux_states_init$diagnostics_daily))

    if (i == 1 && nsteps > 1 && needs_diag_spinup) {
      spinup_start <- strftime(full_time[1], format = "%Y-%m-%d %H:%M", tz = "UTC")
      spinup_stop <- strftime(full_time[2], format = "%Y-%m-%d %H:%M", tz = "UTC")

      # Build per-member input lists in the main process (propose_parameters,
      # met-file selection, and state-slice extraction are all fast/sequential).
      # Workers receive only these compact structs plus the stable globals
      # already loaded via clusterExport.
      spinup_inputs <- lapply(seq_len(nmembers), function(m) {
        curr_met_file <- if (!config$uncertainty$weather) {
          met_file_names[met_index[1]]
        } else {
          met_file_names[met_index[m]]
        }
        if (!is.null(ncol(inflow_file_names))) {
          inflow_file_name  <- inflow_file_names[inflow_outflow_index[m], ]
          outflow_file_name <- outflow_file_names[inflow_outflow_index[m], ]
        } else {
          inflow_file_name  <- NULL
          outflow_file_name <- NULL
        }
        curr_pars_ens <- FLAREr:::propose_parameters(
          i                   = 1L,
          m                   = m,
          pars                = pars,
          pars_config         = pars_config,
          npars               = npars,
          par_fit_method      = par_fit_method,
          da_method           = da_method,
          hist_days           = hist_days,
          include_uncertainty = config$uncertainty$parameter
        )
        list(
          m                        = m,
          spinup_start             = spinup_start,
          spinup_stop              = spinup_stop,
          curr_pars_ens            = curr_pars_ens,
          curr_met_file            = curr_met_file,
          inflow_file_name         = inflow_file_name,
          outflow_file_name        = outflow_file_name,
          glm_heights_start        = model_internal_heights[1, , m],
          lake_depth_start         = lake_depth[1, m],
          snow_ice_thickness_start = snow_ice_thickness[, 1, m],
          states_heights_start     = states_height[1, , , m],
          glm_nml                  = member_glm_nml[[m]],
          aed_nml                  = member_aed_nml[[m]]
        )
      })
      diag_init <- .par_apply(spinup_inputs, function(sl) {
        FLAREr:::run_model(
          i                        = 1L,
          m                        = sl$m,
          curr_start               = sl$spinup_start,
          curr_stop                = sl$spinup_stop,
          par_names                = par_names,
          curr_pars_ens            = sl$curr_pars_ens,
          ens_working_directory    = file.path(working_directory, sl$m),
          par_nml                  = par_file,
          num_phytos               = num_phytos,
          glm_heights_start        = sl$glm_heights_start,
          lake_depth_start         = sl$lake_depth_start,
          full_time                = full_time,
          hist_days                = hist_days,
          modeled_depths           = config$model_settings$modeled_depths,
          ndepths_modeled          = ndepths_modeled,
          curr_met_file            = sl$curr_met_file,
          inflow_file_name         = sl$inflow_file_name,
          outflow_file_name        = sl$outflow_file_name,
          glm_output_vars          = output_vars,
          diagnostics_names        = config$output_settings$diagnostics_names,
          diagnostics_daily_config = config$output_settings$diagnostics_daily,
          npars                    = npars,
          num_wq_vars              = num_wq_vars,
          snow_ice_thickness_start = sl$snow_ice_thickness_start,
          nstates                  = nstates,
          state_names              = states_config$state_names,
          include_wq               = config$include_wq,
          max_layers               = config$model_settings$max_model_layers,
          states_heights_start     = sl$states_heights_start,
          glm_path                 = config$model_settings$glm_path,
          use_glm_restart          = has_zip_restart,
          glm_nml                  = sl$glm_nml,
          aed_nml                  = sl$aed_nml
        )
      })

      for (m in 1:nmembers) {
        num_out_heights <- length(diag_init[[m]]$model_internal_heights)
        non_na_idx <- seq_len(num_out_heights)
        glm_depths_spin <- diag_init[[m]]$lake_depth_end -
          diag_init[[m]]$model_internal_heights[non_na_idx]

        if (length(config$output_settings$diagnostics_names) > 0 &&
              is.null(aux_states_init$diagnostics)) {
          for (d in seq_len(dim(diagnostics)[1])) {
            diagnostics[d, 1, , m] <- approx(
              glm_depths_spin,
              diag_init[[m]]$diagnostics_end[d, non_na_idx],
              config$model_settings$modeled_depths,
              rule = 2
            )$y
          }
        }

        if (length(config$output_settings$diagnostics_daily$names) > 0 &&
              is.null(aux_states_init$diagnostics_daily)) {
          diagnostics_daily[, 1, m] <- diag_init[[m]]$diagnostics_daily_end
        }
      }
      # Seed per-member NML cache from spinup so the first main-loop iteration
      # skips the disk read.
      for (m in seq_len(nmembers)) {
        member_glm_nml[m] <- list(diag_init[[m]]$glm_nml)
        member_aed_nml[m] <- list(diag_init[[m]]$aed_nml)
      }
    }

    # At i==1 the initial conditions already populate the state arrays; skip the
    # main model run and go straight to (optional) data assimilation.
    if (i > 1) {
      # Build per-member input lists on the main process — propose_parameters,
      # met-file selection, and state-slice extraction are all fast/sequential.
      # Workers receive only these compact structs; stable globals come from the
      # clusterExport done at startup.
      .prev <- i - 1L
      step_inputs <- lapply(seq_len(nmembers), function(m) {
        # In forecast mode with weather uncertainty disabled, all members
        # share the deterministic (first) met file.
        curr_met_file <- if (!config$uncertainty$weather & i >= (hist_days + 1)) {
          met_file_names[met_index[1]]
        } else {
          met_file_names[met_index[m]]
        }
        # Mirror the met uncertainty logic for inflow/outflow scenarios.
        if (!is.null(ncol(inflow_file_names))) {
          if (!config$uncertainty$inflow & i > (hist_days + 1)) {
            inflow_file_name  <- inflow_file_names[inflow_outflow_index[1], ]
            outflow_file_name <- outflow_file_names[inflow_outflow_index[1], ]
          } else {
            inflow_file_name  <- inflow_file_names[inflow_outflow_index[m], ]
            outflow_file_name <- outflow_file_names[inflow_outflow_index[m], ]
          }
        } else {
          inflow_file_name  <- NULL
          outflow_file_name <- NULL
        }
        curr_pars_ens <- FLAREr:::propose_parameters(
          i, m, pars, pars_config, npars,
          par_fit_method, da_method, hist_days,
          include_uncertainty = config$uncertainty$parameter
        )
        list(
          m                        = m,
          i                        = i,
          curr_start               = curr_start,
          curr_stop                = curr_stop,
          curr_pars_ens            = curr_pars_ens,
          curr_met_file            = curr_met_file,
          inflow_file_name         = inflow_file_name,
          outflow_file_name        = outflow_file_name,
          glm_heights_start        = model_internal_heights[.prev, , m],
          lake_depth_start         = lake_depth[.prev, m],
          snow_ice_thickness_start = snow_ice_thickness[, .prev, m],
          states_heights_start     = states_height[.prev, , , m],
          use_glm_restart          = i > start_step || has_zip_restart,
          glm_nml                  = member_glm_nml[[m]],
          aed_nml                  = member_aed_nml[[m]]
        )
      })

      out <- .par_apply(step_inputs, function(sl) {
        FLAREr:::run_model(
          i                        = sl$i,
          m                        = sl$m,
          curr_start               = sl$curr_start,
          curr_stop                = sl$curr_stop,
          par_names                = par_names,
          curr_pars_ens            = sl$curr_pars_ens,
          ens_working_directory    = file.path(working_directory, sl$m),
          par_nml                  = par_file,
          num_phytos               = num_phytos,
          glm_heights_start        = sl$glm_heights_start,
          lake_depth_start         = sl$lake_depth_start,
          full_time                = full_time,
          hist_days                = hist_days,
          modeled_depths           = config$model_settings$modeled_depths,
          ndepths_modeled          = ndepths_modeled,
          curr_met_file            = sl$curr_met_file,
          inflow_file_name         = sl$inflow_file_name,
          outflow_file_name        = sl$outflow_file_name,
          glm_output_vars          = output_vars,
          diagnostics_names        = config$output_settings$diagnostics_names,
          diagnostics_daily_config = config$output_settings$diagnostics_daily,
          npars                    = npars,
          num_wq_vars              = num_wq_vars,
          snow_ice_thickness_start = sl$snow_ice_thickness_start,
          nstates                  = nstates,
          state_names              = states_config$state_names,
          include_wq               = config$include_wq,
          max_layers               = config$model_settings$max_model_layers,
          states_heights_start     = sl$states_heights_start,
          glm_path                 = config$model_settings$glm_path,
          use_glm_restart          = sl$use_glm_restart,
          glm_nml                  = sl$glm_nml,
          aed_nml                  = sl$aed_nml
        )
      })

      # Read restart NetCDF bytes into memory now; they are written to the
      # output zip at the end of the run, after the loop closes the files.
      date_label <- format(as.Date(full_time[i]), "%Y-%m-%d")
      if (is.null(keep_restart_dates) || date_label %in% keep_restart_dates) {
        glm_restart_staged[[date_label]] <- list()
        for (m in seq_len(nmembers)) {
          rst_name <- paste0("glm_restart_", m, ".nc")
          rst_src <- file.path(working_directory, m, rst_name)
          if (file.exists(rst_src)) {
            glm_restart_staged[[date_label]][[rst_name]] <- readBin(
              rst_src, "raw", file.info(rst_src)$size
            )
          }
        }
      }

      # Loop through output and assign to matrix
      for (m in 1:nmembers) {
        states_height[i, , , m] <- out[[m]]$x_star_end
        lake_depth[i, m] <- out[[m]]$lake_depth_end
        snow_ice_thickness[, i, m] <- out[[m]]$snow_ice_thickness_end
        curr_pars[, m] <- out[[m]]$curr_pars_ens

        num_out_heights <- length(out[[m]]$model_internal_heights)
        model_internal_heights[i, 1:num_out_heights, m] <- out[[m]]$model_internal_heights
        non_na_heights_index <- 1:num_out_heights

        # GLM reports state on height-from-bottom layers; convert to
        # depth-from-surface on the fixed modeled_depths grid via interpolation.
        glm_depths <- lake_depth[i, m] - model_internal_heights[i, non_na_heights_index, m]
        for (s in 1:nstates) {
          states_depth_wo_noise[s, , m] <- approx(glm_depths, states_height[i, s, non_na_heights_index, m], config$model_settings$modeled_depths, rule = 2)$y
        }

        if (length(config$output_settings$diagnostics_names) > 0) {
          for (d in seq_len(dim(diagnostics)[1])) {
            diagnostics[d, i, , m] <- approx(glm_depths, out[[m]]$diagnostics_end[d, non_na_heights_index], config$model_settings$modeled_depths, rule = 2)$y
          }
        }

        if (length(config$output_settings$diagnostics_daily$names) > 0) {
          diagnostics_daily[, i, m] <- out[[m]]$diagnostics_daily_end
        }

        if (config$uncertainty$process == FALSE && i > (hist_days + 1)) {
          include_process_uncertainty <- FALSE
        } else {
          include_process_uncertainty <- TRUE
        }

        if (config$da_setup$add_random_noise != 0) {
          nv_noise <- FLAREr:::apply_non_vertical_process_noise(
            non_vertical_noise_config = non_vertical_noise_config,
            lake_depth_m = lake_depth[i, m],
            diagnostics_slice = if (length(config$output_settings$diagnostics_names) > 0) {
              diagnostics[, i, , m]
            } else {
              NULL
            },
            diagnostics_daily_slice = if (length(config$output_settings$diagnostics_daily$names) > 0) {
              diagnostics_daily[, i, m]
            } else {
              NULL
            },
            config = config
          )
          lake_depth[i, m] <- nv_noise$lake_depth_m
          if (length(config$output_settings$diagnostics_names) > 0) {
            diagnostics[, i, , m] <- nv_noise$diagnostics_slice
          }
          if (length(config$output_settings$diagnostics_daily$names) > 0) {
            diagnostics_daily[, i, m] <- nv_noise$diagnostics_daily_slice
          }

          with_noise <- FLAREr:::add_process_noise(
            states_height_ens = states_height[i, , , m],
            model_sd = model_sd,
            model_internal_heights_ens = model_internal_heights[i, , m],
            lake_depth_ens = lake_depth[i, m],
            modeled_depths = config$model_settings$modeled_depths,
            vert_decorr_length = states_config$vert_decorr_length,
            include_uncertainty = include_process_uncertainty
          )
          states_depth_w_noise[, , m] <- with_noise$states_depth_ens
          states_height[i, , , m] <- with_noise$states_height_ens
        } else {
          states_depth_w_noise[, , m] <- states_depth_wo_noise[, , m]
        }
      } # END ENSEMBLE LOOP

      if (isTRUE(npars > 0)) pars_corr <- curr_pars

      # Update per-member NML cache so the next step can skip the disk read.
      for (m in seq_len(nmembers)) {
        member_glm_nml[m] <- list(out[[m]]$glm_nml)
        member_aed_nml[m] <- list(out[[m]]$aed_nml)
      }
    } # END MODEL RUN

    ### SETTING OBSERVATIONS FOR POTENTAIL DATA ASSIMILATION

    if (dim(obs)[1] > 1) {
      obs_count <- length(which(!is.na(c(aperm(obs[, i, ], perm = c(2, 1))))))
    } else {
      obs_count <- length(which(!is.na(c(obs[1, i, ]))))
    }

    for (nv_var in names(obs_non_vertical)) {
      if (!is.na(obs_non_vertical[[nv_var]]$obs[i])) {
        obs_count <- obs_count + 1
      }
    }

    if (obs_count == 0 || config$da_setup$da_method == "none" || !config$da_setup$use_obs_constraint) {
      if (i > (hist_days + 1)) {
        data_assimilation_flag[i] <- 0
        forecast_flag[i] <- 1
        da_qc_flag[i] <- 0
      } else if (i <= (hist_days + 1) && config$da_setup$use_obs_constraint) {
        data_assimilation_flag[i] <- 1
        forecast_flag[i] <- 0
        da_qc_flag[i] <- 1
      } else {
        data_assimilation_flag[i] <- 0
        forecast_flag[i] <- 0
        da_qc_flag[i] <- 0
      }

      states_depth[i, , , ] <- states_depth_w_noise

      log_particle_weights[i, ] <- log_particle_weights[i - 1, ]

      inflation[i] <- inflation[i - 1]


      if (npars > 0) pars[i, , ] <- pars_corr

      # At the history/forecast boundary, collapse ensemble spread to the
      # ensemble mean when initial-condition uncertainty is disabled.
      if (i == (hist_days + 1) && config$uncertainty$initial_condition == FALSE) {
        if (npars > 0) pars[i, , ] <- pars_corr
        for (s in 1:nstates) {
          for (k in 1:ndepths_modeled) {
            states_depth[i, s, k, ] <- mean(states_depth_wo_noise[s, k, ])
          }
        }
      }

      for (s in 1:nstates) {
        for (m in 1:nmembers) {
          depth_index <- which(config$model_settings$modeled_depths > lake_depth[i, m])
          states_depth[i, s, depth_index, m] <- NA
        }
      }

      if (length(config$output_settings$diagnostics_names) > 0) {
        for (d in seq_len(dim(diagnostics)[1])) {
          for (m in 1:nmembers) {
            depth_index <- which(config$model_settings$modeled_depths > lake_depth[i, m])
            diagnostics[d, i, depth_index, m] <- NA
          }
        }
      }
    } else {
      message("performing data assimilation")

      # One-step lag: state and parameter filters run separately within each cycle.
      # Parameters are stripped from x_matrix so the state filter has no
      # parameter-state cross-covariance; a dedicated parameter EnKF runs
      # afterward using the forecast predicted observations.
      use_one_step_lag <- isTRUE(config$da_setup$use_one_step_lag) && npars > 0

      x_matrix <- apply(aperm(states_depth_w_noise[, 1:ndepths_modeled, ], perm = c(2, 1, 3)), 3, rbind)

      # Append each non-vertical observation variable to the augmented state vector.
      # active_in_xmatrix tracks which variables were successfully extracted (diagnostic
      # variables return NULL at i==1 before the diagnostics array is populated).
      active_in_xmatrix <- character(0)
      for (nv_var in names(obs_non_vertical)) {
        modeled_val <- extract_modeled_non_vertical( # nolint: object_usage_linter.
          var_name      = nv_var,
          meta          = obs_non_vertical[[nv_var]],
          states_depth  = states_depth_w_noise,
          diagnostics   = diagnostics,
          lake_depth    = lake_depth[i, ],
          states_config = states_config,
          config        = config,
          time_index    = i
        )
        if (!is.null(modeled_val)) {
          ops <- get_non_vertical_operator(nv_var) # nolint: object_usage_linter.
          x_matrix <- rbind(x_matrix, ops$forward_fn(modeled_val))
          active_in_xmatrix <- c(active_in_xmatrix, nv_var)
        }
      }
      n_non_vertical <- length(active_in_xmatrix)

      # Capture state-only forecast before appending parameters; the parameter
      # filter needs these to compute its predicted observations.
      if (use_one_step_lag) {
        x_forecast_states <- x_matrix
      }

      if (npars > 0 && !use_one_step_lag) {
        x_matrix <- rbind(x_matrix, curr_pars)
      }

      data_assimilation_flag[i] <- 1
      forecast_flag[i] <- 0
      da_qc_flag[i] <- 0

      curr_obs <- obs[, i, ]

      vertical_obs <- length(which(obs_config$multi_depth == 1))

      if (dim(obs)[1] > 1) {
        zt <- c(aperm(curr_obs, perm = c(2, 1)))
      } else {
        zt <- curr_obs
      }

      zt <- zt[which(!is.na(zt))]

      for (nv_var in active_in_xmatrix) {
        obs_val <- obs_non_vertical[[nv_var]]$obs[i]
        if (!is.na(obs_val)) zt <- c(zt, obs_val)
      }

      # Build the linear observation operator H that maps the augmented state
      # vector [states (depth×state), non_vertical_1?, ..., non_vertical_n?, pars?]
      # to the observation vector.  Rows = observations, columns = state elements.
      # For one_step_lag the parameter columns are absent from H; npars_in_h
      # keeps the non-vertical column offsets correct in both modes.
      npars_in_h <- if (use_one_step_lag) 0L else npars
      h <- matrix(0,
        nrow = vertical_obs * ndepths_modeled + n_non_vertical,
        ncol = nstates * ndepths_modeled + n_non_vertical + npars_in_h
      )

      index <- 0
      for (k in 1:nstates) {
        for (j in 1:ndepths_modeled) {
          index <- index + 1
          if (!is.na(dplyr::first(states_config$states_to_obs[[k]]))) {
            for (jj in seq_along(states_config$states_to_obs[[k]])) {
              if (!is.na((obs[states_config$states_to_obs[[k]][jj], i, j]))) {
                states_to_obs_index <- states_config$states_to_obs[[k]][jj]
                index2 <- (states_to_obs_index - 1) * ndepths_modeled + j
                h[index2, index] <- states_config$states_to_obs_mapping[[k]][jj]
              }
            }
          }
        }
      }

      # Each non-vertical variable maps 1-to-1 to its own row/column in H.
      for (k in seq_along(active_in_xmatrix)) {
        nv_var <- active_in_xmatrix[k]
        obs_val <- obs_non_vertical[[nv_var]]$obs[i]
        if (!is.na(obs_val)) {
          h_row <- vertical_obs * ndepths_modeled + k
          h_col <- nstates * ndepths_modeled + k
          h[h_row, h_col] <- 1
        }
      }

      # Drop H rows that have no nonzero entries (no observation at this depth).
      z_index <- which(rowSums(h) > 0)

      h <- h[z_index, ]

      if (!is.matrix(h)) {
        h <- t(as.matrix(h))
      }

      psi <- rep(NA, vertical_obs * ndepths_modeled + n_non_vertical)
      index <- 0
      for (k in 1:vertical_obs) {
        for (j in 1:ndepths_modeled) {
          index <- index + 1
          psi[index] <- obs_sd[k]
        }
      }

      for (k in seq_along(active_in_xmatrix)) {
        psi[vertical_obs * ndepths_modeled + k] <- obs_non_vertical[[active_in_xmatrix[k]]]$sd
      }

      if (length(config$output_settings$diagnostics_names) > 0) {
        diagnostics_start <- diagnostics[, i, , ]
      } else {
        diagnostics <- NA
      }

      if (length(config$output_settings$diagnostics_daily$names) > 0) {
        diagnostics_daily_start <- diagnostics_daily[, i, ]
      } else {
        diagnostics_daily_start <- NA
      }

      # Pre-build obs labels for diagnostic output (no cost unless option is set)
      obs_diag_meta <- if (isTRUE(config$da_setup$save_da_diagnostics) &&
                            length(z_index) > 0L) {
        obs_config_vert <- obs_config[obs_config$multi_depth == 1, ]
        all_vars   <- c(rep(obs_config_vert$state_names_obs, each = ndepths_modeled),
                        active_in_xmatrix)
        all_depths <- c(rep(config$model_settings$modeled_depths,
                            times = nrow(obs_config_vert)),
                        rep(NA_real_, length(active_in_xmatrix)))
        list(variable = all_vars[z_index], depth = all_depths[z_index])
      } else {
        NULL
      }

      if (da_method == "enkf") {
        updates <- FLAREr:::run_enkf(x_matrix,
          h,
          pars_corr,
          zt,
          psi,
          z_index,
          states_depth_start = states_depth_w_noise,
          states_height_start = states_height[i, , , ],
          model_internal_heights_start = model_internal_heights[i, , ],
          lake_depth_start = lake_depth[i, ],
          log_particle_weights_start = log_particle_weights[i - 1, ],
          snow_ice_thickness_start = snow_ice_thickness[, i, ],
          diagnostics_start,
          diagnostics_daily_start,
          pars_config,
          config,
          obs_non_vertical,
          active_in_xmatrix,
          n_non_vertical,
          par_fit_method,
          inflation_start = inflation[i - 1],
          lake_max_depth = lake_max_depth,
          states_config = states_config,
          obs_diag_meta = obs_diag_meta
        )
      } else if (da_method == "pf") {
        updates <- FLAREr:::run_particle_filter(x_matrix,
          h,
          pars_corr,
          zt,
          psi,
          z_index,
          states_depth_start = states_depth_w_noise,
          states_height_start = states_height[i, , , ],
          model_internal_heights_start = model_internal_heights[i, , ],
          lake_depth_start = lake_depth[i, ],
          log_particle_weights_start = log_particle_weights[i - 1, ],
          snow_ice_thickness_start = snow_ice_thickness[, i, ],
          diagnostics_start = diagnostics_start,
          diagnostics_daily_start = diagnostics_daily_start,
          pars_config,
          config,
          obs_non_vertical,
          active_in_xmatrix,
          n_non_vertical,
          par_fit_method,
          vertical_obs,
          working_directory,
          obs_config,
          inflation_start = inflation[i - 1]
        )
      } else if (da_method == "etkf") {
        updates <- FLAREr:::run_etkf(x_matrix,
          h,
          pars_corr,
          zt,
          psi,
          z_index,
          states_depth_start = states_depth_w_noise,
          states_height_start = states_height[i, , , ],
          model_internal_heights_start = model_internal_heights[i, , ],
          lake_depth_start = lake_depth[i, ],
          log_particle_weights_start = log_particle_weights[i - 1, ],
          snow_ice_thickness_start = snow_ice_thickness[, i, ],
          diagnostics_start,
          diagnostics_daily_start,
          pars_config,
          config,
          obs_non_vertical,
          active_in_xmatrix,
          n_non_vertical,
          par_fit_method,
          inflation_start = inflation[i - 1],
          lake_max_depth = lake_max_depth
        )
      } else if (da_method == "esmda") {
        updates <- FLAREr:::run_esmda(x_matrix,
          h,
          pars_corr,
          zt,
          psi,
          z_index,
          states_depth_start = states_depth_w_noise,
          states_height_start = states_height[i, , , ],
          model_internal_heights_start = model_internal_heights[i, , ],
          lake_depth_start = lake_depth[i, ],
          log_particle_weights_start = log_particle_weights[i - 1, ],
          snow_ice_thickness_start = snow_ice_thickness[, i, ],
          diagnostics_start,
          diagnostics_daily_start,
          pars_config,
          config,
          obs_non_vertical,
          active_in_xmatrix,
          n_non_vertical,
          par_fit_method,
          inflation_start = inflation[i - 1],
          lake_max_depth = lake_max_depth
        )
      } else if (da_method == "letkf") {
        updates <- FLAREr:::run_letkf(x_matrix,
          h,
          pars_corr,
          zt,
          psi,
          z_index,
          states_depth_start = states_depth_w_noise,
          states_height_start = states_height[i, , , ],
          model_internal_heights_start = model_internal_heights[i, , ],
          lake_depth_start = lake_depth[i, ],
          log_particle_weights_start = log_particle_weights[i - 1, ],
          snow_ice_thickness_start = snow_ice_thickness[, i, ],
          diagnostics_start,
          diagnostics_daily_start,
          pars_config,
          config,
          obs_non_vertical,
          active_in_xmatrix,
          n_non_vertical,
          par_fit_method,
          inflation_start = inflation[i - 1],
          lake_max_depth = lake_max_depth
        )
      } else {
        stop("da_method not supported; select enkf, etkf, esmda, letkf, or pf or none")
      }

      if (!is.null(updates$da_diag)) {
        da_diag_steps[[length(da_diag_steps) + 1L]] <- FLAREr:::collect_da_diagnostics(
          da_diag        = updates$da_diag,
          time           = full_time[i],
          states_config  = states_config,
          pars_config    = pars_config,
          modeled_depths = config$model_settings$modeled_depths
        )
      }

      if (npars > 0) {
        if (use_one_step_lag && length(z_index) > 0 && da_method != "pf") {
          # One-step-lag: run a dedicated parameter EnKF using the pre-DA
          # state ensemble to compute predicted observations.  This avoids
          # state-parameter cross-covariance artifacts in the state filter.
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
      model_internal_heights[i, , ] <- updates$model_internal_heights_updated
      states_height[i, , , ] <- updates$states_height_updated
      states_depth[i, , , ] <- updates$states_depth_updated

      if (length(config$output_settings$diagnostics_names) > 0) {
        diagnostics[, i, , ] <- updates$diagnostics_updated
      } else {
        diagnostics <- updates$diagnostics_updated
      }

      if (length(config$output_settings$diagnostics_daily$names) > 0) {
        diagnostics_daily[, i, ] <- updates$diagnostics_daily_updated
      } else {
        diagnostics_daily <- updates$diagnostics_daily_updated
      }
      lake_depth[i, ] <- updates$lake_depth_updated
      log_particle_weights[i, ] <- updates$log_particle_weights_updated
      snow_ice_thickness[, i, ] <- updates$snow_ice_thickness_updated

      inflation[i] <- updates$inflation_update

      # Re-derive depth-indexed states from the DA-updated height-indexed
      # states, then mask depths that exceed the current lake surface.
      for (m in 1:nmembers) {
        depth_index <- which(config$model_settings$modeled_depths > lake_depth[i, m])
        non_na_heights <- which(!is.na(model_internal_heights[i, , m]))
        glm_depths <- lake_depth[i, m] - model_internal_heights[i, non_na_heights, m]
        for (s in 1:nstates) {
          states_depth[i, s, depth_index, m] <- NA
          states_depth[i, s, , m] <- approx(glm_depths, states_height[i, s, non_na_heights, m], config$model_settings$modeled_depths, rule = 2)$y
        }
      }

      if (length(config$output_settings$diagnostics_names) > 0) {
        for (d in seq_len(dim(diagnostics)[1])) {
          for (m in 1:nmembers) {
            depth_index <- which(config$model_settings$modeled_depths > lake_depth[i, m])
            diagnostics[d, i, depth_index, m] <- NA
          }
        }
      }
    }


    ###############

    # Print parameters to screen
    if (npars > 0) {
      for (par in 1:npars) {
        if (pars_config$fix_par[par] == 0) {
          message(paste0(
            pars_config$par_names_save[par], ": mean ",
            round(mean(pars[i, par, ]), 4), " sd ",
            round(sd(pars[i, par, ]), 4)
          ))
        }
      }
    }
  } # END TIME STEP LOOP

  file_names <- create_filenames(full_time, hist_days, forecast_days, config)


  list(
    full_time = full_time,
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
    glm_restart_staged = glm_restart_staged,
    da_diagnostics = da_diag_steps
  )
}
