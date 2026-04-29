
#' Generate initial conditions from existing output file
#'
#' @param restart_file file name of FLARE output csv (legacy NetCDF path)
#' @param state_names names of states that are initialized
#' @param par_names (optional) names of parameters that are initialized
#' @param restart_index (optional) time index in restart file used for initialization
#' @noRd
#' @return list of initial conditions
#'
generate_restart_initial_conditions <- function(restart_file, state_names, par_names = NULL, restart_index){

  nc <- ncdf4::nc_open(restart_file)
  restart_nmembers <- length(ncdf4::ncvar_get(nc, "ensemble"))

  message(paste0("Using restart file with restart index of ", restart_index))

  lake_depth_restart <- ncdf4::ncvar_get(nc, "lake_depth")[restart_index, ]
  snow_ice_thickness_restart <- ncdf4::ncvar_get(nc, "snow_ice_thickness")[, restart_index, ]
  avg_surf_temp_restart <- ncdf4::ncvar_get(nc, "avg_surf_temp")[restart_index, ]
  mixing_restart <- ncdf4::ncvar_get(nc, "mixing_vars")[ ,restart_index, ]
  mixer_count <- ncdf4::ncvar_get(nc, "mixer_count")[restart_index, ]
  log_particle_weights <- ncdf4::ncvar_get(nc, "log_particle_weights")[restart_index, ]
  model_internal_heights  <- ncdf4::ncvar_get(nc, "model_internal_heights")[restart_index, , ]
  inflation  <- ncdf4::ncvar_get(nc, "inflation")[restart_index]

  states_restart <- array(NA, dim = c(length(state_names), dim(model_internal_heights)[1], restart_nmembers))
  for(i in seq_along(state_names)){
    states_restart[i, , ] <- ncdf4::ncvar_get(nc,paste0(state_names[i],"_heights"))[restart_index, , ]
  }

  if(!is.null(par_names)){
    pars_restart <- array(NA, dim = c(length(par_names), restart_nmembers))
    for(p in seq_along(par_names)){
      pars_restart[p, ] <- ncdf4::ncvar_get(nc, par_names[p])[restart_index, ]
    }
  }else{
    pars_restart <- NULL
  }

  ncdf4::nc_close(nc)

  return(list(states = states_restart,
              pars = pars_restart,
              lake_depth = lake_depth_restart,
              snow_ice_thickness = snow_ice_thickness_restart,
              avg_surf_temp = avg_surf_temp_restart,
              mixing_vars = mixing_restart,
              mixer_count = mixer_count,
              model_internal_heights = model_internal_heights,
              log_particle_weights = log_particle_weights,
              inflation = inflation)
  )
}

#' Generate initial conditions from a GLM restart zip file
#'
#' Unpacks the zip, reads the FLARE restart NetCDF for ensemble DA state,
#' and copies the per-ensemble GLM restart files into the ensemble working
#' directories.  Mixer state, avg_surf_temp, and mixer_count are carried
#' entirely by the GLM restart files and are NOT read from the FLARE NetCDF.
#'
#' @param restart_zip_file path to zip file
#' @param state_names names of states to initialize
#' @param par_names (optional) names of parameters to initialize
#' @param restart_index time index in the FLARE restart NetCDF
#' @param restart_date character "YYYY-MM-DD" matching the simulation start date; used to locate the correct per-date directory inside the zip
#' @param working_directory full path to the execute directory (config$file_path$execute_directory); ensemble subdirectories are created here
#' @param nmembers number of ensemble members
#' @noRd
#' @return list of initial conditions (mixing_vars, mixer_count, avg_surf_temp are NULL — carried by GLM restart files)
#'
generate_restart_initial_conditions_from_zip <- function(restart_zip_file,
                                                         state_names,
                                                         par_names = NULL,
                                                         restart_index,
                                                         restart_date,
                                                         working_directory,
                                                         nmembers) {

  tmp_dir <- tempfile()
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  zip::unzip(restart_zip_file, exdir = tmp_dir)

  # Find top-level FLARE restart NetCDF (not in a subdirectory)
  all_files <- list.files(tmp_dir, full.names = TRUE, recursive = FALSE)
  flare_nc_path <- all_files[grepl("\\.nc$", all_files)]
  if (length(flare_nc_path) == 0) {
    stop("No FLARE restart NetCDF (.nc) found at the top level of the zip file.")
  }
  flare_nc_path <- flare_nc_path[1]

  nc <- ncdf4::nc_open(flare_nc_path)
  restart_nmembers <- length(ncdf4::ncvar_get(nc, "ensemble"))

  message(paste0("Using GLM restart zip with restart index ", restart_index,
                 " (date: ", restart_date, ")"))

  # collapse_degen = FALSE prevents ncdf4 from dropping the time dimension when
  # the restart file contains only a single timestep (the common default case).
  lake_depth_restart        <- ncdf4::ncvar_get(nc, "lake_depth",           collapse_degen = FALSE)[restart_index, ]
  snow_ice_thickness_restart <- ncdf4::ncvar_get(nc, "snow_ice_thickness",  collapse_degen = FALSE)[, restart_index, ]
  log_particle_weights      <- ncdf4::ncvar_get(nc, "log_particle_weights", collapse_degen = FALSE)[restart_index, ]
  model_internal_heights    <- ncdf4::ncvar_get(nc, "model_internal_heights", collapse_degen = FALSE)[restart_index, , ]
  inflation                 <- ncdf4::ncvar_get(nc, "inflation",             collapse_degen = FALSE)[restart_index]

  states_restart <- array(NA, dim = c(length(state_names), dim(model_internal_heights)[1], restart_nmembers))
  for (i in seq_along(state_names)) {
    states_restart[i, , ] <- ncdf4::ncvar_get(nc, paste0(state_names[i], "_heights"), collapse_degen = FALSE)[restart_index, , ]
  }

  if (!is.null(par_names)) {
    pars_restart <- array(NA, dim = c(length(par_names), restart_nmembers))
    for (p in seq_along(par_names)) {
      pars_restart[p, ] <- ncdf4::ncvar_get(nc, par_names[p], collapse_degen = FALSE)[restart_index, ]
    }
  } else {
    pars_restart <- NULL
  }

  ncdf4::nc_close(nc)

  # Locate the per-date GLM restart directory inside the zip
  date_dirs <- list.dirs(tmp_dir, full.names = TRUE, recursive = FALSE)
  glm_dir <- date_dirs[basename(date_dirs) == restart_date]
  if (length(glm_dir) == 0) {
    stop(paste0("No GLM restart directory found for date '", restart_date,
                "' inside the zip file. Available dates: ",
                paste(basename(date_dirs), collapse = ", ")))
  }
  glm_dir <- glm_dir[1]

  # Copy GLM restart files into ensemble working directories
  for (m in seq_len(nmembers)) {
    ens_dir <- file.path(working_directory, m)
    if (!dir.exists(ens_dir)) {
      dir.create(ens_dir, recursive = TRUE, showWarnings = FALSE)
    }
    dst <- file.path(ens_dir, paste0("glm_restart_", m, ".nc"))
    if (file.exists(dst)) unlink(dst)

    src <- file.path(glm_dir, paste0("glm_restart_", m, ".nc"))
    if (!file.exists(src)) {
      warning(paste0("GLM restart file not found for ensemble member ", m,
                     ": ", src, ". Skipping."))
      next
    }
    file.copy(src, dst)
  }

  return(list(
    states                = states_restart,
    pars                  = pars_restart,
    lake_depth            = lake_depth_restart,
    snow_ice_thickness    = snow_ice_thickness_restart,
    avg_surf_temp         = NULL,
    mixing_vars           = NULL,
    mixer_count           = NULL,
    model_internal_heights = model_internal_heights,
    log_particle_weights  = log_particle_weights,
    inflation             = inflation
  ))
}
