
#' Update GLM restart file with FLARE DA-corrected state
#'
#' Overwrites lake layer arrays, ice/snow state, and WQ variables in an
#' existing GLM restart NetCDF with the current FLARE ensemble state.
#' Mixer state, avg_surf_temp, and mixer_count are left untouched — they
#' are already correct from the previous GLM run.
#' Does nothing if no glm_restart.nc exists in the directory.
#' @noRd
update_glm_restart_file <- function(ens_working_directory,
                                    m,
                                    states_heights_start,
                                    glm_heights_start,
                                    snow_ice_thickness_start,
                                    num_wq_vars,
                                    include_wq,
                                    state_names = NULL) {
  rst_path <- file.path(ens_working_directory, paste0("glm_restart_", m, ".nc"))
  if(!file.exists(rst_path)) return(invisible(NULL))

  nc <- ncdf4::nc_open(rst_path, write = TRUE)
  native_idx <- which(!is.na(glm_heights_start))
  nlev <- length(native_idx)
  max_layers_rst <- nc$dim[["nlev"]]$len

  temp_buf <- rep(0.0, max_layers_rst)
  salt_buf <- rep(0.0, max_layers_rst)
  hgt_buf  <- rep(0.0, max_layers_rst)
  temp_buf[seq_len(nlev)] <- rev(states_heights_start[1, native_idx])
  salt_buf[seq_len(nlev)] <- rev(states_heights_start[2, native_idx])
  hgt_buf[seq_len(nlev)]  <- rev(glm_heights_start[native_idx])

  ncdf4::ncvar_put(nc, "lake_temp",   temp_buf)
  ncdf4::ncvar_put(nc, "lake_salt",   salt_buf)
  ncdf4::ncvar_put(nc, "lake_height", hgt_buf)

  ncdf4::ncvar_put(nc, "blue_ice",       snow_ice_thickness_start[3])
  ncdf4::ncvar_put(nc, "white_ice",      snow_ice_thickness_start[2])
  ncdf4::ncvar_put(nc, "snow_thickness", 0.0)

  if(include_wq && num_wq_vars > 0 &&
     "wq_vars" %in% names(nc$var)) {
    # Read the existing full WQ array [num_wq_vars_glm, MaxLayers]
    wq_buf <- ncdf4::ncvar_get(nc, "wq_vars")
    # Read GLM WQ variable names and map FLARE state names to positions
    if(!is.null(state_names) && "wq_var_names" %in% names(nc$var)) {
      glm_wq_names <- trimws(ncdf4::ncvar_get(nc, "wq_var_names"))
      flare_wq_names <- state_names[seq(3, 2 + num_wq_vars)]
      for(wq in seq_len(num_wq_vars)) {
        glm_idx <- which(glm_wq_names == flare_wq_names[wq])
        if(length(glm_idx) == 1) {
          wq_buf[seq_len(nlev), glm_idx] <- rev(states_heights_start[2 + wq, native_idx])
          if(nlev < max_layers_rst) {
            wq_buf[(nlev + 1):max_layers_rst, glm_idx] <- 0.0
          }
        }
      }
    } else {
      # Fallback: assume FLARE WQ vars are the first num_wq_vars in GLM
      for(wq in seq_len(min(num_wq_vars, nrow(wq_buf)))) {
        wq_buf[seq_len(nlev), wq] <- rev(states_heights_start[2 + wq, native_idx])
        if(nlev < max_layers_rst) {
          wq_buf[(nlev + 1):max_layers_rst, wq] <- 0.0
        }
      }
    }
    ncdf4::ncvar_put(nc, "wq_vars", wq_buf)
  }

  ncdf4::ncatt_put(nc, 0, "NumLayers", nlev, prec = "int")
  ncdf4::nc_close(nc)
  invisible(rst_path)
}

#' Run GLM
#' @param i time step index
#' @param m ensemble index
#' @param mixing_vars_start vector; mixing variables vector
#' @param curr_start datetime of current time step
#' @param curr_stop datetime of end of run
#' @param par_names names of parameters that are being calibrated
#' @param curr_pars_ens value for the parameters
#' @param ens_working_directory full path to the directory where the model is executed
#' @param par_nml vector of namelist names associated with each parameter being calibrated
#' @param num_phytos number of phytoplankton groups
#' @param glm_depths_start depth from the last GLM run
#' @param lake_depth_start depth of lake
#' @param x_start state vector
#' @param full_time vector of time step for entire simulation
#' @param hist_days number of historical simulations before forecasting
#' @param modeled_depths depths that are include in the state vector
#' @param ndepths_modeled number of depths in modeled_depths
#' @param curr_met_file met file associated with the ensemble member
#' @param inflow_file_name inflow file associated with the ensemble member
#' @param outflow_file_name outflow file associated with the ensemble member
#' @param glm_output_vars vector of output variable names
#' @param diagnostics_names vector of output diagnostic names
#' @param npars number of parameters calibrated
#' @param num_wq_vars number of water quality variables
#' @param snow_ice_thickness_start vector of snow and ice states
#' @param avg_surf_temp_start average surface temperature
#' @param nstates number of nstates simulated
#' @param state_names state names
#' @param include_wq boolean; TRUE = use water quality model
#' @param states_heights_start matrix of states orientated by height
#' @param max_layers max number of layers allowed in the GLM
#'
#' @return list of output variables
#' @noRd

run_model <- function(i,
                      m,
                      mixing_vars_start,
                      mixer_count_start,
                      curr_start,
                      curr_stop,
                      par_names,
                      curr_pars_ens,
                      ens_working_directory,
                      par_nml,
                      num_phytos,
                      glm_heights_start,
                      lake_depth_start,
                      full_time,
                      hist_days,
                      modeled_depths,
                      ndepths_modeled,
                      curr_met_file,
                      inflow_file_name,
                      outflow_file_name,
                      glm_output_vars,
                      diagnostics_names,
                      diagnostics_daily_config,
                      npars,
                      num_wq_vars,
                      snow_ice_thickness_start,
                      avg_surf_temp_start,
                      nstates,
                      state_names,
                      include_wq,
                      states_heights_start,
                      max_layers,
                      glm_path,
                      use_glm_restart = FALSE){

  rounding_level <- 5

  update_glm_nml_list <- list()
  update_aed_nml_list <- list()
  update_phyto_nml_list <- list()
  update_glm_nml_names <- c()
  update_aed_nml_names <- c()
  update_phyto_nml_names <- c()
  list_index <- 1
  list_index_aed <- 1
  list_index_phyto <- 1

  update_glm_nml_list[[list_index]] <- mixer_count_start
  update_glm_nml_names[list_index] <- "restart_mixer_count"
  list_index <- list_index + 1

  update_glm_nml_list[[list_index]] <- mixing_vars_start
  update_glm_nml_names[list_index] <- "restart_variables"
  list_index <- list_index + 1

  update_glm_nml_list[[list_index]] <- curr_start
  update_glm_nml_names[list_index] <- "start"
  list_index <- list_index + 1

  update_glm_nml_list[[list_index]] <- curr_stop
  update_glm_nml_names[list_index] <- "stop"
  list_index <- list_index + 1

  glm_heights_end <- rep(NA,length(max_layers))
  diagnostics <- array(NA, dim = c(length(diagnostics_names),max_layers))
  x_star_end <- array(NA, dim =c(nstates, max_layers))
  native_heights_index <- which(!is.na(glm_heights_start))

  if(npars > 0){

    unique_pars <- unique(par_names)

    for(par in 1:length(unique_pars)){

      curr_par_set <- which(par_names == unique_pars[par])
      curr_nml <- par_nml[curr_par_set[1]]
      if(curr_nml == "glm3.nml"){
        update_glm_nml_list[[list_index]] <- round(curr_pars_ens[curr_par_set], rounding_level)
        update_glm_nml_names[list_index] <- unique_pars[par]
        list_index <- list_index + 1
      }else if(curr_nml == "aed2.nml"){
        update_aed_nml_list[[list_index_aed]] <- round(curr_pars_ens[curr_par_set], rounding_level)
        update_aed_nml_names[list_index_aed] <- unique_pars[par]
        list_index_aed <- list_index_aed + 1
      }else if(curr_nml == "aed_phyto_pars.csv"){
        update_phyto_nml_list[[list_index_phyto]] <- rep(round(curr_pars_ens[curr_par_set],rounding_level), num_phytos)
        update_phyto_nml_names[list_index_phyto] <- unique_pars[par]
        list_index_phyto <- list_index_phyto + 1
      }
    }
  }

  if(include_wq){

    wq_init_vals <- c()
    start_index <- 2

    for(wq in 1:num_wq_vars){
      wq_tmp <- rev(states_heights_start[start_index + wq, native_heights_index])
      wq_init_vals <- c(wq_init_vals, wq_tmp)
    }

    update_glm_nml_list[[list_index]] <- round(wq_init_vals, rounding_level)
    update_glm_nml_names[list_index] <- "wq_init_vals"
    list_index <- list_index + 1
  }

  the_temps_glm <- rev(states_heights_start[1, native_heights_index])
  update_glm_nml_list[[list_index]] <- round(the_temps_glm, rounding_level)
  update_glm_nml_names[list_index] <- "the_temps"
  list_index <- list_index + 1

  the_sals_glm <- rev(states_heights_start[2, native_heights_index])
  update_glm_nml_list[[list_index]] <- round(the_sals_glm, rounding_level)
  update_glm_nml_names[list_index] <- "the_sals"
  list_index <- list_index + 1

  the_heights <- rev(glm_heights_start[native_heights_index])
  update_glm_nml_list[[list_index]] <- round(the_heights, rounding_level)
  update_glm_nml_names[list_index] <- "the_heights"
  list_index <- list_index + 1


  update_glm_nml_list[[list_index]] <- length(the_heights)
  update_glm_nml_names[list_index] <- "num_heights"
  list_index <- list_index + 1

  update_glm_nml_list[[list_index]] <- round(lake_depth_start, rounding_level)
  update_glm_nml_names[list_index] <- "lake_depth"
  list_index <- list_index + 1

  update_glm_nml_list[[list_index]] <- 0.0
  update_glm_nml_names[list_index] <- "snow_thickness"
  list_index <- list_index + 1

  update_glm_nml_list[[list_index]] <- round(snow_ice_thickness_start[2], rounding_level)
  update_glm_nml_names[list_index] <- "white_ice_thickness"
  list_index <- list_index + 1

  update_glm_nml_list[[list_index]] <- round(snow_ice_thickness_start[3], rounding_level)
  update_glm_nml_names[list_index] <- "blue_ice_thickness"
  list_index <- list_index + 1

  update_glm_nml_list[[list_index]] <- round(avg_surf_temp_start, rounding_level)
  update_glm_nml_names[list_index] <- "avg_surf_temp"
  list_index <- list_index + 1

  #ALLOWS THE LOOPING THROUGH NOAA ENSEMBLES

  update_glm_nml_list[[list_index]] <- curr_met_file
  update_glm_nml_names[list_index] <- "meteo_fl"
  list_index <- list_index + 1

  if(!is.null(inflow_file_name)){
    update_glm_nml_list[[list_index]] <- unlist(inflow_file_name)
    update_glm_nml_names[list_index] <- "inflow_fl"
    list_index <- list_index + 1


    update_glm_nml_list[[list_index]] <- rep(max(the_heights) - 0.1, length(unlist(inflow_file_name)))
    update_glm_nml_names[list_index] <- "subm_elev"
    list_index <- list_index + 1

    update_glm_nml_list[[list_index]] <- unlist(outflow_file_name)
    update_glm_nml_names[list_index] <- "outflow_fl"
    list_index <- list_index + 1
  } else {
    update_glm_nml_list[[list_index]] <- 0
    update_glm_nml_names[list_index] <- "num_inflows"
    list_index <- list_index + 1

    update_glm_nml_list[[list_index]] <- 0
    update_glm_nml_names[list_index] <- "num_outlet"
    list_index <- list_index + 1
  }

  update_glm_nml_list[[list_index]] <- as.integer(use_glm_restart)
  update_glm_nml_names[list_index] <- "init_restart_from_file"
  list_index <- list_index + 1

  rst_filename <- paste0("glm_restart_", m, ".nc")
  update_glm_nml_list[[list_index]] <- rst_filename
  update_glm_nml_names[list_index] <- "restart_fname"
  list_index <- list_index + 1

  update_glm_nml_list[[list_index]] <- rst_filename
  update_glm_nml_names[list_index] <- "init_restart_fname"
  list_index <- list_index + 1

  FLAREr:::update_nml(var_list = update_glm_nml_list,
             var_name_list = update_glm_nml_names,
             working_directory = ens_working_directory,
             nml = "glm3.nml")

  if(list_index_aed > 1){
    FLAREr:::update_nml(update_aed_nml_list,
               update_aed_nml_names,
               working_directory = ens_working_directory,
               "aed2.nml")
  }

  if(list_index_phyto > 1){
    phytos <- readr::read_csv(file.path(ens_working_directory, "aed_phyto_pars.csv"),show_col_types = FALSE)

    for(k in 1:length(update_phyto_nml_names)){
      phytos[which(stringr::str_detect(phytos$`'p_name'`, update_phyto_nml_names[[k]])),2:ncol(phytos)] <- update_phyto_nml_list[[k]]
    }

    readr::write_csv(phytos, file.path(ens_working_directory, "aed_phyto_pars.csv"))
  }

  # Update GLM restart file with FLARE DA-corrected state (no-op if absent)
  if(use_glm_restart){
  update_glm_restart_file(
    ens_working_directory    = ens_working_directory,
    m                        = m,
    states_heights_start     = states_heights_start,
    glm_heights_start        = glm_heights_start,
    snow_ice_thickness_start = snow_ice_thickness_start,
    num_wq_vars              = num_wq_vars,
    include_wq               = include_wq,
    state_names              = state_names
  )
  }


  if(m == 1){
    print("initialzed")
    print(i)
  print(glm_heights_start)
  print(states_heights_start[1,native_heights_index])
  }

  #Use GLM NML files to run GLM for a day
  # Only allow simulations without NaN values in the output to proceed.
  #Necessary due to random Nan in AED output
  pass <- FALSE
  num_reruns <- 0
  verbose <- FALSE

  if(i == 2){
    file.copy(from = paste0(ens_working_directory, "/", "glm3.nml"), #GLM SPECIFIC
              to = paste0(ens_working_directory, "/", "glm3_initial.nml"),
              overwrite = TRUE) #GLM SPECIFIC
  }

  verbose <- FALSE
  while(!pass){
    unlink(paste0(ens_working_directory, "/output.nc"))

    run_glm(dir = ens_working_directory, verbose = verbose)
    verbose <- TRUE

    if(file.exists(paste0(ens_working_directory, "/output.nc"))){

      nc <- tryCatch(ncdf4::nc_open(paste0(ens_working_directory, "/output.nc")),
                     error = function(e){
                       warning(paste0(e$message, " error in output.nc regenration: ensemble ", m),
                               call. = FALSE)
                       return(NULL)
                     },
                     finally = NULL)

      if(!is.null(nc)){
        tallest_layer <- ncdf4::ncvar_get(nc, "NS")[1]
        z <- ncdf4::ncvar_get(nc, "z")[1]
        temp <- ncdf4::ncvar_get(nc, "temp")[1]
        ncdf4::nc_close(nc)
        if(!is.na(tallest_layer) | is.nan(temp)){
          if(!is.nan(z)) {
            success <- TRUE
          } else {
            # Catch for if the output has more than one layer
            message(paste0("'output.nc' file generated but the height (z) is NaN. Re-running simulation: ensemble ", m))
            success <- FALSE
          }
        }else{
          message(paste0("'output.nc' file generated but has NA for the layer in the file. Re-running simulation: ensemble ", m))
          success <- FALSE
        }
      }else{
        message(paste0("'output.nc' file generated but has NA for the layer in the file. Re-running simulation: ensemble ", m))
        success <- FALSE
      }
    }else{
      message(paste0("'output.nc' file not generated. Re-running simulation: ensemble ", m))
      success <- FALSE
    }

    if(success){

      output_vars_multi_depth <- state_names
      output_vars_no_depth <- NA

      GLM_temp_wq_out <-  get_glm_nc_var(ncFile = "/output.nc",
                                         working_dir = ens_working_directory,
                                         z_out = modeled_depths,
                                         vars_depth = output_vars_multi_depth,
                                         vars_no_depth = output_vars_no_depth,
                                         diagnostic_vars = diagnostics_names,
                                         diagnostics_daily_config = diagnostics_daily_config)

      unlink(paste0(ens_working_directory, "/output.nc"))

      num_glm_heights <- length(GLM_temp_wq_out$heights)
      glm_heights_end[1:num_glm_heights] <- rev(GLM_temp_wq_out$heights)
      x_star_end[1,1:num_glm_heights] <- rev(GLM_temp_wq_out$output[ ,1])
      x_star_end[2,1:num_glm_heights] <- rev(GLM_temp_wq_out$output[ ,2])

      if(include_wq){
        start_index <- 2
        for(wq in 1:num_wq_vars){
          glm_wq <- rev(GLM_temp_wq_out$output[ ,start_index+wq])
          x_star_end[start_index + wq,1:num_glm_heights] <- glm_wq
        }
      }

      if(length(diagnostics_names) > 0){
        for(wq in 1:length(diagnostics_names)){
          diagnostic <-  rev(GLM_temp_wq_out$diagnostics_output[ , wq])
          diagnostics[wq ,1:num_glm_heights] <- diagnostic
        }
      }

      if(length(which(is.na(x_star_end[, 1:num_glm_heights]))) == 0){
        pass = TRUE
      }else{
        message("NA or NaN in output file'. Re-running simulation...")
        num_reruns <- num_reruns + 1
      }
    }
    num_reruns <- num_reruns + 1
    if(num_reruns > 10){
      stop(paste0("Too many re-runs (> 10) due to issues generating output",
                  '\n Suggest testing specific GLM execution with the following code:',
                  '\n FLAREr:::run_glm(','"' ,ens_working_directory,'")'))
    }

  }

  return(list(x_star_end  = x_star_end,
              lake_depth_end  = GLM_temp_wq_out$lake_depth,
              snow_ice_thickness_end  = GLM_temp_wq_out$snow_wice_bice,
              avg_surf_temp_end  = GLM_temp_wq_out$avg_surf_temp,
              mixing_vars_end = GLM_temp_wq_out$mixing_vars,
              mixer_count_end = GLM_temp_wq_out$mixer_count,
              diagnostics_end  = diagnostics,
              diagnostics_daily_end = GLM_temp_wq_out$diagnostics_daily_output,
              model_internal_heights  = glm_heights_end,
              curr_pars_ens = curr_pars_ens
  ))
}
