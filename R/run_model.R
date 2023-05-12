
#' Run GLM
#' @param i time step index
#' @param m ensemble index
#' @param mixing_vars_start vector; mixing variables vector
#' @param curr_start datetime of current time step
#' @param curr_stop datetime of end of run
#' @param par_names names of parameters that are being calibrated
#' @param curr_pars value for the parameters
#' @param working_directory full path to the directory where the model is executed
#' @param par_nml vector of namelist names associated with each parameter being calibrated
#' @param num_phytos number of phytoplankton groups
#' @param glm_depths_start depth from the last GLM run
#' @param lake_depth_start depth of lake
#' @param x_start state vector
#' @param full_time vector of time step for entire simulation
#' @param wq_start starting index of each state in the x_start
#' @param wq_end ending index of each state in the x_start
#' @param management management list
#' @param hist_days number of historical simulations before forecasting
#' @param modeled_depths depths that are include in the state vector
#' @param ndepths_modeled number of depths in modeled_depths
#' @param curr_met_file met file associated with the ensemble member
#' @param inflow_file_name inflow file associated with the ensemble member
#' @param outflow_file_name outflow file associated with the ensemble member
#' @param glm_output_vars vector of output variable names
#' @param diagnostics_names vector of output diagnmostic names
#' @param npars number of parameters calibrated
#' @param num_wq_vars number of water quality variables
#' @param snow_ice_thickness_start vector of snow and ice states
#' @param avg_surf_temp_start average surface temperature
#' @param nstates number of nstates simulated
#' @param state_names state names
#' @param include_wq boolean; TRUE = use water quality model
#' @param debug boolen; TRUE = turn on more messages for debugging
#'
#' @return list of output variables
#' @export
#' @noRd

run_model <- function(i,
                      m,
                      mixing_vars_start,
                      curr_start,
                      curr_stop,
                      par_names,
                      curr_pars,
                      working_directory,
                      par_nml,
                      num_phytos,
                      glm_depths_start,
                      lake_depth_start,
                      x_start,
                      full_time,
                      wq_start,
                      wq_end,
                      management = NULL,
                      hist_days,
                      modeled_depths,
                      ndepths_modeled,
                      curr_met_file,
                      inflow_file_name,
                      outflow_file_name,
                      glm_output_vars,
                      diagnostics_names,
                      npars,
                      num_wq_vars,
                      snow_ice_thickness_start,
                      avg_surf_temp_start,
                      nstates,
                      state_names,
                      include_wq,
                      debug = TRUE){

  switch(Sys.info() [["sysname"]],
         Linux = { machine <- "unix" },
         Darwin = { machine <- "mac" },
         Windows = { machine <- "windows"})

  if(is.null(management)){
    simulate_sss <- FALSE
  }else{
    simulate_sss <- management$simulate_sss
  }

  update_glm_nml_list <- list()
  update_aed_nml_list <- list()
  update_phyto_nml_list <- list()
  update_glm_nml_names <- c()
  update_aed_nml_names <- c()
  update_phyto_nml_names <- c()
  list_index <- 1
  list_index_aed <- 1
  list_index_phyto <- 1

  update_glm_nml_list[[list_index]] <- mixing_vars_start
  update_glm_nml_names[list_index] <- "restart_variables"
  list_index <- list_index + 1

  update_glm_nml_list[[list_index]] <- curr_start
  update_glm_nml_names[list_index] <- "start"
  list_index <- list_index + 1

  update_glm_nml_list[[list_index]] <- curr_stop
  update_glm_nml_names[list_index] <- "stop"
  list_index <- list_index + 1

  glm_depths_end <- rep(NA,length(glm_depths_start))

  diagnostics <- array(NA, dim = c(length(diagnostics_names),ndepths_modeled))

  x_star_end <- array(NA, dim =c(nstates, ndepths_modeled))

  if(npars > 0){

    unique_pars <- unique(par_names)

    for(par in 1:length(unique_pars)){

      curr_par_set <- which(par_names == unique_pars[par])
      curr_nml <- par_nml[curr_par_set[1]]
      if(curr_nml == "glm3.nml"){
        update_glm_nml_list[[list_index]] <- round(curr_pars[curr_par_set], 4)
        update_glm_nml_names[list_index] <- unique_pars[par]
        list_index <- list_index + 1
      }else if(curr_nml == "aed2.nml"){
        update_aed_nml_list[[list_index_aed]] <- round(curr_pars[curr_par_set], 4)
        update_aed_nml_names[list_index_aed] <- unique_pars[par]
        list_index_aed <- list_index_aed + 1
      }else if(curr_nml == "aed_phyto_pars.csv"){
        update_phyto_nml_list[[list_index_phyto]] <- rep(round(curr_pars[curr_par_set],4), num_phytos)
        update_phyto_nml_names[list_index_phyto] <- unique_pars[par]
        list_index_phyto <- list_index_phyto + 1
      }
    }
  }

  glm_depths_tmp <- glm_depths_start[!is.na(glm_depths_start)]
  glm_depths_tmp_tmp <- c(glm_depths_tmp, lake_depth_start)
  glm_depths_mid <- glm_depths_tmp_tmp[1:(length(glm_depths_tmp_tmp)-1)] + diff(glm_depths_tmp_tmp)/2

  if(include_wq){

    wq_init_vals <- c()
    start_index <- 2

    for(wq in 1:num_wq_vars){

      wq_enkf_tmp <- x_start[start_index + wq, ]
      wq_enkf_tmp[wq_enkf_tmp < 0] <- 0
      wq_init_vals <- c(wq_init_vals,
                        approx(modeled_depths,wq_enkf_tmp, glm_depths_mid, rule = 2)$y)
    }
    update_glm_nml_list[[list_index]] <- round(wq_init_vals, 4)
    update_glm_nml_names[list_index] <- "wq_init_vals"
    list_index <- list_index + 1

    if(simulate_sss){
      if(is.na(management$specified_sss_inflow_file)){
        FLAREr:::create_sss_input_output(x = x_start,
                                         i,
                                         m,
                                         full_time,
                                         working_directory,
                                         wq_start,
                                         management$management_input,
                                         hist_days,
                                         management$forecast_sss_on,
                                         management$sss_depth,
                                         management$use_specified_sss,
                                         state_names,
                                         modeled_depths = modeled_depths,
                                         forecast_sss_flow = management$forecast_sss_flow,
                                         forecast_sss_oxy = management$forecast_sss_oxy,
                                         salt = salt_start)
      }else{
        file.copy(file.path(working_directory, management$specified_sss_inflow_file), paste0(working_directory,"/sss_inflow.csv"))
        if(!is.na(management$specified_sss_outflow_file)){
          file.copy(file.path(working_directory, management$specified_sss_outflow_file), paste0(working_directory,"/sss_outflow.csv"))
        }
      }
    }
  }

  the_temps_enkf_tmp <- x_start[1, ]
  the_temps_glm <- approx(modeled_depths,the_temps_enkf_tmp, glm_depths_mid, rule = 2)$y
  update_glm_nml_list[[list_index]] <- round(the_temps_glm, 4)
  update_glm_nml_names[list_index] <- "the_temps"
  list_index <- list_index + 1

  salt_start <- x_start[2, ]
  the_sals_glm <- approx(modeled_depths,salt_start, glm_depths_mid, rule = 2)$y
  update_glm_nml_list[[list_index]] <- round(the_sals_glm, 4)
  update_glm_nml_names[list_index] <- "the_sals"
  list_index <- list_index + 1

  update_glm_nml_list[[list_index]] <- round(glm_depths_tmp, 4)
  update_glm_nml_names[list_index] <- "the_depths"
  list_index <- list_index + 1

  update_glm_nml_list[[list_index]] <- length(glm_depths_tmp)
  update_glm_nml_names[list_index] <- "num_depths"
  list_index <- list_index + 1

  update_glm_nml_list[[list_index]] <- round(lake_depth_start, 4)
  update_glm_nml_names[list_index] <- "lake_depth"
  list_index <- list_index + 1

  update_glm_nml_list[[list_index]] <- 0.0
  update_glm_nml_names[list_index] <- "snow_thickness"
  list_index <- list_index + 1

  update_glm_nml_list[[list_index]] <- round(snow_ice_thickness_start[2], 4)
  update_glm_nml_names[list_index] <- "white_ice_thickness"
  list_index <- list_index + 1

  update_glm_nml_list[[list_index]] <- round(snow_ice_thickness_start[3], 4)
  update_glm_nml_names[list_index] <- "blue_ice_thickness"
  list_index <- list_index + 1

  update_glm_nml_list[[list_index]] <- round(avg_surf_temp_start, 4)
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


  FLAREr:::update_nml(var_list = update_glm_nml_list,
                      var_name_list = update_glm_nml_names,
                      working_directory,
                      nml = "glm3.nml")

  if(list_index_aed > 1){
    FLAREr:::update_nml(update_aed_nml_list,
                        update_aed_nml_names,
                        working_directory,
                        "aed2.nml")
  }

  if(list_index_phyto > 1){
    phytos <- readr::read_csv(file.path(working_directory, "aed_phyto_pars.csv"),show_col_types = FALSE)

    for(k in 1:length(update_phyto_nml_names)){
      phytos[which(stringr::str_detect(phytos$`'p_name'`, update_phyto_nml_names[[k]])),2:ncol(phytos)] <- update_phyto_nml_list[[k]]
    }

    readr::write_csv(phytos, file.path(working_directory, "aed_phyto_pars.csv"))
  }

  #Use GLM NML files to run GLM for a day
  # Only allow simulations without NaN values in the output to proceed.
  #Necessary due to random Nan in AED output
  pass <- FALSE
  num_reruns <- 0
  verbose <- FALSE

  if(i == 2 & m == 1 & debug){
    file.copy(from = paste0(working_directory, "/", "glm3.nml"), #GLM SPECIFIC
              to = paste0(working_directory, "/", "glm3_initial.nml"),
              overwrite = TRUE) #GLM SPECIFIC
  }

  while(!pass){
    unlink(paste0(working_directory, "/output.nc"))

    if(machine %in% c("unix", "mac", "windows")){
      GLM3r::run_glm(sim_folder = working_directory, verbose = verbose)
    }else{
      message("Machine not identified")
      stop()
    }

    if(file.exists(paste0(working_directory, "/output.nc"))){

      nc <- tryCatch(ncdf4::nc_open(paste0(working_directory, "/output.nc")),
                     error = function(e){
                       warning(paste0(e$message, " error in output.nc regenration: ensemble ", m),
                               call. = FALSE)
                       return(NULL)
                     },
                     finally = NULL)

      if(!is.null(nc)){
        tallest_layer <- ncdf4::ncvar_get(nc, "NS")[1]
        ncdf4::nc_close(nc)
        if(!is.na(tallest_layer)){
          if(tallest_layer > 1) {
            success <- TRUE
          } else {
            # Catch for if the output has more than one layer
            message(paste0("'output.nc' file generated but has one layer in the file. Re-running simulation: ensemble ", m))
            success <- FALSE
            if(debug){
              verbose <- TRUE
            }
          }
        }else{
          message(paste0("'output.nc' file generated but has NA for the layer in the file. Re-running simulation: ensemble ", m))
          success <- FALSE
          if(debug){
            verbose <- TRUE
          }
        }
      }else{
        message(paste0("'output.nc' file generated but has NA for the layer in the file. Re-running simulation: ensemble ", m))
        success <- FALSE
        if(debug){
          verbose <- TRUE
        }
      }
    }else{
      message(paste0("'output.nc' file not generated. Re-running simulation: ensemble ", m))
      success <- FALSE
      if(debug){
        verbose <- TRUE
      }
    }



    if(success){

      output_vars_multi_depth <- state_names
      output_vars_no_depth <- NA

      GLM_temp_wq_out <-  FLAREr:::get_glm_nc_var_all_wq(ncFile = "/output.nc",
                                                         working_dir = working_directory,
                                                         z_out = modeled_depths,
                                                         vars_depth = output_vars_multi_depth,
                                                         vars_no_depth = output_vars_no_depth,
                                                         diagnostic_vars = diagnostics_names)

      if(!debug){
        unlink(paste0(working_directory, "/output.nc"))
      }

      num_glm_depths <- length(GLM_temp_wq_out$depths_enkf)
      glm_temps <- rev(GLM_temp_wq_out$output[ ,1])

      glm_depths_end[1:num_glm_depths] <- GLM_temp_wq_out$depths_enkf

      glm_depths_tmp <- c(GLM_temp_wq_out$depths_enkf,GLM_temp_wq_out$lake_depth)

      glm_depths_mid <- glm_depths_tmp[1:(length(glm_depths_tmp)-1)] + diff(glm_depths_tmp)/2

      x_star_end[1, ] <- approx(glm_depths_mid,glm_temps, modeled_depths, rule = 2)$y

      glm_salt <- rev(GLM_temp_wq_out$output[ ,2])
      x_star_end[2, ] <- approx(glm_depths_mid, glm_salt, modeled_depths, rule = 2)$y

      if(include_wq){
      start_index <- 2
        for(wq in 1:num_wq_vars){
          glm_wq <-  rev(GLM_temp_wq_out$output[ ,start_index+wq])
          #if(length(is.na(glm_wq)) > 0){next}
          x_star_end[start_index + wq, ] <- approx(glm_depths_mid,glm_wq, modeled_depths, rule = 2)$y
        }
      }

      if(length(diagnostics_names) > 0){
        for(wq in 1:length(diagnostics_names)){
          glm_wq <-  rev(GLM_temp_wq_out$diagnostics_output[ , wq])
          diagnostics[wq , ] <- approx(glm_depths_mid,glm_wq, modeled_depths, rule = 2)$y
        }
      }

      if(length(which(is.na(x_star_end))) == 0){
        pass = TRUE
      }else{
        message("NA or NaN in output file'. Re-running simulation...")
        num_reruns <- num_reruns + 1
      }
    }
    num_reruns <- num_reruns + 1
    if(num_reruns > 100){
      stop(paste0("Too many re-runs (> 1000) due to issues generating output"))
    }

  }

  return(list(x_star_end  = x_star_end,
              lake_depth_end  = GLM_temp_wq_out$lake_depth,
              snow_ice_thickness_end  = GLM_temp_wq_out$snow_wice_bice,
              avg_surf_temp_end  = GLM_temp_wq_out$avg_surf_temp,
              mixing_vars_end = GLM_temp_wq_out$mixing_vars,
              diagnostics_end  = diagnostics,
              model_internal_depths  = glm_depths_end,
              curr_pars = curr_pars))
}
