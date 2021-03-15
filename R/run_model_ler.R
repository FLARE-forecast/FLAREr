#' @title Download and Downscale NOAA GEFS for a single site
#' @return None
#'
#' @param site_index, index of site_list, lat_list, lon_list to be downloaded
#' @param lat_list, vector of latitudes that correspond to site codes
#' @param lon_list, vector of longitudes that correspond to site codes
#' @param site_list, vector of site codes, used in directory and file name generation
#' @param downscale, logical specifying whether to downscale from 6-hr to 1-hr
#' @param overwrite, logical stating to overwrite any existing output_file
#' @param model_name, directory name for the 6-hr forecast, this will be used in directory and file name generation
#' @param model_name_ds, directory name for the 1-hr forecast, this will be used in directory and file name generation
#' @param output_directory, directory where the model output will be save
#' @noRd
#'
#' @author Quinn Thomas
#'


run_model_ler <- function(model,
                      i,
                      m,
                      mixing_vars_start,
                      curr_start,
                      curr_stop,
                      par_names,
                      curr_pars,
                      working_directory,
                      par_file,
                      num_phytos,
                      model_depths_start,
                      lake_depth_start,
                      x_start,
                      full_time_local,
                      wq_start,
                      wq_end,
                      management = NULL,
                      hist_days,
                      modeled_depths,
                      ndepths_modeled,
                      curr_met_file,
                      inflow_file_name,
                      outflow_file_name,
                      output_vars,
                      diagnostics_names,
                      npars,
                      num_wq_vars,
                      snow_ice_thickness_start,
                      avg_surf_temp_start,
                      salt_start,
                      nstates,
                      state_names,
                      include_wq){

  switch(Sys.info() [["sysname"]],
         Linux = { machine <- "unix" },
         Darwin = { machine <- "mac" },
         Windows = { machine <- "windows"})

  if(is.null(management)){
    simulate_sss <- FALSE
  }else{
    simulate_sss <- management$simulate_sss
  }

  ler_yaml <- "test.yaml"
  yml <- yaml::read_yaml(file.path(working_directory, ler_yaml))
  #
  #
  # yml <- configr::read.config(file.path(working_directory, ler_yaml))
  # configr::write.config(yml, file.path(working_directory, ler_yaml))

  model_depths_end <- rep(NA,length(model_depths_start))

  model_depths_tmp <- model_depths_start[!is.na(model_depths_start)]
  model_depths_tmp_tmp <- c(model_depths_tmp, lake_depth_start)
  model_depths_mid <- model_depths_tmp_tmp[1:(length(model_depths_tmp_tmp)-1)] + diff(model_depths_tmp_tmp)/2


  # GLM ----
  if( model == "GLM") {

    input_yaml_multiple(ler_yaml, key1 = "model_parameters", key2 = "GLM", key3 = "restart_variables", value = mixing_vars_start)
    # yml[["model_parameters"]][[model]][["restart_variables"]] <- mixing_vars_start

    # update_glm_nml_list <- list()
    update_aed_nml_list <- list()
    update_phyto_nml_list <- list()
    # update_glm_nml_names <- c()
    update_aed_nml_names <- c()
    update_phyto_nml_names <- c()
    # list_index <- 1
    list_index_aed <- 1
    list_index_phyto <- 1


    diagnostics <- array(NA, dim = c(length(diagnostics_names),ndepths_modeled))

    x_star_end <- rep(NA, nstates)

    if(npars > 0){

      unique_pars <- unique(par_names)

      for(par in 1:length(unique_pars)){

        curr_par_set <- which(par_names == unique_pars[par])
        curr_nml <- par_file[curr_par_set[1]]
        if(curr_nml == "glm3.nml"){
          input_yaml_multiple(ler_yaml, key1 = "model_parameters", key2 = "GLM", key3 = unique_pars[par], value = round(curr_pars[curr_par_set], 4))
          # yml[["model_parameters"]][[model]][[unique_pars[par]]] <- round(curr_pars[curr_par_set], 4)
        }else if(curr_nml == "aed2.nml"){
          update_aed_nml_list[[list_index_aed]] <- round(curr_pars[curr_par_set], 4)
          update_aed_nml_names[list_index_aed] <- unique_pars[par]
          list_index_aed <- list_index_aed + 1
        }else if(curr_nml == "aed2_phyto_pars.nml"){
          update_phyto_nml_list[[list_index_phyto]] <- rep(round(curr_pars[curr_par_set],4), num_phytos)
          update_phyto_nml_names[list_index_phyto] <- unique_pars[par]
          list_index_phyto <- list_index_phyto + 1
        }
      }
    }

    if(include_wq){

      wq_init_vals <- c()

      for(wq in 1:num_wq_vars){
        wq_enkf_tmp <- x_start[wq_start[wq + 1]:wq_end[wq + 1]]
        wq_enkf_tmp[wq_enkf_tmp < 0] <- 0
        wq_init_vals <- c(wq_init_vals,
                          approx(modeled_depths,wq_enkf_tmp, model_depths_mid, rule = 2)$y)
      }
      update_glm_nml_list[[list_index]] <- round(wq_init_vals, 4)
      update_glm_nml_names[list_index] <- "wq_init_vals"
      list_index <- list_index + 1

      if(simulate_sss){
        if(is.na(management$specified_sss_inflow_file)){
          flare:::create_sss_input_output(x = x_start,
                                          i,
                                          m,
                                          full_time_local,
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



    # input_yaml_multiple(ler_yaml, key1 = "model_parameters", key2 = "GLM", key3 = "the_sals", value = round(the_sals_glm, 4))
    input_yaml_multiple(ler_yaml, key1 = "model_parameters", key2 = "GLM", key3 = "num_depths", value = length(model_depths_tmp))
    input_yaml_multiple(ler_yaml, key1 = "model_parameters", key2 = "GLM", key3 = "lake_depth", value = round(lake_depth_start, 4))
    input_yaml_multiple(ler_yaml, key1 = "model_parameters", key2 = "GLM", key3 = "snow_thickness", value = 0)
    input_yaml_multiple(ler_yaml, key1 = "model_parameters", key2 = "GLM", key3 = "white_ice_thickness", value = round(snow_ice_thickness_start[2], 4))
    input_yaml_multiple(ler_yaml, key1 = "model_parameters", key2 = "GLM", key3 = "blue_ice_thickness", value = round(snow_ice_thickness_start[3], 4))
    input_yaml_multiple(ler_yaml, key1 = "model_parameters", key2 = "GLM", key3 = "avg_surf_temp", value = round(avg_surf_temp_start, 4))
    input_yaml_multiple(ler_yaml, key1 = "model_parameters", key2 = "GLM", key3 = "out_dir", value = "'output'")

    # # yml[["model_parameters"]][[model]][["the_temps"]] <- round(the_temps_glm, 4)
    # yml[["model_parameters"]][[model]][["the_sals"]] <- round(the_sals_glm, 4)
    # # yml[["model_parameters"]][[model]][["the_depths"]] <- round(model_depths_tmp, 4)
    # yml[["model_parameters"]][[model]][["num_depths"]] <- length(model_depths_tmp)
    # yml[["model_parameters"]][[model]][["lake_depth"]] <- round(lake_depth_start, 4)
    # yml[["model_parameters"]][[model]][["snow_thickness"]] <- 0
    # yml[["model_parameters"]][[model]][["white_ice_thickness"]] <- round(snow_ice_thickness_start[2], 4)
    # yml[["model_parameters"]][[model]][["blue_ice_thickness"]] <- round(snow_ice_thickness_start[3], 4)
    # yml[["model_parameters"]][[model]][["avg_surf_temp"]] <- round(avg_surf_temp_start, 4)



  }

  # GOTM ----
  if( model == "GOTM") {

    # input_yaml_multiple(ler_yaml, key1 = "model_parameters", key2 = "GLM", key3 = "restart_variables", value = mixing_vars_start)
    # yml[["model_parameters"]][[model]][["restart_variables"]] <- mixing_vars_start
    #
    # # update_glm_nml_list <- list()
    # update_aed_nml_list <- list()
    # update_phyto_nml_list <- list()
    # # update_glm_nml_names <- c()
    # update_aed_nml_names <- c()
    # update_phyto_nml_names <- c()
    # # list_index <- 1
    # list_index_aed <- 1
    # list_index_phyto <- 1



    diagnostics <- array(NA, dim = c(length(diagnostics_names), ndepths_modeled))

    x_star_end <- rep(NA, nstates)

    if(npars > 0){

      unique_pars <- unique(par_names)

      for(par in 1:length(unique_pars)){

        curr_par_set <- which(par_names == unique_pars[par])
        curr_nml <- par_file[curr_par_set[1]]
        input_yaml_multiple(ler_yaml, key1 = "model_parameters", key2 = model, key3 = unique_pars[par], value = signif(curr_pars[curr_par_set], 4))

      }
    }



    # if(include_wq){
    #
    #   wq_init_vals <- c()
    #
    #   for(wq in 1:num_wq_vars){
    #     wq_enkf_tmp <- x_start[wq_start[wq + 1]:wq_end[wq + 1]]
    #     wq_enkf_tmp[wq_enkf_tmp < 0] <- 0
    #     wq_init_vals <- c(wq_init_vals,
    #                       approx(modeled_depths,wq_enkf_tmp, model_depths_mid, rule = 2)$y)
    #   }
    #   update_glm_nml_list[[list_index]] <- round(wq_init_vals, 4)
    #   update_glm_nml_names[list_index] <- "wq_init_vals"
    #   list_index <- list_index + 1
    #
    #   if(simulate_sss){
    #     if(is.na(management$specified_sss_inflow_file)){
    #       flare:::create_sss_input_output(x = x_start,
    #                                       i,
    #                                       m,
    #                                       full_time_local,
    #                                       working_directory,
    #                                       wq_start,
    #                                       management$management_input,
    #                                       hist_days,
    #                                       management$forecast_sss_on,
    #                                       management$sss_depth,
    #                                       management$use_specified_sss,
    #                                       state_names,
    #                                       modeled_depths = modeled_depths,
    #                                       forecast_sss_flow = management$forecast_sss_flow,
    #                                       forecast_sss_oxy = management$forecast_sss_oxy,
    #                                       salt = salt_start)
    #     }else{
    #       file.copy(file.path(working_directory, management$specified_sss_inflow_file), paste0(working_directory,"/sss_inflow.csv"))
    #       if(!is.na(management$specified_sss_outflow_file)){
    #         file.copy(file.path(working_directory, management$specified_sss_outflow_file), paste0(working_directory,"/sss_outflow.csv"))
    #       }
    #     }
    #   }
    # }






  }


  # Initial temperature
  the_temps_enkf_tmp <- x_start[1:ndepths_modeled]

  the_temps <- approx(modeled_depths,the_temps_enkf_tmp, model_depths_mid, rule = 2)$y
  the_sals <- approx(modeled_depths,salt_start, model_depths_mid, rule = 2)$y

  init_prof <- data.frame(Depth_meter = round(model_depths_tmp, 4),
                          Water_Temperature_celsius = round(the_temps, 4))
  write.csv(init_prof, file.path(working_directory, "initial_profile.csv"),
            row.names = FALSE, quote = FALSE)

  #ALLOWS THE LOOPING THROUGH NOAA ENSEMBLES
  input_yaml_multiple(ler_yaml, key1 = "input", key2 = "init_temp_profile", key3 = "file", value = "initial_profile.csv")
  input_yaml_multiple(ler_yaml, key1 = "input", key2 = "meteo", key3 = "file", value = basename(curr_met_file))
  gotmtools::input_yaml(ler_yaml, label = "inflows", key = "file", value = basename(unlist(inflow_file_name)))
  # input_yaml_multiple(ler_yaml, key1 = "inflows", key2 = "file", value = basename(unlist(inflow_file_name)))
  input_yaml_multiple(ler_yaml, key1 = "time", key2 = "start", value = curr_start)
  input_yaml_multiple(ler_yaml, key1 = "time", key2 = "stop", value = curr_stop)



  # yml$input$init_temp_profile$file <- "initial_profile.csv"
  # yml$input$meteo$file <- basename(curr_met_file)
  # yml$inflows$file <- basename(unlist(inflow_file_name))
  # yml$time$start <- curr_start
  # yml$time$stop <- curr_stop
  #
  # yaml::write_yaml(yml, file.path(working_directory, ler_yaml)) #


  # No outflows in LER yet!!!
  # update_glm_nml_list[[list_index]] <- unlist(outflow_file_name)
  # update_glm_nml_names[list_index] <- "outflow_fl"
  # list_index <- list_index + 1

  # load("../flare_lake_examples/fcre/.RData")
  # library(LakeEnsemblR)


  # gotmtools::get_yaml_value(ler_yaml, label = "inflows", key = "use")
  # get_yaml_multiple(ler_yaml, key1 = "inflows", key2 = "use")
  # get_yaml_multiple(ler_yaml, key1 = "inflows", key2 = "file")

  LakeEnsemblR::export_config(config_file = ler_yaml, model = model, dirs = FALSE,
                              time = TRUE, location = TRUE, output_settings = TRUE,
                              meteo = T, init_cond = TRUE, extinction = FALSE,
                              inflow = T, # INFLOWS SWITCHED OFF!
                              model_parameters = TRUE,
                              folder = working_directory)


  if(model == "GLM" & include_wq){
    flare:::update_nml(update_aed_nml_list,
                       update_aed_nml_names,
                       working_directory,
                       "aed2.nml")
  }

  if(model == "GLM" & include_wq){
    flare:::update_nml(update_phyto_nml_list,
                       update_phyto_nml_names,
                       working_directory,
                       "aed2_phyto_pars.nml")
  }

  #if(ncol(as.matrix(inflow_file_names)) == 2){
  #  tmp <- file.copy(from = inflow_file_names[inflow_outflow_index, 1],
  #                   to = "inflow_file1.csv", overwrite = TRUE)
  #  tmp <- file.copy(from = inflow_file_names[inflow_outflow_index, 2],
  #                   to = "inflow_file2.csv", overwrite = TRUE)
  #}else{
  #  tmp <- file.copy(from = inflow_file_names[inflow_outflow_index],
  #                   to = "inflow_file1.csv", overwrite = TRUE)
  #}
  #tmp <- file.copy(from = outflow_file_names[inflow_outflow_index],
  #                 to = "outflow_file1.csv", overwrite = TRUE)

  #Use GLM NML files to run GLM for a day
  # Only allow simulations without NaN values in the output to proceed.
  #Necessary due to random Nan in AED output
  pass <- FALSE
  num_reruns <- 0
  setwd(working_directory)

  if(i == 2 & m == 1){
    file.copy(from = paste0(working_directory, "/", "LakeEnsemblR.yaml"), #GLM SPECIFIC
              to = paste0(working_directory, "/", "LakeEnsemblR_initial.yaml"),
              overwrite = TRUE) #GLM SPECIFIC
  }

  # From LakeEnsemblR::run_ensemble()
  time_step <- gotmtools::get_yaml_value(ler_yaml, "output", "time_step")
  time_unit <- gotmtools::get_yaml_value(ler_yaml, "output", "time_unit")
  if(time_unit == "second"){
    # Needed to create out_time vector
    time_unit <- "sec"
  }
  # Create output time vector
  out_time <- data.frame(datetime = seq.POSIXt(as.POSIXct(curr_start),
                                               as.POSIXct(curr_stop),
                                               by = paste(time_step, time_unit)))
  out_vars <- gotmtools::get_yaml_value(ler_yaml, "output", "variables")
  # run_model_args <- list(config_file = ler_yaml,
  #                        folder = working_directory,
  #                        return_list = TRUE,
  #                        create_output = FALSE,
  #                        start = start,
  #                        stop = stop,
  #                        verbose = FALSE,
  #                        obs_deps = modeled_depths,
  #                        out_time = out_time,
  #                        out_vars = out_vars)

  # Create output folder
  dir.create(file.path(working_directory, model, "output"), showWarnings = FALSE)

  while(!pass){

    #Delete previous output
    old_output <- list.files(file.path(working_directory, model, "output"))
    unlink(file.path(working_directory, model, "output", old_output), recursive = TRUE)

    model_states <- flare:::run_models_LER(model = model,
                          config_file = ler_yaml,
                          folder = working_directory,
                          return_list = TRUE,
                          create_output = FALSE,
                          start = start,
                          stop = stop,
                          verbose = FALSE,
                          obs_deps = modeled_depths,
                          out_time = out_time,
                          out_vars = out_vars,
                          local_tzone = config$local_tzone)
    # glmtools::plot_var(file = "GLM/output/output.nc", var_name = "temp")


    # if(machine == "unix"){
    #   system2(paste0(working_directory, "/", "glm_linux"),
    #           stdout = FALSE,
    #           stderr = FALSE,
    #           env = paste0("DYLD_LIBRARY_PATH=",working_directory))
    # }else if(machine == "mac"){
    #   system2(paste0(working_directory, "/", "glm"),
    #           stdout = FALSE,
    #           stderr = FALSE,
    #           env = paste0("DYLD_LIBRARY_PATH=",working_directory))
    # }else if(machine == "windows"){
    #   GLM3r::run_glm()
    #   # glmtools::plot_temp()
    #   # system2(paste0(working_directory, "/", "glm.exe"),
    #   #         invisible = FALSE)
    # }else{
    #   print("Machine not identified")
    #   stop()
    # }
    fils <- list.files(file.path(working_directory, model, "output"))

    if(length(fils) != 0){ #&
       # !testit::has_error(nc <- ncdf4::nc_open(paste0(working_directory, "/output/output.nc")))){

      # if(length(ncvar_get(nc, "time")) >= 1){
      if(TRUE){
          # nc_close(nc)

        output_vars_multi_depth <- state_names
        output_vars_no_depth <- NA

        # if( model == "GLM") {
        #   GLM_temp_wq_out <-  get_glm_nc_var_all_wq(ncFile = "/GLM/output/output.nc",
        #                                             working_dir = working_directory,
        #                                             z_out = modeled_depths,
        #                                             vars_depth = output_vars_multi_depth,
        #                                             vars_no_depth = output_vars_no_depth,
        #                                             diagnostic_vars = diagnostics_names)
        # }


        ler_temp_out <-  flare:::get_ler_nc_var_all(model = model,
                                                  working_dir = working_directory,
                                                  z_out = modeled_depths,
                                                  vars_depth = output_vars_multi_depth,
                                                  vars_no_depth = output_vars_no_depth,
                                                  diagnostic_vars = diagnostics_names)


        num_model_depths <- length(ler_temp_out$depths_enkf)
        temps <- rev(ler_temp_out$output[ ,1])
        model_depths_end[1:num_model_depths] <- ler_temp_out$depths_enkf

        model_depths_tmp <- c(ler_temp_out$depths_enkf, ler_temp_out$lake_depth)

        model_depths_mid <- model_depths_tmp[1:(length(model_depths_tmp)-1)] + diff(model_depths_tmp)/2


        x_star_end[1:ndepths_modeled] <- approx(model_depths_mid, temps,
                                                modeled_depths, rule = 2)$y
        # x_star_end[1:ndepths_modeled] <- approx(LER_temp_out$depths, LER_temp_out$temp,
        #                                         modeled_depths, rule = 2)$y

        salt_end <- approx(model_depths_mid, ler_temp_out$salt, modeled_depths, rule = 2)$y

        if(include_wq){
          for(wq in 1:num_wq_vars){
            glm_wq <-  rev(ler_temp_out$output[ ,1+wq])
            x_star_end[wq_start[1 + wq]:wq_end[1 + wq]] <- approx(model_depths_mid,glm_wq, modeled_depths, rule = 2)$y
          }
        }

        if(length(diagnostics_names) > 0){
          for(wq in 1:length(diagnostics_names)){
            glm_wq <-  rev(ler_temp_out$diagnostics_output[ , wq])
            diagnostics[wq , ] <- approx(model_depths_mid,glm_wq, modeled_depths, rule = 2)$y
          }
        } else {
          diagnostics <- rep(NA, length(modeled_depths))
        }

        if(length(which(is.na(x_star_end))) == 0){
          pass = TRUE
        }else{
          num_reruns <- num_reruns + 1
        }
      }else{
        num_reruns <- num_reruns + 1
      }
    }else{
      num_reruns <- num_reruns + 1
    }
    if(num_reruns > 1000){
      stop(paste0("Too many re-runs (> 1000) due to NaN values in output"))
    }

    return(list(x_star_end  = x_star_end,
                lake_depth_end  = ler_temp_out$lake_depth,
                snow_ice_thickness_end  = ler_temp_out$snow_wice_bice,
                avg_surf_temp_end  = ler_temp_out$avg_surf_temp,
                mixing_vars_end = ler_temp_out$mixing_vars,
                salt_end = salt_end,
                diagnostics_end  = diagnostics,
                model_internal_depths  = model_depths_end))
  }
}
