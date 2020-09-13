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
                      full_time_local,
                      wq_start,
                      wq_end,
                      management = NULL,
                      hist_days,
                      modeled_depths,
                      ndepths_modeled,
                      curr_met_file,
                      inflow_file_names,
                      inflow_outflow_index,
                      outflow_file_names,
                      glm_output_vars,
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

  x_star_end <- rep(NA, nstates)

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
      }else if(curr_nml == "aed2_phyto_pars.nml"){
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

    for(wq in 1:num_wq_vars){
      wq_enkf_tmp <- x_start[wq_start[wq + 1]:wq_end[wq + 1]]
      wq_enkf_tmp[wq_enkf_tmp < 0] <- 0
      wq_init_vals <- c(wq_init_vals,
                        approx(modeled_depths,wq_enkf_tmp, glm_depths_mid, rule = 2)$y)
    }
    update_glm_nml_list[[list_index]] <- round(wq_init_vals, 4)
    update_glm_nml_names[list_index] <- "wq_init_vals"
    list_index <- list_index + 1

    if(simulate_sss){
      if(is.na(management$specified_sss_inflow_file)){
        flare:::create_sss_input_output(x_start,
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
                                        include_wq,
                                        modeled_depths = modeled_depths,
                                        forecast_sss_flow = management$forecast_sss_flow,
                                        forecast_sss_oxy = management$forecast_sss_oxy)
      }else{
        file.copy(file.path(working_directory, management$specified_sss_inflow_file), paste0(working_directory,"/sss_inflow.csv"))
        if(!is.na(management$specified_sss_outflow_file)){
          file.copy(file.path(working_directory, management$specified_sss_outflow_file), paste0(working_directory,"/sss_outflow.csv"))
        }
      }
    }
  }

  the_temps_enkf_tmp <- x_start[1:ndepths_modeled]

  the_temps_glm <- approx(modeled_depths,the_temps_enkf_tmp, glm_depths_mid, rule = 2)$y

  update_glm_nml_list[[list_index]] <- round(the_temps_glm, 4)
  update_glm_nml_names[list_index] <- "the_temps"
  list_index <- list_index + 1

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

  flare:::update_nml(update_glm_nml_list,
                     update_glm_nml_names,
                     working_directory,
                     "glm3.nml")

  if(list_index_aed > 1){
    flare:::update_nml(update_aed_nml_list,
                       update_aed_nml_names,
                       working_directory,
                       "aed2.nml")
  }

  if(list_index_phyto > 1){
    flare:::update_nml(update_phyto_nml_list,
                       update_phyto_nml_names,
                       working_directory,
                       "aed2_phyto_pars.nml")
  }



  if(ncol(as.matrix(inflow_file_names)) == 2){
    tmp <- file.copy(from = inflow_file_names[inflow_outflow_index, 1],
                     to = "inflow_file1.csv", overwrite = TRUE)
    tmp <- file.copy(from = inflow_file_names[inflow_outflow_index, 2],
                     to = "inflow_file2.csv", overwrite = TRUE)
  }else{
    tmp <- file.copy(from = inflow_file_names[inflow_outflow_index],
                     to = "inflow_file1.csv", overwrite = TRUE)
  }
  tmp <- file.copy(from = outflow_file_names[inflow_outflow_index],
                   to = "outflow_file1.csv", overwrite = TRUE)

  #Use GLM NML files to run GLM for a day
  # Only allow simulations without NaN values in the output to proceed.
  #Necessary due to random Nan in AED output
  pass <- FALSE
  num_reruns <- 0

  if(i == 2 & m == 1){
    file.copy(from = paste0(working_directory, "/", "glm3.nml"), #GLM SPECIFIC
              to = paste0(working_directory, "/", "glm3_initial.nml"),
              overwrite = TRUE) #GLM SPECIFIC
  }

  while(!pass){
    unlink(paste0(working_directory, "/output.nc"))

    if(machine == "unix"){
      system2(paste0(working_directory, "/", "glm_linux"),
              stdout = FALSE,
              stderr = FALSE,
              env = paste0("DYLD_LIBRARY_PATH=",working_directory))
    }else if(machine == "mac"){
      system2(paste0(working_directory, "/", "glm"),
              stdout = FALSE,
              stderr = FALSE,
              env = paste0("DYLD_LIBRARY_PATH=",working_directory))
    }else if(machine == "windows"){
      system2(paste0(working_directory, "/", "glm.exe"),
              invisible = FALSE)
    }else{
      print("Machine not identified")
      stop()
    }

    if(file.exists(paste0(working_directory, "/output.nc")) &
       !testit::has_error(nc <- ncdf4::nc_open(paste0(working_directory, "/output.nc")))){

      if(length(ncvar_get(nc, "time")) >= 1){
        nc_close(nc)

        output_vars_multi_depth <- state_names
        output_vars_no_depth <- NA

        GLM_temp_wq_out <-  flare:::get_glm_nc_var_all_wq(ncFile = "/output.nc",
                                                          working_dir = working_directory,
                                                          z_out = modeled_depths,
                                                          vars_depth = output_vars_multi_depth,
                                                          vars_no_depth = output_vars_no_depth,
                                                          diagnostic_vars = diagnostics_names)

        num_glm_depths <- length(GLM_temp_wq_out$depths_enkf)
        glm_temps <- rev(GLM_temp_wq_out$output[ ,1])
        glm_depths_end[1:num_glm_depths] <- GLM_temp_wq_out$depths_enkf

        glm_depths_tmp <- c(GLM_temp_wq_out$depths_enkf,GLM_temp_wq_out$lake_depth)

        glm_depths_mid <- glm_depths_tmp[1:(length(glm_depths_tmp)-1)] + diff(glm_depths_tmp)/2


        x_star_end[1:ndepths_modeled] <- approx(glm_depths_mid,glm_temps, modeled_depths, rule = 2)$y

        salt_end <- approx(glm_depths_mid, GLM_temp_wq_out$salt_end, modeled_depths, rule = 2)$y

        if(include_wq){
          for(wq in 1:num_wq_vars){
            glm_wq <-  rev(GLM_temp_wq_out$output[ ,1+wq])
            x_star_end[wq_start[1 + wq]:wq_end[1 + wq]] <- approx(glm_depths_mid,glm_wq, modeled_depths, rule = 2)$y
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
                lake_depth_end  = GLM_temp_wq_out$lake_depth,
                snow_ice_thickness_end  = GLM_temp_wq_out$snow_wice_bice,
                avg_surf_temp_end  = GLM_temp_wq_out$avg_surf_temp,
                mixing_vars_end = GLM_temp_wq_out$mixing_vars,
                salt_end = salt_end,
                diagnostics_end  = diagnostics,
                model_internal_depths  = glm_depths_end))
  }
}

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

set_up_model <- function(executable_location,
                         config,
                         working_directory,
                         num_wq_vars){

  switch(Sys.info() [["sysname"]],
         Linux = { machine <- "unix" },
         Darwin = { machine <- "mac" },
         Windows = { machine <- "windows"})


  GLM_folder <- executable_location
  fl <- c(list.files(GLM_folder, full.names = TRUE))
  tmp <- file.copy(from = fl, to = working_directory, overwrite = TRUE)

  file.copy(from = file.path(config$run_config$forecast_location, config$base_GLM_nml),
            to = paste0(working_directory, "/", "glm3.nml"), overwrite = TRUE)

  update_var(num_wq_vars, "num_wq_vars", working_directory, "glm3.nml") #GLM SPECIFIC


  if(config$include_wq){

    file.copy(from =  file.path(config$run_config$forecast_location,config$base_AED_nml),
              to = paste0(working_directory, "/", "aed2.nml"), overwrite = TRUE)

    file.copy(from =  file.path(config$run_config$forecast_location,config$base_AED_phyto_pars_nml),
              to = paste0(working_directory, "/", "aed2_phyto_pars.nml"), overwrite = TRUE)

    file.copy(from =  file.path(config$run_config$forecast_location,config$base_AED_zoop_pars_nml),
              to = paste0(working_directory, "/", "aed2_zoop_pars.nml"), overwrite = TRUE)

  }


  update_var(length(config$modeled_depths), "num_depths", working_directory, "glm3.nml") #GLM SPECIFIC

  update_var(config$modeled_depths, "the_depths", working_directory, "glm3.nml") #GLM SPECIFIC

  #Create a copy of the NML to record starting initial conditions
  file.copy(from = paste0(working_directory, "/", "glm3.nml"), #GLM SPECIFIC
            to = paste0(working_directory, "/", "glm3_initial.nml"), overwrite = TRUE) #GLM SPECIFIC
}

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
#'
#'
create_sss_input_output <- function(x,
                                    i,
                                    m,
                                    full_time_local,
                                    working_directory,
                                    wq_start,
                                    management_input,
                                    hist_days,
                                    forecast_sss_on,
                                    sss_depth,
                                    use_specified_sss,
                                    state_names,
                                    include_wq,
                                    modeled_depths,
                                    forecast_sss_flow,
                                    forecast_sss_oxy){

  full_time_day_local <- lubridate::as_date(full_time_local)

  potential_names <- c("OXY_oxy",
                       "SIL_rsi",
                       "NIT_amm",
                       "NIT_nit",
                       "PHS_frp",
                       "OGM_doc",
                       "OGM_docr",
                       "OGM_poc",
                       "OGM_don",
                       "OGM_donr",
                       "OGM_pon",
                       "OGM_dop",
                       "OGM_dopr",
                       "OGM_pop")


  sss_oxy_factor <- 1.0

  depth_index <- which.min(abs(modeled_depths - sss_depth))

  if(include_wq){
    wq_names_tmp <- state_names[which(state_names %in% potential_names)]
  }else{
    wq_names_tmp <- NULL
  }

  time_sss <- c(full_time_day_local[i - 1],full_time_day_local[i])
  if(i > (hist_days + 1)){
    if(forecast_sss_on){
      if(use_specified_sss){
        FLOW1 <- management_input[i-1, 1]
        OXY1 <- management_input[i-1, 2]  * sss_oxy_factor
      }else{
        FLOW1 <- forecast_SSS_flow * (1/(60*60*24))
        OXY1 <- forecast_SSS_Oxy * sss_oxy_factor
      }
    }else{
      FLOW1 <- 0.0
      OXY1 <-  0.0
    }
  }else{
    FLOW1 <- management_input[i-1, 1]
    OXY1 <-  management_input[i-1, 2]  * sss_oxy_factor
  }

  if(i > (hist_days + 1)){
    if(forecast_sss_on){
      if(use_specified_sss){
        FLOW2 <- management_input[i, 1]
        OXY2 <- management_input[i, 2]  * sss_oxy_factor
      }else{
        FLOW2 <- forecast_SSS_flow * (1/(60*60*24))
        OXY2 <- forecast_SSS_Oxy * sss_oxy_factor
      }
    }else{
      FLOW2 <- 0.0
      OXY2 <- 0.0
    }
  }else{
    FLOW2 <- management_input[i, 1]
    OXY2 <- management_input[i, 2]  * sss_oxy_factor
  }

  FLOW <- round(c(FLOW1, FLOW2), 5)
  TEMP <- round(rep(x[depth_index],2), 3)
  SALT <- rep(0,2)

  #OXY_EQ <- Eq.Ox.conc(TEMP[1], elevation.m = 506,
  #           bar.press = NULL, bar.units = NULL,
  #           out.DO.meas = "mg/L",
  #           salinity = 0, salinity.units = "pp.thou")*1000*(1/32)

  #if(OXY1 > OXY_EQ){OXY1 = OXY_EQ}
  #if(OXY2 > OXY_EQ){OXY2 = OXY_EQ}

  OXY_oxy <- round(c(OXY1, OXY2), 3)

  if(length(which(state_names != "OXY_oxy")) == 1){
    sss_inflow <- data.frame(time = time_sss, FLOW = FLOW, TEMP = TEMP, SALT = SALT, OXY_oxy = OXY_oxy)
  }else{

    NIT_amm <- round(rep(x[wq_start[which(state_names == "NIT_amm")] + depth_index - 1],2), 3)
    NIT_nit <- round(rep(x[wq_start[which(state_names == "NIT_nit")] + depth_index - 1],2), 3)
    PHS_frp <- round(rep(x[wq_start[which(state_names == "PHS_frp")] + depth_index - 1],2), 3)
    OGM_doc <- round(rep(x[wq_start[which(state_names == "OGM_doc")] + depth_index - 1],2), 3)
    OGM_docr <- round(rep(x[wq_start[which(state_names == "OGM_docr")] + depth_index - 1],2), 3)
    OGM_poc <- round(rep(x[wq_start[which(state_names == "OGM_poc")] + depth_index - 1],2), 3)
    OGM_don <- round(rep(x[wq_start[which(state_names == "OGM_don")] + depth_index - 1],2), 3)
    OGM_donr <- round(rep(x[wq_start[which(state_names == "OGM_donr")] + depth_index - 1],2), 3)
    OGM_dop <- round(rep(x[wq_start[which(state_names == "OGM_dop")] + depth_index - 1],2), 3)
    OGM_dopr <- round(rep(x[wq_start[which(state_names == "OGM_dop")] + depth_index - 1],2), 3)
    OGM_pop <- round(rep(x[wq_start[which(state_names == "OGM_pop")] + depth_index - 1],2), 3)
    OGM_pon <- round(rep(x[wq_start[which(state_names == "OGM_pon")] + depth_index - 1],2), 3)
    #PHS_frp_ads <- round(rep(x[i-1, m, wq_start[which(state_names == "PHS_frp_ads")-1] + depth_index - 1],2), 3)
    CAR_dic <- round(rep(x[i-1, m, wq_start[which(state_names == "CAR_dic")-1] + depth_index - 1],2), 3)
    CAR_ch4 <- round(rep(x[i-1, m, wq_start[which(state_names == "CAR_ch4")-1] + depth_index - 1],2), 3)
    SIL_rsi <- round(rep(x[wq_start[which(state_names == "SIL_rsi")] + depth_index - 1],2), 3)

    sss_inflow <- data.frame(time = time_sss,
                             FLOW = FLOW,
                             TEMP = TEMP,
                             SALT = SALT,
                             OXY_oxy = OXY_oxy,

                             NIT_amm = NIT_amm,
                             NIT_nit = NIT_nit,
                             PHS_frp = PHS_frp,
                             OGM_doc = OGM_doc,
                             OGM_docr = OGM_docr,
                             OGM_poc = OGM_poc,
                             OGM_don = OGM_don,
                             OGM_donr = OGM_donr,
                             OGM_pon = OGM_pon,
                             OGM_dop = OGM_dop,
                             OGM_dopr = OGM_dopr,
                             OGM_pop = OGM_pop,
                             SIL_rsi = SIL_rsi,
                             #PHS_frp_ads = PHS_frp_ads,
                             CAR_dic = CAR_dic,
                             CAR_ch4 = CAR_ch4,
                             SIL_rsi = SIL_rsi,
    )

    #sss_inflow <- sss_inflow %>%
    #  select(vars(c("FLOW", "TEMP", "SALT", all_of(wq_names_tmp))))
  }



  sss_outflow <- data.frame(time = time_sss, FLOW = FLOW, TEMP = TEMP, SALT = SALT)

  write.csv(sss_inflow, paste0(working_directory, "/sss_inflow.csv"), row.names = FALSE, quote = FALSE)
  write.csv(sss_outflow, paste0(working_directory, "/sss_outflow.csv"), row.names = FALSE, quote = FALSE)
}

