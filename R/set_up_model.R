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

set_up_model <- function(config,
                         ens_working_directory,
                         state_names,
                         inflow_file_names,
                         outflow_file_names){

  file.copy(from = file.path(config$file_path$configuration_directory, "forecast_model", config$model_settings$model_name, config$model_settings$base_GLM_nml),
            to = file.path(ens_working_directory, "glm3.nml"), overwrite = TRUE)

  non_temp_names <- state_names[which(!(state_names %in% c("temp", "salt")))]

  FLAREr:::update_var(length(non_temp_names), "num_wq_vars", ens_working_directory, "glm3.nml") #GLM SPECIFIC

  if(length(non_temp_names) > 1) {
    FLAREr:::update_var(non_temp_names, "wq_names", ens_working_directory, "glm3.nml")
  }

  if(!is.null(ncol(inflow_file_names))) {
    FLAREr:::update_var(ncol(inflow_file_names), "num_inflows", ens_working_directory, "glm3.nml")
    FLAREr:::update_var(ncol(outflow_file_names), "num_outlet", ens_working_directory, "glm3.nml")
    inflow_var_names <- c("FLOW","TEMP","SALT", non_temp_names)
    FLAREr:::update_var(inflow_var_names, "inflow_vars", ens_working_directory, "glm3.nml")
    FLAREr:::update_var(length(inflow_var_names), "inflow_varnum", ens_working_directory, "glm3.nml")
  }


  if(config$include_wq){

    file.copy(from =  file.path(config$run_config$forecast_location,config$base_AED_nml),
              to = paste0(ens_working_directory, "/", "aed2.nml"), overwrite = TRUE)

    file.copy(from =  file.path(config$run_config$forecast_location,config$base_AED_phyto_pars_nml),
              to = paste0(ens_working_directory, "/", "aed2_phyto_pars.nml"), overwrite = TRUE)

    file.copy(from =  file.path(config$run_config$forecast_location,config$base_AED_zoop_pars_nml),
              to = paste0(ens_working_directory, "/", "aed2_zoop_pars.nml"), overwrite = TRUE)

  }


  FLAREr:::update_var(length(config$modeled_depths), "num_depths", ens_working_directory, "glm3.nml") #GLM SPECIFIC


  inflow_var_names <- c("FLOW","TEMP","SALT", non_temp_names)

  #Create a copy of the NML to record starting initial conditions
  file.copy(from = paste0(ens_working_directory, "/", "glm3.nml"), #GLM SPECIFIC
            to = paste0(ens_working_directory, "/", "glm3_initial.nml"), overwrite = TRUE) #GLM SPECIFIC
}
