#' Set of model for FLARE simulation
#'
#' @param config list of configurations
#' @param ens_working_directory full path of directory where ensemble member is executed
#' @param state_names vector of state names
#' @param inflow_file_names matrix or vector of inflow file names
#' @param outflow_file_names matrix or vector of outflow file names

#' @noRd
set_up_model <- function(config,
                         ens_working_directory,
                         state_names,
                         inflow_file_names,
                         outflow_file_names){

  file.copy(from = file.path(config$file_path$configuration_directory, config$model_settings$base_GLM_nml),
            to = file.path(ens_working_directory, "glm3.nml"), overwrite = TRUE)

  non_temp_names <- state_names[which(!(state_names %in% c("temp", "salt")))]

  update_var(length(non_temp_names), "num_wq_vars", ens_working_directory, "glm3.nml") #GLM SPECIFIC

  if(length(non_temp_names) > 1) {
    update_var(non_temp_names, "wq_names", ens_working_directory, "glm3.nml")
  }

  if(!is.null(ncol(inflow_file_names))) {
    update_var(ncol(inflow_file_names), "num_inflows", ens_working_directory, "glm3.nml")
    update_var(ncol(outflow_file_names), "num_outlet", ens_working_directory, "glm3.nml")
    inflow_var_names <- c("FLOW","TEMP","SALT", non_temp_names)
    update_var(inflow_var_names, "inflow_vars", ens_working_directory, "glm3.nml")
    update_var(length(inflow_var_names), "inflow_varnum", ens_working_directory, "glm3.nml")
  }


  if(!is.null(config$model_settings$base_AED_phyto_pars_nml)){
    file.copy(from = file.path(config$file_path$configuration_directory, config$model_settings$base_AED_nml),
              to = file.path(ens_working_directory, "aed2.nml"), overwrite = TRUE)
  }

  if(!is.null(config$model_settings$base_AED_phyto_pars_nml)){
    file.copy(from = file.path(config$file_path$configuration_directory, config$model_settings$base_AED_phyto_pars_nml),
              to = file.path(ens_working_directory, "aed_phyto_pars.csv"), overwrite = TRUE)
  }

  if(!is.null(config$model_settings$base_AED_zoop_pars_nml)){
    file.copy(from = file.path(config$file_path$configuration_directory, config$model_settings$base_AED_zoop_pars_nml),
              to = file.path(ens_working_directory, "aed2_zoop_pars.csv"), overwrite = TRUE)
  }

  #update_var(length(config$modeled_depths), "num_depths", ens_working_directory, "glm3.nml") #GLM SPECIFIC


  inflow_var_names <- c("FLOW","TEMP","SALT", non_temp_names)

}
