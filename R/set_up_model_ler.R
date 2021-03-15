

set_up_model_ler <- function(model,
                         executable_location,
                         config,
                         working_directory,
                         state_names,
                         inflow_file_names,
                         outflow_file_names){

  switch(Sys.info() [["sysname"]],
         Linux = { machine <- "unix" },
         Darwin = { machine <- "mac" },
         Windows = { machine <- "windows"})


  ler_yaml <- "test.yaml"
  # yml <- yaml::read_yaml(file.path(working_directory, ler_yaml))
  ler_directory <- gsub(config$lake_name_code, "", working_directory)

  if(model == "GLM") {

    # GLM_folder <- executable_location
    # fl <- c(list.files(GLM_folder, full.names = TRUE))
    # model_directory <- file.path(working_directory, model)
    # dir.create(model_directory, showWarnings = FALSE)
    # tmp <- file.copy(from = fl, to = model_directory, overwrite = TRUE)

    file.copy(from = file.path(config$run_config$forecast_location, config$base_GLM_nml),
              to = file.path(working_directory, "GLM", "glm3.nml"), overwrite = TRUE)

    non_temp_names <- state_names[which(!(state_names %in% "temp"))]
    inflow_var_names <- c("FLOW","TEMP","SALT", non_temp_names)

    input_yaml_multiple(file = ler_yaml, value = length(non_temp_names), key1 = "model_parameters", key2 = "GLM", key3 = "num_wq_vars")
    input_yaml_multiple(file = ler_yaml, value = non_temp_names, key1 = "model_parameters", key2 = "GLM", key3 = "wq_names")
    input_yaml_multiple(file = ler_yaml, value = ncol(inflow_file_names), key1 = "model_parameters", key2 = "GLM", key3 = "num_inflows")
    input_yaml_multiple(file = ler_yaml, value = ncol(outflow_file_names), key1 = "model_parameters", key2 = "GLM", key3 = "num_outlet")
    input_yaml_multiple(file = ler_yaml, value = config$modeled_depths, key1 = "model_parameters", key2 = "GLM", key3 = "the_depths")
    input_yaml_multiple(file = ler_yaml, value = length(config$modeled_depths), key1 = "model_parameters", key2 = "GLM", key3 = "num_depths")
    input_yaml_multiple(file = ler_yaml, value = length(inflow_var_names), key1 = "model_parameters", key2 = "GLM", key3 = "inflow_varnum")
    input_yaml_multiple(file = ler_yaml, value = inflow_var_names, key1 = "model_parameters", key2 = "GLM", key3 = "inflow_vars")
    input_yaml_multiple(file = ler_yaml, value = FALSE, key1 = "model_parameters", key2 = "GLM", key3 = "include_wq")
    input_yaml_multiple(file = ler_yaml, value = "'output'", key1 = "model_parameters", key2 = "GLM", key3 = "out_dir")
    #
    # yml[["model_parameters"]][["GLM"]][["num_wq_vars"]] <- length(non_temp_names)
    # yml[["model_parameters"]][["GLM"]][["wq_names"]] <- non_temp_names
    # yml[["model_parameters"]][["GLM"]][["num_inflows"]] <- ncol(inflow_file_names)
    # yml[["model_parameters"]][["GLM"]][["num_outlet"]] <- ncol(outflow_file_names)
    # yml[["model_parameters"]][["GLM"]][["the_depths"]] <- config$modeled_depths
    # yml[["model_parameters"]][["GLM"]][["num_depths"]] <- length(config$modeled_depths)
    # yml[["model_parameters"]][["GLM"]][["inflow_vars"]] <- length(config$modeled_depths)
    # yml[["model_parameters"]][["GLM"]][["inflow_varnum"]] <- length(config$modeled_depths)
    # yml[["model_parameters"]][["GLM"]][["include_wq"]] <- length(config$include_wq)
    # yml[["model_parameters"]][["GLM"]][["out_dir"]] <- "'.'"

    if(config$include_wq){

      file.copy(from =  file.path(config$run_config$forecast_location,config$base_AED_nml),
                to = paste0(working_directory, "/", "aed2.nml"), overwrite = TRUE)

      file.copy(from =  file.path(config$run_config$forecast_location,config$base_AED_phyto_pars_nml),
                to = paste0(working_directory, "/", "aed2_phyto_pars.nml"), overwrite = TRUE)

      file.copy(from =  file.path(config$run_config$forecast_location,config$base_AED_zoop_pars_nml),
                to = paste0(working_directory, "/", "aed2_zoop_pars.nml"), overwrite = TRUE)

    }

    #Create a copy of the NML to record starting initial conditions
    # file.copy(from = paste0(working_directory, "/", "glm3.nml"), #GLM SPECIFIC
    #           to = paste0(working_directory, "/", "glm3_initial.nml"), overwrite = TRUE) #GLM SPECIFIC
  }



  # yaml::write_yaml(yml, file.path(working_directory, "test.yaml")) # file.path(working_directory, ler_yaml)


  LakeEnsemblR::export_config(config_file = "test.yaml", model = model, dirs = TRUE,
                              time = FALSE, location = TRUE, output_settings = TRUE,
                              meteo = FALSE, init_cond = FALSE, extinction = TRUE,
                              inflow = FALSE, model_parameters = TRUE,
                              folder = working_directory)


}
