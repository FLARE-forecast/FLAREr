#'Export output settings for each model
#'
#'Exports settings related to output (time step, format),
#'  for each model
#'
#'@param config_file name of the master LakeEnsemblR config file
#'@param model vector; model to export configuration file.
#'  Options include c("GOTM", "GLM", "Simstrat", "FLake", "MyLake")
#'@param folder folder
#'@keywords methods
#'@examples
#'
#'@importFrom gotmtools get_yaml_value input_yaml
#'@importFrom glmtools read_nml set_nml write_nml
#'
#'@export
run_models_LER <- function(model, config_file, folder, return_list, create_output, start,
                           stop, verbose, obs_deps, out_time, out_hour, out_vars, local_tzone) {

  if(model == "GLM") {
    #Delete previous output
    old_output <- list.files(file.path(folder, "GLM", "output"))
    unlink(file.path(folder, "GLM", "output", old_output), recursive = TRUE)

    GLM3r::run_glm(sim_folder = file.path(folder, "GLM"), verbose = verbose)
    # glmtools::plot_temp(file.path(folder, "GLM", "output.nc"))

    message("GLM run is complete! ", paste0("[", Sys.time(), "]"))

    if(return_list | create_output) {

      # Extract output
      glm_out <- LakeEnsemblR::get_output(config_file = config_file, model = "GLM",
                                          vars = out_vars, obs_depths = obs_deps,
                                          folder = folder)

      out_time <- format(out_time, format = "%Y-%m-%d %H:%M:%S")
      if(!is.list(glm_out)) {
        glm_out <- merge(glm_out, out_time, by = "datetime", all.y = TRUE)
      } else {
        glm_out <- lapply(seq_len(length(glm_out)), function(x){
          glm_out[[x]][, 1] <- format(glm_out[[x]][, 1], format = "%Y-%m-%d %H:%M:%S")
          df <- merge(glm_out[[x]], out_time, by = 1, all.y = TRUE)
          df[, 1] <- as.POSIXct(df[, 1], tz = local_tzone)
          return(df)
        })
        names(glm_out) <- out_vars # Re-assign names to list
      }

    }
    return(glm_out)
  }

}
