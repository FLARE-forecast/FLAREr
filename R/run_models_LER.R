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
run_models_LER <- function(model, folder, verbose) {

  if(model == "GLM") {

    GLM3r::run_glm(sim_folder = file.path(folder, "GLM"), verbose = verbose)
    # glmtools::plot_temp(file.path(folder, "GLM", "output.nc"))

    message("GLM run is complete! ", paste0("[", Sys.time(), "]"))
  }

  # GOTM ----
  if(model == "GOTM") {

    GOTMr::run_gotm(sim_folder = file.path(folder, "GOTM"), verbose = verbose)

    message("GOTM run is complete! ", paste0("[", Sys.time(), "]"))
  }

  # Simstrat ----
  if(model == "Simstrat") {

    SimstratR::run_simstrat(sim_folder = file.path(folder, "Simstrat"), par_file = "simstrat.par", verbose = verbose)

    message("Simstrat run is complete! ", paste0("[", Sys.time(), "]"))
  }

}
