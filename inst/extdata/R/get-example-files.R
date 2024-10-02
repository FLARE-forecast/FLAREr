dir.create(file.path(lake_directory, "configuration/default"), recursive = TRUE)
dir.create(file.path(lake_directory, "targets")) # For QAQC data
dir.create(file.path(lake_directory, "drivers")) # Weather and inflow forecasts
file.copy(system.file("extdata", "configuration", "default", "configure_flare.yml", package = "FLAREr"), file.path(lake_directory, "configuration", "default", "configure_flare.yml"))
file.copy(system.file("extdata", "configuration", "default", "configure_run.yml", package = "FLAREr"), file.path(lake_directory, "configuration", "default", "configure_run.yml"))
file.copy(system.file("extdata", "configuration", "default", "parameter_calibration_config.csv", package = "FLAREr"), file.path(lake_directory, "configuration", "default", "parameter_calibration_config.csv"))
file.copy(system.file("extdata", "configuration", "default", "states_config.csv", package = "FLAREr"), file.path(lake_directory, "configuration", "default", "states_config.csv"))
file.copy(system.file("extdata", "configuration", "default", "depth_model_sd.csv", package = "FLAREr"), file.path(lake_directory, "configuration", "default", "depth_model_sd.csv"))
file.copy(system.file("extdata", "configuration", "default", "observations_config.csv", package = "FLAREr"), file.path(lake_directory, "configuration", "default", "observations_config.csv"))
file.copy(system.file("extdata", "configuration", "default", "glm3.nml", package = "FLAREr"), file.path(lake_directory, "configuration", "default", "glm3.nml"))
file.copy(from = system.file("extdata/targets", package = "FLAREr"), to = lake_directory, recursive = TRUE)
file.copy(from = system.file("extdata/drivers", package = "FLAREr"), to = lake_directory, recursive = TRUE)
