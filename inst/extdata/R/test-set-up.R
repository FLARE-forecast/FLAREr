file.copy(system.file("extdata", package = "FLAREr"), dir, recursive = TRUE)

dir.create(file.path(lake_directory, "configuration", config_set_name), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(lake_directory, "targets"), showWarnings = FALSE) # For QAQC data
dir.create(file.path(lake_directory, "drivers"), showWarnings = FALSE) # Weather and inflow forecasts


file.copy(from = system.file("extdata/targets", package = "FLAREr"), to = lake_directory, recursive = TRUE)
file.copy(from = system.file("extdata/drivers", package = "FLAREr"), to = lake_directory, recursive = TRUE)

if(!dir.exists(file.path(lake_directory, "configuration", config_set_name))){
  stop(paste0("lake_directory is missing the configuration/",config_set_name," directory"))
}

config <- FLAREr:::set_configuration(configure_run_file, lake_directory, config_set_name = config_set_name)

config <- FLAREr:::get_restart_file(config, lake_directory)

if(!is.null(config$model_settings$par_config_file)){
  if(!is.na(config$model_settings$par_config_file)){
    pars_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$par_config_file), col_types = readr::cols())
    if(!setequal(names(pars_config),c("par_names","par_names_save","par_file","par_init","par_init_lowerbound","par_init_upperbound","par_lowerbound","par_upperbound","inflat_pars","perturb_par","par_units", "fix_par"))){
      stop(" par configuraiton file does not have the correct columns")
    }
  }
}

obs_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$obs_config_file), col_types = readr::cols())
states_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$states_config_file), col_types = readr::cols())

if(!"temp" %in% states_config$state_names) stop("missing temp as a state name in states config")
if(!"salt" %in% states_config$state_names) stop("missing salt as a state name in states config")

met_start_datetime <- lubridate::as_datetime(config$run_config$start_datetime)
met_forecast_start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime)

