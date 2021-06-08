
template_folder <- system.file("example", package= "FLAREr")
temp_dir <- tempdir()
# dir.create("example")
file.copy(from = template_folder, to = temp_dir, recursive = TRUE)

test_directory <- file.path(temp_dir, "example")

lake_directory <- test_directory
configuration_directory <- file.path(lake_directory, "configuration")
execute_directory <- file.path(test_directory, "flare_tempdir")
qaqc_data_directory <- file.path(test_directory, "data_processed")
forecast_input_directory <- file.path(test_directory, "forecasted_drivers")

##### Read configuration files
config <- yaml::read_yaml(file.path(configuration_directory, "flarer","configure_flare.yml"))
run_config <- yaml::read_yaml(file.path(configuration_directory, "flarer","configure_run.yml"))

config$run_config <- run_config
config$run_config$lake_directory <- lake_directory
config$run_config$execute_directory <- execute_directory
config$file_path$noaa_directory <- file.path(forecast_input_directory, config$met$forecast_met_model)
config$file_path$inflow_directory <- file.path(forecast_input_directory, config$inflow$forecast_inflow_model)
config$file_path$configuration_directory<- configuration_directory
config$file_path$execute_directory <- file.path(lake_directory, "flare_tempdir")
config$run_config$forecast_output_directory <- file.path(test_directory, "forecast_output")
config$file_path$qaqc_data_directory <- file.path(test_directory, "data_processed")

if(!dir.exists(config$run_config$execute_directory)){
  dir.create(config$run_config$execute_directory)
}

file.copy(file.path(configuration_directory, "forecast_model", "glm", "glm3.nml"), execute_directory)

pars_config <- readr::read_csv(file.path(configuration_directory, "flarer", config$model_settings$par_config_file), col_types = readr::cols())
obs_config <- readr::read_csv(file.path(configuration_directory, "flarer", config$model_settings$obs_config_file), col_types = readr::cols())
states_config <- readr::read_csv(file.path(configuration_directory, "flarer", config$model_settings$states_config_file), col_types = readr::cols())

#Download and process observations (already done)

cleaned_observations_file_long <- file.path(config$file_path$qaqc_data_directory,"observations_postQAQC_long.csv")
cleaned_inflow_file <- file.path(config$file_path$qaqc_data_directory, "/inflow_postQAQC.csv")
observed_met_file <- file.path(config$file_path$qaqc_data_directory,"observed-met_fcre.nc")

#Step up Drivers
met_out <- FLAREr::generate_glm_met_files(obs_met_file = observed_met_file,
                                          out_dir = config$run_config$execute_directory,
                                          forecast_dir = config$file_path$noaa_directory,
                                          config)
met_file_names <- met_out$filenames

historical_met_error <- met_out$historical_met_error

inflow_forecast_path <- config$file_path$inflow_directory

inflow_outflow_files <- FLAREr::create_glm_inflow_outflow_files(inflow_file_dir = inflow_forecast_path,
                                                                inflow_obs = cleaned_inflow_file,
                                                                working_directory = config$run_config$execute_directory,
                                                                config,
                                                                state_names = NULL)

inflow_file_names <- inflow_outflow_files$inflow_file_name
outflow_file_names <- inflow_outflow_files$outflow_file_name

obs <- FLAREr::create_obs_matrix(cleaned_observations_file_long,
                                obs_config,
                                config)

states_config <- FLAREr::generate_states_to_obs_mapping(states_config, obs_config)
config_file_location <- file.path(config$file_path$configuration_directory, "flarer")

model_sd <- FLAREr::initiate_model_error(config, states_config, config_file_location)
init <- FLAREr::generate_initial_conditions(states_config,
                                           obs_config,
                                           pars_config,
                                           obs,
                                           config)
