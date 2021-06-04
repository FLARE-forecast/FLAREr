
template_folder <- system.file("data", package= "FLAREr")
temp_dir <- tempdir()
# dir.create("example")
file.copy(from = template_folder, to = temp_dir, recursive = TRUE)

test_location <- file.path(temp_dir, "data")

forecast_location <- test_location
execute_location <- file.path(test_location, "output")
data_location <- file.path(test_location, "input_data")
qaqc_data_location <- file.path(test_location, "input_data")

##### Read configuration files
config <- yaml::read_yaml(file.path(forecast_location,"configure_flare.yml"))
config$ensemble_size <- 500 # Increase no. of particles
run_config <- yaml::read_yaml(file.path(forecast_location,"run_configuration.yml"))

config$run_config <- run_config
config$run_config$forecast_location <- forecast_location
config$run_config$execute_location <- execute_location

if(!dir.exists(config$run_config$execute_location)){
  dir.create(config$run_config$execute_location)
}

file.copy(file.path(forecast_location, "glm3.nml"), execute_location)

config$data_location <- data_location
config$qaqc_data_location <- qaqc_data_location

pars_config <- readr::read_csv(file.path(config$run_config$forecast_location, config$par_file), col_types = readr::cols())
obs_config <- readr::read_csv(file.path(config$run_config$forecast_location, config$obs_config_file), col_types = readr::cols())
states_config <- readr::read_csv(file.path(config$run_config$forecast_location,config$states_config_file), col_types = readr::cols())

#Download and process observations (already done)

cleaned_observations_file_long <- file.path(config$qaqc_data_location,"observations_postQAQC_long.csv")
cleaned_inflow_file <- file.path(config$qaqc_data_location, "/inflow_postQAQC.csv")
observed_met_file <- file.path(config$qaqc_data_location,"observed-met_fcre.nc")

#Weather Drivers

met_out <- FLAREr::generate_glm_met_files(obs_met_file = observed_met_file,
                                         out_dir = config$run_config$execute_location,
                                         forecast_dir = file.path(config$data_location, config$forecast_met_model),
                                         config)
met_file_names <- met_out$filenames

historical_met_error <- met_out$historical_met_error

suppressMessages({
  inflow_forecast_path <- file.path(config$data_location, config$forecast_inflow_model)

  #### NEED A TEST HERE TO CHECK THAT INFLOW FILES ARE GENERATED AND CORRECT
  inflow_outflow_files <- FLAREr::create_glm_inflow_outflow_files(inflow_file_dir = inflow_forecast_path,
                                                                 inflow_obs = cleaned_inflow_file,
                                                                 working_directory = config$run_config$execute_location,
                                                                 config,
                                                                 state_names = NULL)
})

inflow_file_names <- inflow_outflow_files$inflow_file_name
outflow_file_names <- inflow_outflow_files$outflow_file_name

obs_tmp <- read.csv(cleaned_observations_file_long)
write.csv(obs_tmp, cleaned_observations_file_long, row.names = FALSE, quote = FALSE)

obs <- FLAREr::create_obs_matrix(cleaned_observations_file_long,
                                obs_config,
                                config)

states_config <- FLAREr::generate_states_to_obs_mapping(states_config, obs_config)
config_file_location <- config$run_config$forecast_location

model_sd <- FLAREr::initiate_model_error(config, states_config, config_file_location)

init <- FLAREr::generate_initial_conditions(states_config,
                                           obs_config,
                                           pars_config,
                                           obs,
                                           config)
