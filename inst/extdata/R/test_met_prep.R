
template_folder <- system.file("example", package= "FLAREr")
temp_dir <- tempdir()
# dir.create("example")
file.copy(from = template_folder, to = temp_dir, recursive = TRUE)

test_directory <- file.path(temp_dir, "example")
# print(list.files(test_directory))
# print(readLines(file.path(test_directory, "test_met_prep.R")))


lake_directory <- test_directory
configuration_directory <- file.path(lake_directory, "configuration","default")
execute_directory <- file.path(test_directory, "flare_tempdir")
qaqc_data_directory <- file.path(test_directory, "data_processed")
forecast_input_directory <- file.path(test_directory, "forecasted_drivers")

##### Read configuration files
config <- yaml::read_yaml(file.path(configuration_directory,"configure_flare.yml"))
run_config <- yaml::read_yaml(file.path(configuration_directory, "configure_run.yml"))

config$run_config <- run_config
config$file_path$noaa_directory <- file.path(forecast_input_directory, config$met$forecast_met_model)
config$file_path$inflow_directory <- file.path(forecast_input_directory, config$inflow$forecast_inflow_model)
config$file_path$configuration_directory<- configuration_directory
config$file_path$execute_directory <- file.path(lake_directory, "flare_tempdir")
config$file_path$forecast_output_directory <- file.path(test_directory, "forecast_output")
config$file_path$qaqc_data_directory <- file.path(test_directory, "data_processed")

if(!dir.exists(config$file_path$execute_directory)){
  dir.create(config$file_path$execute_directory)
}

file.copy(file.path(configuration_directory, "glm3.nml"), execute_directory)

config$qaqc_data_directory <- qaqc_data_directory

pars_config <- readr::read_csv(file.path(configuration_directory, config$model_settings$par_config_file), col_types = readr::cols())
obs_config <- readr::read_csv(file.path(configuration_directory, config$model_settings$obs_config_file), col_types = readr::cols())
states_config <- readr::read_csv(file.path(configuration_directory, config$model_settings$states_config_file), col_types = readr::cols())

#Download and process observations (already done)

cleaned_observations_file_long <- file.path(config$file_path$qaqc_data_directory,"fcre-targets-insitu.csv")
cleaned_inflow_file <- file.path(config$file_path$qaqc_data_directory, "fcre-targets-inflow.csv")
observed_met_file <- file.path(config$file_path$qaqc_data_directory,"observed-met_fcre.nc")
