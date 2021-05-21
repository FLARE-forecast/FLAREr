
template_folder <- system.file("data", package= "FLAREr")
temp_dir <- tempdir()
# dir.create("example")
file.copy(from = template_folder, to = temp_dir, recursive = TRUE)

test_location <- file.path(temp_dir, "data")
# print(list.files(test_location))
# print(readLines(file.path(test_location, "test_met_prep.R")))

forecast_location <- test_location
execute_location <- file.path(test_location, "output")
data_location <- file.path(test_location, "input_data")
qaqc_data_location <- file.path(test_location, "input_data")

##### Read configuration files
config <- yaml::read_yaml(file.path(forecast_location,"configure_flare.yml"))
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
