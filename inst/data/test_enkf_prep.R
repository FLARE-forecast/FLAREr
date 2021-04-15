
template_folder <- system.file("data", package= "flare")
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

# Set up timings
start_datetime_local <- lubridate::as_datetime(paste0(config$run_config$start_day_local," ",config$run_config$start_time_local), tz = config$local_tzone)
if(is.na(config$run_config$forecast_start_day_local)){
  end_datetime_local <- lubridate::as_datetime(paste0(config$run_config$end_day_local," ",config$run_config$start_time_local), tz = config$local_tzone)
  forecast_start_datetime_local <- end_datetime_local
}else{
  forecast_start_datetime_local <- lubridate::as_datetime(paste0(config$run_config$forecast_start_day_local," ",config$run_config$start_time_local), tz = config$local_tzone)
  end_datetime_local <- forecast_start_datetime_local + lubridate::days(config$run_config$forecast_horizon)
}

#Download and process observations (already done)

cleaned_observations_file_long <- file.path(config$qaqc_data_location,"observations_postQAQC_long.csv")
cleaned_inflow_file <- file.path(config$qaqc_data_location, "/inflow_postQAQC.csv")
observed_met_file <- file.path(config$qaqc_data_location,"observed-met_fcre.nc")

#Step up Drivers

#Weather Drivers
start_datetime_UTC <-  lubridate::with_tz(start_datetime_local, tzone = "UTC")
end_datetime_UTC <-  lubridate::with_tz(end_datetime_local, tzone = "UTC")
forecast_start_datetime_UTC <- lubridate::with_tz(forecast_start_datetime_local, tzone = "UTC")
forecast_hour <- lubridate::hour(forecast_start_datetime_UTC)
if(forecast_hour < 10){forecast_hour <- paste0("0",forecast_hour)}
forecast_path <- file.path(config$data_location, "NOAAGEFS")

met_out <- flare::generate_glm_met_files(obs_met_file = observed_met_file,
                                         out_dir = config$run_config$execute_location,
                                         forecast_dir = forecast_path,
                                         local_tzone = config$local_tzone,
                                         start_datetime_local = start_datetime_local,
                                         end_datetime_local = end_datetime_local,
                                         forecast_start_datetime = forecast_start_datetime_local,
                                         use_forecasted_met = TRUE)
met_file_names <- met_out$filenames

historical_met_error <- met_out$historical_met_error

suppressMessages({
  inflow_forecast_path <- file.path(config$data_location)

  #### NEED A TEST HERE TO CHECK THAT INFLOW FILES ARE GENERATED AND CORRECT
  inflow_outflow_files <- flare::create_glm_inflow_outflow_files(inflow_file_dir = inflow_forecast_path,
                                                                 inflow_obs = cleaned_inflow_file,
                                                                 working_directory = config$run_config$execute_location,
                                                                 start_datetime_local = start_datetime_local,
                                                                 end_datetime_local = end_datetime_local,
                                                                 forecast_start_datetime_local = forecast_start_datetime_local,
                                                                 use_future_inflow = TRUE,
                                                                 state_names = NULL)
})

inflow_file_names <- inflow_outflow_files$inflow_file_name
outflow_file_names <- inflow_outflow_files$outflow_file_name

obs <- flare::create_obs_matrix(cleaned_observations_file_long,
                                obs_config,
                                start_datetime_local,
                                end_datetime_local,
                                local_tzone = config$local_tzone,
                                modeled_depths = config$modeled_depths)

#Set observations in the "future" to NA
full_time_forecast <- seq(start_datetime_local, end_datetime_local, by = "1 day")
obs[ , which(full_time_forecast > forecast_start_datetime_local), ] <- NA


states_config <- flare::generate_states_to_obs_mapping(states_config, obs_config)
config_file_location <- config$run_config$forecast_location

model_sd <- flare::initiate_model_error(config, states_config, config_file_location)

init <- flare::generate_initial_conditions(states_config,
                                           obs_config,
                                           pars_config,
                                           obs,
                                           config)
