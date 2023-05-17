test_location <- "/Users/quinn/Dropbox/Research/SSC_forecasting/flare_test_suite/"
forecast_location <- test_location
data_location <- file.path(test_location, "input_data")
qaqc_data_location <- file.path(test_location, "input_data")
noaa_forecast_path <- data_location

source(file.path(forecast_location,"forecast_inflow_outflows.R"))
config <- yaml::read_yaml(file.path(forecast_location, "configure_flare.yml"))
run_config <- yaml::read_yaml(file.path(forecast_location, "run_configuration.yml"))

config$run_config <- run_config
config$run_config$forecast_location <- forecast_location
config$data_location <- data_location
config$qaqc_data_location <- qaqc_data_location

# Set up timings
start_datetime_local <- lubridate::as_datetime(paste0(config$run_config$start_day_local," ",config$run_config$start_time_local), tz = config$local_tzone)
if(is.na(config$run_config$forecast_start_day_local)){
  end_datetime_local <- lubridate::as_datetime(paste0(config$run_config$end_day_local," ",config$run_config$start_time_local), tz = config$local_tzone)
  forecast_start_datetime_local <- end_datetime_local
}else{
  forecast_start_datetime_local <- lubridate::as_datetime(paste0(config$run_config$forecast_start_day_local," ",config$run_config$start_time_local), tz = config$local_tzone)
  end_datetime_local <- forecast_start_datetime_local + lubridate::days(config$run_config$forecast_horizon)
}

#Weather Drivers
start_datetime_UTC <-  lubridate::with_tz(start_datetime_local, tzone = "UTC")
end_datetime_UTC <-  lubridate::with_tz(end_datetime_local, tzone = "UTC")
forecast_start_datetime_UTC <- lubridate::with_tz(forecast_start_datetime_local, tzone = "UTC")
forecast_hour <- lubridate::hour(forecast_start_datetime_UTC)


message("Forecasting inflow and outflows")

# Forecast Inflows

forecast_files <- list.files(noaa_forecast_path, pattern = "NOAAGEFS_1hr", full.names = TRUE)
forecast_inflows_outflows(inflow_obs = file.path(config$file_path$qaqc_data_directory, "fcre-targets-inflow.csv"),
                          forecast_files = forecast_files,
                          obs_met_file = file.path(config$file_path$qaqc_data_directory,"observed-met_fcre.nc"),
                          output_dir = config$file_path$inflow_directory,
                          inflow_model = config$inflow$forecast_inflow_model,
                          inflow_process_uncertainty = FALSE,
                          forecast_location = config$file_path$forecast_output_directory,
                          config = config,
                          use_s3 = config$run_config$use_s3,
                          bucket = "drivers",
                          model_name = config$model_settings$model_name)
