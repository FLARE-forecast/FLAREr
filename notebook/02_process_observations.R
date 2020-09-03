config <- yaml::read_yaml("/Users/quinn/Dropbox/Research/SSC_forecasting/FLARE_package/flare/notebook/configure_flare.yml")
config_run <- yaml::read_yaml("/Users/quinn/Dropbox/Research/SSC_forecasting/FLARE_package/flare/notebook/run_configuration.yml")


source(paste0(config$code_folder_old,"/","Rscripts/",config$lake_name_code,"/in_situ_qaqc.R"))
source(paste0(config$code_folder_old,"/","Rscripts/",config$lake_name_code,"/met_qaqc.R"))
source(paste0(config$code_folder_old,"/","Rscripts/",config$lake_name_code,"/inflow_qaqc.R"))

cleaned_met_file <- paste0(config$qaqc_data_location, "/met_full_postQAQC.csv")
if(is.na(config$met_file)){
  met_qaqc(realtime_file = file.path(config$data_location, config$met_raw_obs_fname[1]),
           qaqc_file = file.path(config$data_location, config$met_raw_obs_fname[2]),
           cleaned_met_file,
           input_file_tz = "EST",
           config$local_tzone,
           full_time_local)
}else{
  file.copy(file.path(config$data_location,config$met_file), cleaned_met_file, overwrite = TRUE)
}

cleaned_inflow_file <- paste0(config$qaqc_data_location, "/inflow_postQAQC.csv")

if(is.na(config$inflow1_file)){
  inflow_qaqc(realtime_file = file.path(config$data_location, config$inflow_raw_file1[1]),
              qaqc_file = file.path(config$data_location, config$inflow_raw_file1[2]),
              nutrients_file = file.path(config$data_location, config$nutrients_fname),
              cleaned_inflow_file ,
              config$local_tzone,
              input_file_tz = 'EST')
}else{
  file.copy(file.path(config$data_location,config$inflow1_file), cleaned_inflow_file, overwrite = TRUE)
}


cleaned_observations_file_long <- paste0(config$qaqc_data_location,
                                         "/observations_postQAQC_long.csv")
if(is.na(config$combined_obs_file)){
  in_situ_qaqc(insitu_obs_fname = file.path(config$data_location,config$insitu_obs_fname),
               data_location = config$data_location,
               maintenance_file = file.path(config$data_location,config$maintenance_file),
               ctd_fname = config$ctd_fname,
               nutrients_fname = config$nutrients_fname,
               cleaned_observations_file_long = cleaned_observations_file_long,
               config$lake_name_code,
               config$code_folder)
}else{
  file.copy(file.path(config$data_location,config$combined_obs_file), cleaned_observations_file_long, overwrite = TRUE)
}

