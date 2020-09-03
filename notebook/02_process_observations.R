full_time_local <- seq(start_datetime_local, end_datetime_local, by = "1 day")


cleaned_met_file <- paste0(working_directory, "/met_full_postQAQC.csv")
if(is.na(config$met_file)){
  met_qaqc(realtime_file = config$met_raw_obs_fname[1],
           qaqc_file = config$met_raw_obs_fname[2],
           cleaned_met_file,
           input_file_tz = "EST",
           config$local_tzone,
           full_time_local)
}else{
  file.copy(config$met_file, cleaned_met_file)
}

cleaned_inflow_file <- paste0(working_directory, "/inflow_postQAQC.csv")

if(is.na(config$inflow1_file)){
  inflow_qaqc(realtime_file = config$inflow_raw_file1[1],
              qaqc_file = config$inflow_raw_file1[2],
              nutrients_file = config$nutrients_fname,
              cleaned_inflow_file ,
              config$local_tzone,
              input_file_tz = 'EST')
}else{
  file.copy(config$inflow1_file, cleaned_inflow_file)
}


cleaned_observations_file_long <- paste0(working_directory,
                                         "/observations_postQAQC_long.csv")
if(is.na(combined_obs_file)){
  in_situ_qaqc(insitu_obs_fname = config$insitu_obs_fname,
               data_location = config$data_location,
               maintenance_file = config$maintenance_file,
               ctd_fname = config$ctd_fname,
               nutrients_fname = config$nutrients_fname,
               cleaned_observations_file_long = cleaned_observations_file_long,
               config$lake_name_code,
               config$code_folder)
}else{
  file.copy(config$combined_obs_file, cleaned_observations_file_long)
}
