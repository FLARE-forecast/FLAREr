#missing noaa files
#20181221gep_all_00z.csv to 20181229gep_all_00z.csv

#Load required elements



if (!"mvtnorm" %in% installed.packages()) install.packages("mvtnorm")
if (!"ncdf4" %in% installed.packages()) install.packages("ncdf4")
if (!"lubridate" %in% installed.packages()) install.packages("lubridate")
if (!"testit" %in% installed.packages()) install.packages("testit")
if (!"imputeTS" %in% installed.packages()) install.packages("imputeTS")
if (!"tidyverse" %in% installed.packages()) install.packages("tidyverse")
if (!"rMR" %in% installed.packages()) install.packages("rMR")
if (!"patchwork" %in% installed.packages()) install.packages("patchwork")
if (!"EML" %in% installed.packages()) install.packages("EML")
if (!"uuid" %in% installed.packages()) install.packages("uuid")
if (!"EFIstandards" %in% installed.packages()){
  library(devtools)
  install_github("eco4cast/EFIstandards")
}

library(mvtnorm)
#library(ncdf4)
library(lubridate)
library(testit)
library(imputeTS)
library(tidyverse)
library(tools)
library(rMR)
#library(EML)
#library(EFIstandards)

#diskutil partitionDisk $(hdiutil attach -nomount ram://2048000) 1 GPTFormat APFS 'ramdisk' '100%'
#diskutil eject /Volumes/ramdisk

#source(paste0(forecast_location,"/","configure_FLARE.R"))
#source(paste0(code_folder, "/", "Rscripts/visualization_analysis/plotting_general.R"))

restart_file <- NA

sim_name <- "test"
forecast_days <- 0
spin_up_days <- 0

start_day_local <- "2018-07-12"
start_time_local <- "07:00:00"
end_day_local <- "2018-07-15"
forecast_start_day_local <- "2018-07-15"

start_datetime <- as_datetime(paste0(start_day_local," ",start_time_local), tz = local_tzone)
end_datetime <- as_datetime(paste0(end_day_local," ",start_time_local), tz = local_tzone)
forecast_start_datetime <- as_datetime(paste0(forecast_start_day_local," ",start_time_local), tz = local_tzone)

forecast_project_id <- "test"

start_day_local <- as.POSIXct(start_day_local, format = "%Y-%m-%d")
forecast_start_day_local <- as.POSIXct(forecast_start_day_local, format = "%Y-%m-%d")
hist_days <- as.numeric(difftime(as_date(forecast_start_day_local),as_date(start_day_local)))

forecast_sss_on <- FALSE

#source(paste0(code_folder_old,"/","Rscripts/edit_nml_functions.R"))
#source(paste0(code_folder_old,"/","Rscripts/archive_forecast.R"))
#source(paste0(code_folder,"/","R/write_forecast_netcdf.R"))
#source(paste0(code_folder,"/","R/run_EnKF.R"))
source(paste0(code_folder_old,"/","Rscripts/met_downscale/process_downscale_GEFS.R"))
#source(paste0(code_folder_old,"/","Rscripts/glmtools.R"))
#source(paste0(code_folder_old,"/","Rscripts/localization.R"))
#source(paste0(code_folder_old,"/","Rscripts/extract_observations.R"))
source(paste0(code_folder_old,"/","Rscripts/create_obs_met_input.R"))
source(paste0(code_folder_old,"/","Rscripts/create_sss_input_output.R"))
source(paste0(code_folder_old,"/","Rscripts/create_inflow_outflow_file.R"))
source(paste0(code_folder_old,"/","Rscripts/read_sss_files.R"))
#source(paste0(code_folder,"/","R/create_flare_eml.R"))

source(paste0(code_folder_old,"/","Rscripts/",lake_name_code,"/in_situ_qaqc.R"))
source(paste0(code_folder_old,"/","Rscripts/",lake_name_code,"/met_qaqc.R"))
source(paste0(code_folder_old,"/","Rscripts/",lake_name_code,"/inflow_qaqc.R"))

#source(paste0(code_folder,"/","R/define_model_",model_name,".R"))
config <- yaml::read_yaml("/Users/quinn/Dropbox/Research/SSC_forecasting/FLARE_package/flare/notebook/configure_flare.yml")
config$base_GLM_nml  <- file.path(config$forecast_location,config$base_GLM_nml)
config$base_AED_nml  <- file.path(config$forecast_location,config$base_AED_nml)
config$base_AED_phyto_pars_nml  <- file.path(config$forecast_location,config$base_AED_phyto_pars_nml)
config$base_AED_zoop_pars_nml  <- file.path(config$forecast_location,config$base_AED_zoop_pars_nml)
config$sss_fname <- file.path(config$data_location,config$sss_fname)
config$par_file <- file.path(config$forecast_location,config$par_file)
config$obs_config_file <- file.path(config$forecast_location,config$obs_config_file)
config$states_config_file <- file.path(config$forecast_location,config$states_config_file)
config$downscaling_coeff <- file.path(config$data_location, downscaling_coeff)


####################################################
#### DETECT PLATFORM
####################################################

switch(Sys.info() [["sysname"]],
       Linux = { config$machine <- "unix" },
       Darwin = { config$machine <- "mac" },
       Windows = { config$machine <- "windows"})


#INSTALL libnetcdf
#if(machine == "unix") {
#  system("if [ $(dpkg-query -W -f='${Status}' libnetcdf-dev 2>/dev/null | grep -c 'ok installed') -eq 0 ]; then sudo apt update && sudo apt install libnetcdf-dev; fi;")
#  Sys.setenv(LD_LIBRARY_PATH=paste("../glm/unix/", Sys.getenv("LD_LIBRARY_PATH"),sep=":"))
#}

####################################################
#### STEP 1: CREATE TIME VECTORS
####################################################

total_days <- hist_days + forecast_days
start_forecast_step <- hist_days + 1

start_datetime_local <- as_datetime(paste0(start_day_local," ",start_time_local), tz = local_tzone)
end_datetime_local <- start_datetime_local + total_days*24*60*60
forecast_start_time_local <- start_datetime_local + hist_days*24*60*60

full_time_local <- seq(start_datetime_local, end_datetime_local, by = "1 day")

####################################################
#### STEP 2: SET ARRAY DIMENSIONS
####################################################

if(!is.na(config$par_file)){
  config$pars_config <- read_csv(par_file, col_types = cols())
  npars <- nrow( config$pars_config)
}else{
  npars <- 0
}

config$obs_config <- read_csv(obs_config_file, col_types = cols())

config$states_config <- read_csv(config$states_config_file, col_types = cols())

nsteps <- length(full_time_local)

if(nrow(config$states_config) > 0){
  config$include_wq <- TRUE
}else{
  config$include_wq <- FALSE
}

ndepths_modeled <- length(config$modeled_depths)
nstates <- ndepths_modeled * length(config$mstates_config$state_names)
num_wq_vars <-  length(config$mstates_config$state_names) - 1

glm_output_vars <- config$mstates_config$state_names

config$n_met_members <- 21
# SET UP NUMBER OF ENSEMBLE MEMBERS
if(forecast_days > 0 & config$use_future_met == TRUE  & (config$ensemble_size %% (config$n_met_members * config$n_ds_members)) != 0){
  stop(paste0("ensemble_size (",config$ensemble_size,") is not a multiple of the number of
                n_met_members (",config$n_met_members,
              ") * n_ds_members (",config$n_ds_members,")"))
}

if(config$single_run){
  config$n_met_members <- 3
  config$n_ds_members <- 1
}

####################################################
#### STEP 3: ORGANIZE FILES
####################################################

###CREATE DIRECTORY PATHS AND STRUCTURE
working_directory <- paste0(execute_location, "/", "working_directory")
if(!dir.exists(working_directory)){
  dir.create(working_directory, showWarnings = FALSE)
}
####Clear out temp GLM working directory
unlink(paste0(working_directory, "/*"), recursive = FALSE)


if(is.na(config$sim_name)){
  sim_name <- paste0(year(full_time_local[1]), "_",
                     month(full_time_local[1]), "_",
                     day(full_time_local[1]))
}

####################################################
#### STEP 4: PROCESS RAW INPUT AND OBSERVATION DATA
####################################################

#Container 1:  Downloading data

source("/Users/quinn/Dropbox/Research/SSC_forecasting/FLARE_package/flare/notebook/01_download_data.R")


#Container 2:  QAQC Data

source("/Users/quinn/Dropbox/Research/SSC_forecasting/FLARE_package/flare/notebook/02_process_observations.R")

#### END QAQC CONTAINER ####

source("/Users/quinn/Dropbox/Research/SSC_forecasting/FLARE_package/flare/notebook/03_generate_forecast.R")
