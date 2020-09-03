config <- yaml::read_yaml("/Users/quinn/Dropbox/Research/SSC_forecasting/FLARE_package/flare/notebook/configure_flare.yml")
run_config <- yaml::read_yaml("/Users/quinn/Dropbox/Research/SSC_forecasting/FLARE_package/flare/notebook/run_configuration.yml")

config$run_config <- run_config

source(paste0(config$code_folder_old,"/","Rscripts/met_downscale/process_downscale_GEFS.R"))
source(paste0(config$code_folder_old,"/","Rscripts/create_sss_input_output.R"))

start_datetime_local <- lubridate::as_datetime(paste0(config$run_config$start_day_local," ",config$run_config$start_time_local), tz = config$local_tzone)
end_datetime_local <- lubridate::as_datetime(paste0(config$run_config$end_day_local," ",config$run_config$start_time_local), tz = config$local_tzone)

if(is.na(config$run_config$forecast_start_day_local)){
  forecast_start_datetime_local <- lubridate::as_datetime(paste0(config$run_config$end_datetime_local," ",config$run_config$start_time_local), tz = config$local_tzone)
}else{
  forecast_start_datetime_local <- lubridate::as_datetime(paste0(config$run_config$forecast_start_day_local," ",config$run_config$start_time_local), tz = config$local_tzone)
}

forecast_days <- as.numeric(forecast_start_datetime_local - end_datetime_local)



hist_days <- as.numeric(forecast_start_datetime_local - start_datetime_local)


full_time_local <- seq(start_datetime_local, end_datetime_local, by = "1 day")



switch(Sys.info() [["sysname"]],
       Linux = { config$machine <- "unix" },
       Darwin = { config$machine <- "mac" },
       Windows = { config$machine <- "windows"})

if(!is.na(config$par_file)){
  config$pars_config <- readr::read_csv(file.path(run_config$forecast_location, config$par_file), col_types = readr::cols())
  npars <- nrow(config$pars_config)
}else{
  npars <- 0
}

config$obs_config <- readr::read_csv(file.path(run_config$forecast_location, config$obs_config_file), col_types = readr::cols())

config$states_config <- readr::read_csv(file.path(run_config$forecast_location, config$states_config_file), col_types = readr::cols())

nsteps <- length(full_time_local)

if(nrow(config$states_config) > 0){
  config$include_wq <- TRUE
}else{
  config$include_wq <- FALSE
}

ndepths_modeled <- length(config$modeled_depths)
nstates <- ndepths_modeled * length(config$states_config$state_names)
num_wq_vars <-  length(config$states_config$state_names) - 1

glm_output_vars <- config$states_config$state_names

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
working_directory <- paste0(run_config$execute_location, "/", "working_directory")
if(!dir.exists(working_directory)){
  dir.create(working_directory, showWarnings = FALSE)
}
####Clear out temp GLM working directory
unlink(paste0(working_directory, "/*"), recursive = FALSE)

####################################################
#### STEP 4: PROCESS RAW INPUT AND OBSERVATION DATA
####################################################

#Container 1:  Downloading data

### All of this is for working with the NOAA data #####
### METEROLOGY DOWNSCALING OPTIONS
if(is.na(config$downscaling_coeff)){
  FIT_PARAMETERS <- TRUE
}else{
  FIT_PARAMETERS <- FALSE
}

if(config$downscale_met == FALSE){
  FIT_PARAMETERS <- FALSE
}

start_datetime_GMT <- lubridate::with_tz(dplyr::first(full_time_local), tzone = "GMT")
end_datetime_GMT <- lubridate::with_tz(dplyr::last(full_time_local), tzone = "GMT")
forecast_start_time_GMT<- lubridate::with_tz(forecast_start_datetime_local, tzone = "GMT")

forecast_start_time_GMT_past <- forecast_start_time_GMT - lubridate::days(1)

noaa_hour <- NA
if(!lubridate::hour(forecast_start_time_GMT) %in% c(0,6,12,18) & forecast_days > 0){
  stop(paste0("local_start_datetime of ", local_start_datetime," does not have a corresponding GMT time with a NOAA forecast
                The GMT times that are avialable are 00:00:00, 06:00:00, 12:00:00, and 18:00:00"))
}else{
  if(lubridate::hour(forecast_start_time_GMT) == 0){
    noaa_hour <- "00"
  }
  if(lubridate::hour(forecast_start_time_GMT) == 6){
    noaa_hour <- "06"
  }
  if(lubridate::hour(forecast_start_time_GMT) == 12){
    noaa_hour <- "12"
  }
  if(lubridate::hour(forecast_start_time_GMT) == 18){
    noaa_hour <- "18"
  }
}

if(lubridate::day(forecast_start_time_GMT) < 10){
  forecast_day_GMT <- paste0("0", lubridate::day(forecast_start_time_GMT))
}else{
  forecast_day_GMT <- paste0(lubridate::day(forecast_start_time_GMT))
}
if(lubridate::month(forecast_start_time_GMT) < 10){
  forecast_month_GMT <- paste0("0", lubridate::month(forecast_start_time_GMT))
}else{
  forecast_month_GMT <- paste0(lubridate::month(forecast_start_time_GMT))
}

if(lubridate::day(forecast_start_time_GMT_past) < 10){
  forecast_day_GMT_past <- paste0("0", lubridate::day(forecast_start_time_GMT_past))
}else{
  forecast_day_GMT_past <- paste0(lubridate::day(forecast_start_time_GMT_past))
}
if(lubridate::month(forecast_start_time_GMT_past) < 10){
  forecast_month_GMT_past <- paste0("0", lubridate::month(forecast_start_time_GMT_past))
}else{
  forecast_month_GMT_past <- paste0(lubridate::month(forecast_start_time_GMT_past))
}

forecast_base_name <- paste0(config$lake_name_code,"_",
                             lubridate::year(forecast_start_time_GMT),
                             forecast_month_GMT,
                             forecast_day_GMT,"_",
                             "gep_all_",
                             noaa_hour,
                             "z")

forecast_base_name_past <- paste0(config$lake_name_code,"_",
                                  lubridate::year(forecast_start_time_GMT_past),
                                  forecast_month_GMT_past,
                                  forecast_day_GMT_past,"_",
                                  "gep_all_",
                                  noaa_hour,
                                  "z")

met_forecast_base_file_name <- paste0("met_hourly_",
                                      forecast_base_name,
                                      "_ens")

met_file_names <- rep(NA, (config$n_met_members*config$n_ds_members))
obs_met_outfile <- "met_historical.csv"

cleaned_met_file <- paste0(config$qaqc_data_location, "/met_full_postQAQC.csv")



if(is.na(config$specified_metfile)){
  missing_met <- flare::create_obs_met_input(fname = cleaned_met_file,
                                      outfile = obs_met_outfile,
                                      full_time_local,
                                      config$local_tzone,
                                      working_directory,
                                      hist_days,
                                      missing_met_data_threshold = config$missing_met_data_threshold)
}else{
  missing_met <- FALSE
  file.copy(config$specified_metfile, paste0(working_directory,"/",obs_met_outfile))
}

if(missing_met  == FALSE){
  met_file_names[] <- obs_met_outfile
}else{
  if(hist_days > 1){
    stop(paste0("Running more than 1 hist_day but met data has ",
                missing_met," values"))
  }
  in_directory <- paste0(noaa_location)
  out_directory <- working_directory
  file_name <- forecast_base_name_past

  VarInfo <- data.frame("VarNames" = c("AirTemp",
                                       "WindSpeed",
                                       "RelHum",
                                       "ShortWave",
                                       "LongWave",
                                       "Rain"),
                        "VarType" = c("State",
                                      "State",
                                      "State",
                                      "Flux",
                                      "Flux",
                                      "Flux"),
                        "ds_res" = c("hour",
                                     "hour",
                                     "hour",
                                     "hour",
                                     "6hr",
                                     "6hr"),
                        "debias_method" = c("lm",
                                            "lm",
                                            "lm",
                                            "lm",
                                            "lm",
                                            "none"),
                        "use_covariance" = c(TRUE,
                                             FALSE,
                                             TRUE,
                                             TRUE,
                                             TRUE,
                                             FALSE),
                        stringsAsFactors = FALSE)

  replaceObsNames <- c("AirTemp" = "AirTemp",
                       "WindSpeed" = "WindSpeed",
                       "RelHum" = "RelHum",
                       "ShortWave" = "ShortWave",
                       "LongWave" = "LongWave",
                       "Rain" = "Rain")

  temp_met_file<- process_downscale_GEFS(folder = config$code_folder,
                                         noaa_location = config$noaa_location,
                                         input_met_file = cleaned_met_file,
                                         working_directory,
                                         n_ds_members = config$n_ds_members,
                                         n_met_members = config$n_met_members,
                                         file_name,
                                         local_tzone = config$local_tzone,
                                         FIT_PARAMETERS,
                                         DOWNSCALE_MET,
                                         met_downscale_uncertainty = FALSE,
                                         compare_output_to_obs = FALSE,
                                         VarInfo,
                                         replaceObsNames,
                                         downscaling_coeff = file.path(config$data_location, config$downscaling_coeff),
                                         full_time_local,
                                         first_obs_date = lubridate::date(config$met_ds_obs_start),
                                         last_obs_date = lubridate::as_date(config$met_ds_obs_end),
                                         input_met_file_tz = config$local_tzone,
                                         weather_uncertainty = config$weather_uncertainty,
                                         obs_met_outfile)

  met_file_names[1] <- temp_met_file[1]
}

###CREATE FUTURE MET FILES
if(forecast_days > 0 & config$use_future_met){
  in_directory <- paste0(config$noaa_location)
  out_directory <- working_directory
  file_name <- forecast_base_name

  VarInfo <- data.frame("VarNames" = c("AirTemp",
                                       "WindSpeed",
                                       "RelHum",
                                       "ShortWave",
                                       "LongWave",
                                       "Rain"),
                        "VarType" = c("State",
                                      "State",
                                      "State",
                                      "Flux",
                                      "Flux",
                                      "Flux"),
                        "ds_res" = c("hour",
                                     "hour",
                                     "hour",
                                     "hour",
                                     "6hr",
                                     "6hr"),
                        "debias_method" = c("lm",
                                            "lm",
                                            "lm",
                                            "lm",
                                            "lm",
                                            "none"),
                        "use_covariance" = c(TRUE,
                                             TRUE,
                                             TRUE,
                                             TRUE,
                                             TRUE,
                                             FALSE),
                        stringsAsFactors = FALSE)

  replaceObsNames <- c("AirTemp" = "AirTemp",
                       "WindSpeed" = "WindSpeed",
                       "RelHum" = "RelHum",
                       "ShortWave" = "ShortWave",
                       "LongWave" = "LongWave",
                       "Rain" = "Rain")

  met_file_names[] <- process_downscale_GEFS(folder = config$code_folder,
                                             noaa_location = config$noaa_location,
                                             input_met_file = cleaned_met_file,
                                             working_directory,
                                             n_ds_members = config$n_ds_members,
                                             n_met_members = config$n_met_members,
                                             file_name,
                                             local_tzone = config$local_tzone,
                                             FIT_PARAMETERS,
                                             DOWNSCALE_MET,
                                             met_downscale_uncertainty = FALSE,
                                             compare_output_to_obs = FALSE,
                                             VarInfo,
                                             replaceObsNames,
                                             downscaling_coeff = file.path(config$data_location, config$downscaling_coeff),
                                             full_time_local,
                                             first_obs_date = lubridate::date(config$met_ds_obs_start),
                                             last_obs_date = lubridate::as_date(config$met_ds_obs_end),
                                             input_met_file_tz = config$local_tzone,
                                             weather_uncertainty = config$weather_uncertainty,
                                             obs_met_outfile)
}

inflow_met_file_names <- met_file_names

if(config$weather_uncertainty == FALSE){
  #n_enkf_members <- n_enkf_members * n_met_members
  config$n_met_members <- 1
}

##CREATE INFLOW AND OUTFILE FILES

cleaned_inflow_file <- paste0(config$qaqc_data_location, "/inflow_postQAQC.csv")

start_forecast_step <- as.numeric(forecast_start_datetime_local - start_datetime_local) + 1

inflow_outflow_files <- flare::create_inflow_outflow_file(full_time_local,
                                                   working_directory,
                                                   input_file_tz = "EST",
                                                   start_forecast_step,
                                                   inflow_file1 = cleaned_inflow_file,
                                                   inflow_file2 = config$inflow_file2,
                                                   outflow_file1 = config$outflow_file1,
                                                   chemistry_file = cleaned_inflow_file,
                                                   local_tzone = config$local_tzone,
                                                   met_file_names,
                                                   forecast_days,
                                                   inflow_process_uncertainty = config$inflow_process_uncertainty,
                                                   future_inflow_flow_coeff  = config$future_inflow_flow_coeff,
                                                   future_inflow_flow_error = config$future_inflow_flow_error,
                                                   future_inflow_temp_coeff = config$future_inflow_temp_coeff,
                                                   future_inflow_temp_error = config$future_inflow_temp_error,
                                                   states_config = config$states_config,
                                                   include_wq = config$include_wq,
                                                   use_future_inflow = config$use_future_inflow,
                                                   doc_scalar = config$doc_scalar)


if(is.na(config$specified_inflow1)){
  inflow_file_names <- cbind(inflow1 = inflow_outflow_files$inflow_file_names,
                             inflow2 = inflow_outflow_files$wetland_file_names)
  outflow_file_names <- cbind(inflow_outflow_files$spillway_file_names)
}else{
  inflow_file_names <- cbind(inflow1 = file.path(config$data_location, config$specified_inflow1),
                             inflow2 = file.path(config$data_location, config$specified_inflow2))
  outflow_file_names <- cbind(file.path(config$data_location, config$specified_outflow1))
}



#### END DRIVER CONTAINER ####

cleaned_observations_file_long <- paste0(config$qaqc_data_location,
                                         "/observations_postQAQC_long.csv")

obs <- flare::create_obs_matrix(cleaned_observations_file_long, config, start_datetime_local, end_datetime_local)


####################################################
#### STEP 8: SET UP INITIAL CONDITIONS
####################################################

init_depth <- list()
for(i in 1:nrow(config$states_config)){
  if(!is.na(config$states_config$init_obs_name[i])){
    obs_index <- which(config$obs_config$state_names_obs == config$states_config$init_obs_name[i])
    #init_obs <- z[1, ,obs_index] * (1/states_config$states_to_obs_mapping[[i]][1]) * states_config$init_obs_mapping[i]
    init_obs <- obs[1, ,obs_index] * (1/config$states_config$states_to_obs_mapping_1[i]) * config$states_config$init_obs_mapping[i]
    if(length(which(!is.na(init_obs))) == 0){
      init_depth[[i]] <- rep(config$states_config$initial_conditions[i], ndepths_modeled)
      if(config$states_config$init_obs_name[i] == "temp"){
        init_obs <- approx(x = config$default_temp_init_depths, y = config$default_temp_init, xout = config$modeled_depths, rule=2)
      }
    }else if(length(which(!is.na(init_obs))) == 1){
      init_depth[[i]] <- rep(init_obs[!is.na(init_obs)], ndepths_modeled)
    }else{
      init_depth[[i]] <- approx(x = config$modeled_depths[!is.na(init_obs)], y = init_obs[!is.na(init_obs)], xout = config$modeled_depths, rule=2)$y
    }
  }else{
    init_depth[[i]] <- rep(config$states_config$initial_conditions[i], ndepths_modeled)
  }
}

num_wq_vars <- length(config$states_config$state_names) - 1

wq_start <- NA
wq_end <- NA
if(config$include_wq){
  temp_start <- 1
  temp_end <- ndepths_modeled
  wq_start <- rep(NA, num_wq_vars)
  wq_end <- rep(NA, num_wq_vars)
  for(wq in 1:num_wq_vars){
    if(wq == 1){
      wq_start[wq] <- temp_end+1
      wq_end[wq] <- temp_end + (ndepths_modeled)
    }else{
      wq_start[wq] <- wq_end[wq-1]+1
      wq_end[wq] <- wq_end[wq-1] + (ndepths_modeled)
    }
  }
}else{
  temp_start <- 1
  temp_end <- ndepths_modeled
  wq_start <- temp_end+1
  wq_end <- temp_end+1
}


#######################################################
#### STEP 9: CREATE THE PSI VECTOR (DATA uncertainty)
#######################################################

psi_slope <- rep(NA, length(config$obs_config$state_names_obs) * ndepths_modeled)
psi_intercept <- rep(NA, length(config$obs_config$state_names_obs) * ndepths_modeled)

index <- 0
for(i in 1:length(config$obs_config$state_names_obs)){
  for(j in 1:ndepths_modeled){
    index <- index + 1
    psi_intercept[index] <- config$obs_config$obs_error_intercept[[i]]
    psi_slope[index] <- config$obs_config$obs_error_slope[[i]]
  }
}

psi <- cbind(psi_intercept, psi_slope)

states_to_obs_temp <- cbind(config$states_config$states_to_obs_1,config$states_config$states_to_obs_2, config$states_config$states_to_obs_3)
states_to_obs_mapping_temp <- cbind(config$states_config$states_to_obs_mapping_1,config$states_config$states_to_obs_mapping_2, config$states_config$states_to_obs_mapping_3)

states_to_obs <- list()
states_to_obs_mapping <- list()
for(i in 1:nrow(states_to_obs_temp)){

  names_temp <- states_to_obs_temp[i,which(!is.na(states_to_obs_temp[i,]))]
  mapping_temp <- states_to_obs_mapping_temp[i,which(!is.na(states_to_obs_mapping_temp[i,]))]
  if(length(names_temp) == 0){
    values1 <- NA
    values2 <- NA
  }else{
    values1 <- rep(NA,length(names_temp))
    for(j in 1:length(names_temp)){
      values1[j] <- which(config$obs_config$state_names_obs == names_temp[j])
    }
    values2 <- c(mapping_temp)
  }
  states_to_obs[[i]] <- values1
  states_to_obs_mapping[[i]] <- values2
}

config$states_config$states_to_obs <- states_to_obs
config$states_config$states_to_obs_mapping <- states_to_obs_mapping

####################################################
#### STEP 10: CREATE THE PROCESS UNCERTAINTY
####################################################

process_sd <- config$states_config$process_error
init_sd <- config$states_config$initial_error

################################################################
#### STEP 11: CREATE THE X ARRAY (STATES X TIME);INCLUDES INITIALATION
################################################################
nmembers <- config$ensemble_size

nstates <- ndepths_modeled * length(config$states_config$state_names)

init <- list()
init$x <- array(NA, dim=c(nmembers, nstates + npars))

restart_present <- FALSE
if(!is.na(run_config$restart_file)){
  if(file.exists(restart_file)){
    restart_present <- TRUE
  }
}

#Initial conditions
if(!restart_present){

  #Matrix to store essemble specific surface height
  init$surface_height <- array(NA, dim=c(nmembers))
  init$snow_ice_thickness <- array(NA, dim=c(nmembers, 3))
  init$avg_surf_temp <- array(NA, dim=c(nmembers))
  init$mixing_vars <- array(NA, dim=c(nmembers, 17))
  init$glm_depths <- array(NA, dim = c(nmembers, 500))

  alpha_v <- 1 - exp(-config$vert_decorr_length)

  q_v <- rep(NA ,ndepths_modeled)
  w <- rep(NA, ndepths_modeled)

  combined_initial_conditions <- unlist(init_depth)


  for(m in 1:nmembers){
    q_v[] <- NA
    w[] <- NA
    for(jj in 1:length(process_sd)){
      w[] <- rnorm(ndepths_modeled, 0, 1)
      q_v[1] <- process_sd[jj] * w[1]
      for(kk in 2:ndepths_modeled){
        q_v[kk] <- alpha_v * q_v[kk-1] + sqrt(1 - alpha_v^2) * init_sd[jj] * w[kk]
      }

      if(config$single_run | (config$initial_condition_uncertainty == FALSE & hist_days == 0)){
        init$x[m,(((jj-1)*ndepths_modeled)+1):(jj*ndepths_modeled)] <-
          combined_initial_conditions[(((jj-1)*ndepths_modeled)+1):(jj*ndepths_modeled)]
      }else{
        init$x[m,(((jj-1)*ndepths_modeled)+1):(jj*ndepths_modeled)] <-
          combined_initial_conditions[(((jj-1)*ndepths_modeled)+1):(jj*ndepths_modeled)] + q_v
      }
    }
  }

  for(par in 1:npars){
    init$x[ ,(nstates+par)] <- runif(n=nmembers,config$pars_config$par_init_lowerbound[par], config$pars_config$par_init_upperbound[par])
    if(config$single_run){
      init$x[ ,(nstates+par)] <-  rep(config$pars_config$par_init[par], nmembers)
    }
  }

  if(config$include_wq){
    for(m in 1:nmembers){
      index <- which(init$x[ m, 1:wq_end[num_wq_vars]] < 0.0)
      index <- index[which(index > wq_start[1])]
      init$x[ m, index] <- 0.0
    }
  }

  init$surface_height[] <- round(config$lake_depth_init, 3)
  #Matrix to store snow and ice heights
  init$snow_ice_thickness[ ,1] <- config$default_snow_thickness_init
  init$snow_ice_thickness[ ,2] <- config$default_white_ice_thickness_init
  init$snow_ice_thickness[ ,3] <- config$default_blue_ice_thickness_init
  init$avg_surf_temp[] <- init$x[ ,1]
  init$mixing_vars[, ] <- 0.0

  for(m in 1:nmembers){
    init$glm_depths[m, 1:ndepths_modeled] <- config$modeled_depths
  }
}else{
  #Restart from yesterday's run
  init <- flare::generate_restart_initial_conditions(restart_file, config)

}

#If hist_days = 0 then the first day of the simulation will be a forecast
#therefre the the initial_condition_uncertainty and parameter_uncertainty
#need to be dealt with in the x[1, ,] , normally it is dealt with in the
#run_EnKF script
if(hist_days == 0){
  if(config$initial_condition_uncertainty == FALSE){
    states_mean <- colMeans(x[1, ,1:nstates])
    for(m in 1:nmembers){
      x[1, m, 1:nstates]  <- states_mean
    }
  }
  if(parameter_uncertainty == FALSE){
    mean_pars <- colMeans(x[1, ,(nstates + 1):(nstates + npars)])
    for(m in 1:nmembers){
      x[1, m, (nstates + 1):(nstates +  npars)] <- mean_pars
    }
  }
}

management_input <- flare::read_sss_files(full_time_local,
                                   sss_file = file.path(config$data_location, config$sss_fname))

####################################################
#### STEP 12: Run Ensemble Kalman Filter
####################################################
aux_states_init <- list()
aux_states_init$snow_ice_thickness <- init$snow_ice_thickness
aux_states_init$avg_surf_temp <- init$avg_surf_temp
aux_states_init$the_sals_init <- config$the_sals_init
aux_states_init$mixing_vars <- init$mixing_vars
aux_states_init$glm_depths <- init$glm_depths
aux_states_init$surface_height <- init$surface_height




x_init <- init$x

enkf_output <- flare::run_EnKF(x_init,
                               obs,
                               psi,
                               process_sd,
                               working_directory,
                               met_file_names,
                               inflow_file_names,
                               outflow_file_names,
                               sim_start_datetime = start_datetime_local,
                               sim_end_datetime = end_datetime_local,
                               forecast_start_datetime = forecast_start_datetime_local,
                               management_input,
                               wq_start,
                               wq_end,
                               config,
                               aux_states_init,
                               code_folder
)

###SAVE FORECAST
saved_file <- flare::write_forecast_netcdf(enkf_output,
                                           wq_start,
                                           wq_end,
                                           forecast_location = run_config$forecast_location)

#Create EML Metadata
flare::create_flare_eml(file_name = saved_file,
                        enkf_output)


#### END START ARCHIVE CONTAINER

