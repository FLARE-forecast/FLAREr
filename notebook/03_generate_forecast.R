config <- yaml::read_yaml("/Users/quinn/Dropbox/Research/SSC_forecasting/FLARE_package/flare_fcr/configure_flare.yml")
run_config <- yaml::read_yaml("/Users/quinn/Dropbox/Research/SSC_forecasting/FLARE_package/flare_fcr/run_configuration.yml")

config$run_config <- run_config

#library(tidyverse)

#Load the NOAA GEFS script because they aren't officially part of the
#package yet
#source(system.file("process_downscale_GEFS.R", package="flare"))
#source(system.file("process_GEFS.R", package="flare"))
#source(system.file("check_CI.R", package="flare"))
#source(system.file("aggregate_to_daily.R", package="flare"))
#source(system.file("aggregate_obs_to_hrly.R", package="flare"))
#source(system.file("ShortWave_to_hrly.R", package="flare"))
#source(system.file("daily_debias_from_coeff.R", package="flare"))
#source(system.file("downscale_met.R", package="flare"))
#source(system.file("fit_downscaling_parameters.R", package="flare"))
#source(system.file("daily_to_6hr.R", package="flare"))
#source(system.file("spline_to_hourly.R", package="flare"))
#source(system.file("prep_for.R", package="flare"))
#source(system.file("plot_downscaled_met.R", package="flare"))
#source(system.file("add_noise.R", package="flare"))
#source(system.file("repeat_6hr_to_hrly.R", package="flare"))
#source(system.file("compare_output_to_obs.R", package="flare"))
#source(system.file("solar_geom.R", package="flare"))

start_datetime_local <- lubridate::as_datetime(paste0(config$run_config$start_day_local," ",config$run_config$start_time_local), tz = config$local_tzone)
end_datetime_local <- lubridate::as_datetime(paste0(config$run_config$end_day_local," ",config$run_config$start_time_local), tz = config$local_tzone)

if(is.na(config$run_config$forecast_start_day_local)){
  forecast_start_datetime_local <- lubridate::as_datetime(paste0(config$run_config$end_datetime_local," ",config$run_config$start_time_local), tz = config$local_tzone)
}else{
  forecast_start_datetime_local <- lubridate::as_datetime(paste0(config$run_config$forecast_start_day_local," ",config$run_config$start_time_local), tz = config$local_tzone)
}

forecast_days <- as.numeric(end_datetime_local - forecast_start_datetime_local)



hist_days <- as.numeric(forecast_start_datetime_local - start_datetime_local)


full_time_local <- seq(start_datetime_local, end_datetime_local, by = "1 day")


if(!is.na(config$par_file)){
  pars_config <- readr::read_csv(file.path(run_config$forecast_location, config$par_file), col_types = readr::cols())
  npars <- nrow(pars_config)
}else{
  npars <- 0
}

obs_config <- readr::read_csv(file.path(run_config$forecast_location, config$obs_config_file), col_types = readr::cols())

states_config <- readr::read_csv(file.path(run_config$forecast_location, config$states_config_file), col_types = readr::cols())

nsteps <- length(full_time_local)

if(nrow(states_config) > 0){
  config$include_wq <- TRUE
}else{
  config$include_wq <- FALSE
}

ndepths_modeled <- length(config$modeled_depths)
nstates <- ndepths_modeled * length(states_config$state_names)


glm_output_vars <- states_config$state_names

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

  temp_met_file<- flare::process_downscale_GEFS(folder = config$code_folder,
                                         noaa_location = config$noaa_location,
                                         input_met_file = cleaned_met_file,
                                         working_directory,
                                         n_ds_members = config$n_ds_members,
                                         n_met_members = config$n_met_members,
                                         file_name,
                                         local_tzone = config$local_tzone,
                                         FIT_PARAMETERS,
                                         DOWNSCALE_MET = config$downscale_met,
                                         met_downscale_uncertainty = FALSE,
                                         compare_output_to_obs = FALSE,
                                         VarInfo,
                                         replaceObsNames,
                                         downscaling_coeff = file.path(config$qaqc_data_location, config$downscaling_coeff),
                                         full_time_local,
                                         first_obs_date = lubridate::date(config$met_ds_obs_start),
                                         last_obs_date = lubridate::as_date(config$met_ds_obs_end),
                                         input_met_file_tz = config$local_tzone,
                                         weather_uncertainty = config$weather_uncertainty,
                                         obs_met_outfile,
                                         lake_latitude = config$lake_latitude,
                                         lake_longitude = config$lake_longitude)

  met_file_names[1] <- temp_met_file[1]
}

###CREATE FUTURE MET FILES
if(forecast_days > 0 & config$use_future_met){
  in_directory <- file.path(config$data_location, config$noaa_location)
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

  met_file_names[] <- flare::process_downscale_GEFS(folder = config$code_folder,
                                             noaa_location = file.path(config$data_location, config$noaa_location),
                                             input_met_file = cleaned_met_file,
                                             working_directory,
                                             n_ds_members = config$n_ds_members,
                                             n_met_members = config$n_met_members,
                                             file_name,
                                             local_tzone = config$local_tzone,
                                             FIT_PARAMETERS,
                                             DOWNSCALE_MET = config$downscale_met,
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
                                             obs_met_outfile,
                                             lake_latitude = config$lake_latitude,
                                             lake_longitude = config$lake_longitude)

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
                                                   states_config = states_config,
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

obs <- flare::create_obs_matrix(cleaned_observations_file_long,
                                obs_config,
                                start_datetime_local,
                                end_datetime_local,
                                local_tzone = config$local_tzone,
                                modeled_depths = config$modeled_depths)

if(!config$use_obs_constraint){
  obs[, , ] <- NA
}

####################################################
#### STEP 8: SET UP INITIAL CONDITIONS
####################################################

init_depth <- array(NA, dim = c(nrow(states_config),ndepths_modeled))
for(i in 1:nrow(states_config)){
  if(!is.na(states_config$init_obs_name[i])){
    obs_index <- which(obs_config$state_names_obs == states_config$init_obs_name[i])
    init_obs <- obs[obs_index, 1, ] * (1/states_config$states_to_obs_mapping_1[i]) * states_config$init_obs_mapping[i]
    if(length(which(!is.na(init_obs))) == 0){
      init_depth[i, ] <- rep(states_config$initial_conditions[i], ndepths_modeled)
      if(states_config$init_obs_name[i] == "temp"){
        init_obs <- approx(x = config$default_temp_init_depths, y = config$default_temp_init, xout = config$modeled_depths, rule=2)
      }
    }else if(length(which(!is.na(init_obs))) == 1){
      init_depth[i, ]  <- rep(init_obs[!is.na(init_obs)], ndepths_modeled)
    }else{
      init_depth[i, ]  <- approx(x = config$modeled_depths[!is.na(init_obs)], y = init_obs[!is.na(init_obs)], xout = config$modeled_depths, rule=2)$y
    }
  }else{
    init_depth[i, ]  <- rep(states_config$initial_conditions[i], ndepths_modeled)
  }
}

states_to_obs_temp <- cbind(states_config$states_to_obs_1,states_config$states_to_obs_2, states_config$states_to_obs_3)
states_to_obs_mapping_temp <- cbind(states_config$states_to_obs_mapping_1,states_config$states_to_obs_mapping_2, states_config$states_to_obs_mapping_3)

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
      values1[j] <- which(obs_config$state_names_obs == names_temp[j])
    }
    values2 <- c(mapping_temp)
  }
  states_to_obs[[i]] <- values1
  states_to_obs_mapping[[i]] <- values2
}

states_config$states_to_obs <- states_to_obs
states_config$states_to_obs_mapping <- states_to_obs_mapping

################################################################
#### STEP 11: CREATE THE X ARRAY (STATES X TIME);INCLUDES INITIALATION
################################################################
nmembers <- config$ensemble_size

nstates <- length(states_config$state_names)

init <- list()


restart_present <- FALSE
if(!is.na(run_config$restart_file)){
  if(file.exists(restart_file)){
    restart_present <- TRUE
  }
}

#Initial conditions
if(!restart_present){

  init$states <- array(NA, dim=c(nstates, ndepths_modeled, nmembers))
  init$pars <- array(NA, dim=c(npars, nmembers))
  init$lake_depth <- array(NA, dim=c(nmembers))
  init$snow_ice_thickness <- array(NA, dim=c(3, nmembers))
  init$avg_surf_temp <- array(NA, dim=c(nmembers))
  init$mixing_vars <- array(NA, dim=c(17, nmembers))
  init$model_internal_depths <- array(NA, dim = c(500, nmembers))
  init$salt <- array(NA, dim = c(ndepths_modeled, nmembers))

  alpha_v <- 1 - exp(-config$vert_decorr_length)

  q_v <- rep(NA ,ndepths_modeled)
  w <- rep(NA, ndepths_modeled)

  combined_initial_conditions <- init_depth


  for(m in 1:nmembers){
    q_v[] <- NA
    w[] <- NA
    for(jj in 1:nstates){
      w[] <- rnorm(ndepths_modeled, 0, 1)
      q_v[1] <- states_config$initial_model_sd[jj] * w[1]
      for(kk in 2:ndepths_modeled){
        q_v[kk] <- alpha_v * q_v[kk-1] + sqrt(1 - alpha_v^2) * states_config$initial_model_sd[jj] * w[kk]
      }

      if(config$single_run | (config$initial_condition_uncertainty == FALSE & hist_days == 0)){
        init$states[jj, , m] <- combined_initial_conditions[jj, ]
      }else{
        init$states[jj, , m] <- combined_initial_conditions[jj, ] + q_v
      }
      if(jj > 1){
      init$states[jj,which(init$states[jj, , m]) < 0.0 , m] <- 0.0
      }
    }
  }

  for(par in 1:npars){
    init$pars[par, ] <- runif(n=nmembers,pars_config$par_init_lowerbound[par], pars_config$par_init_upperbound[par])
    if(config$single_run){
      init$pars[par, ] <-  rep(pars_config$par_init[par], nmembers)
    }
  }

  init$lake_depth[] <- round(config$lake_depth_init, 3)
  #Matrix to store snow and ice heights
  init$snow_ice_thickness[1, ] <- config$default_snow_thickness_init
  init$snow_ice_thickness[2, ] <- config$default_white_ice_thickness_init
  init$snow_ice_thickness[3, ] <- config$default_blue_ice_thickness_init
  init$avg_surf_temp[] <- init$states[1 , 1, ]
  init$mixing_vars[, ] <- 0.0
  init$salt[, ] <- config$the_sals_init

  for(m in 1:nmembers){
    init$model_internal_depths[1:ndepths_modeled, m] <- config$modeled_depths
  }
}else{
  #Restart from yesterday's run
  init <- flare::generate_restart_initial_conditions(restart_file,
                                                     state_names = states_config$state_names,
                                                     par_names = pars_config$par_names_save)

}

#### Create management


d <- readr::read_csv(file.path(config$data_location, config$sss_fname), col_type = readr::cols(
  time = readr::col_date(format = ""),
  FLOW = readr::col_double(),
  OXY_oxy = readr::col_double())
)

full_time_day_local <- lubridate::as_date(full_time_local)

sss_flow <- rep(0, length(full_time_day_local))
sss_OXY_oxy <- rep(0, length(full_time_day_local))

if(length(which(d$time == full_time_day_local[1])) > 0){

  for(i in 1:(length(full_time_day_local))){
    index <- which(d$time == full_time_day_local[i])
    if(length(index) > 0){
      sss_flow[i] <- unlist(d[index, "FLOW"])
      sss_OXY_oxy[i] <- unlist(d[index, "OXY_oxy"])
    }
  }
}
management_input <- data.frame(sss_flow = sss_flow, sss_OXY_oxy = sss_OXY_oxy)

management <- list()
management$management_input <- management_input
management$simulate_sss <- config$simulate_sss
management$forecast_sss_on <- run_config$forecast_sss_on
management$sss_depth <- config$sss_depth
management$use_specified_sss <- config$use_specified_sss
management$specified_sss_inflow_file <- config$specified_sss_inflow_file
management$specified_sss_outflow_file <- config$specified_sss_outflow_file
management$forecast_sss_flow <- config$forecast_sss_flow
management$forecast_sss_oxy <- config$forecast_sss_oxy

####################################################
#### STEP 12: Run Ensemble Kalman Filter
####################################################
aux_states_init <- list()
aux_states_init$snow_ice_thickness <- init$snow_ice_thickness
aux_states_init$avg_surf_temp <- init$avg_surf_temp
aux_states_init$the_sals_init <- config$the_sals_init
aux_states_init$mixing_vars <- init$mixing_vars
aux_states_init$model_internal_depths <- init$model_internal_depths
aux_states_init$lake_depth <- init$lake_depth
aux_states_init$salt <- init$salt


enkf_output <- flare::run_EnKF(states_init = init$states,
                               pars_init = init$pars,
                               obs = obs,
                               obs_sd = obs_config$obs_sd,
                               model_sd = states_config$model_sd,
                               working_directory = working_directory,
                               met_file_names = met_file_names,
                               inflow_file_names = inflow_file_names,
                               outflow_file_names = outflow_file_names,
                               sim_start_datetime = start_datetime_local,
                               sim_end_datetime = end_datetime_local,
                               forecast_start_datetime = forecast_start_datetime_local,
                               config = config,
                               pars_config = pars_config,
                               states_config = states_config,
                               obs_config = obs_config,
                               aux_states_init = aux_states_init,
                               management = management
)

###SAVE FORECAST
saved_file <- flare::write_forecast_netcdf(enkf_output,
                                           forecast_location = run_config$forecast_location)

#Create EML Metadata
flare::create_flare_eml(file_name = saved_file,
                        enkf_output)

unlink(working_directory)


#### END START ARCHIVE CONTAINER

