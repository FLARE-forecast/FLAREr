full_time_local <- seq(start_datetime_local, end_datetime_local, by = "1 day")

### All of this is for working with the NOAA data #####
### METEROLOGY DOWNSCALING OPTIONS
if(is.na(config$downscaling_coeff)){
  FIT_PARAMETERS <- TRUE
}else{
  FIT_PARAMETERS <- FALSE
}

if(DOWNSCALE_MET == FALSE){
  FIT_PARAMETERS <- FALSE
}

start_datetime_GMT <- lubridate::with_tz(first(full_time_local), tzone = "GMT")
end_datetime_GMT <- lubridate::with_tz(last(full_time_local), tzone = "GMT")
forecast_start_time_GMT<- lubridate::with_tz(forecast_start_time_local, tzone = "GMT")

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

if(day(forecast_start_time_GMT) < 10){
  forecast_day_GMT <- paste0("0", lubridate::day(forecast_start_time_GMT))
}else{
  forecast_day_GMT <- paste0(lubridate::day(forecast_start_time_GMT))
}
if(month(forecast_start_time_GMT) < 10){
  forecast_month_GMT <- paste0("0", lubridate::month(forecast_start_time_GMT))
}else{
  forecast_month_GMT <- paste0(lubridate::month(forecast_start_time_GMT))
}

if(day(forecast_start_time_GMT_past) < 10){
  forecast_day_GMT_past <- paste0("0", lubridate::day(forecast_start_time_GMT_past))
}else{
  forecast_day_GMT_past <- paste0(lubridate::day(forecast_start_time_GMT_past))
}
if(month(forecast_start_time_GMT_past) < 10){
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

cleaned_met_file <- paste0(working_directory, "/met_full_postQAQC.csv")

if(is.na(forecast_start_datetime)){
  forecast_start_datetime <- sim_end_datetime
}

hist_days <- as.numeric(forecast_start_datetime - start_datetime)

if(is.na(config$specified_metfile)){
  missing_met <- create_obs_met_input(fname = cleaned_met_file,
                                      outfile = obs_met_outfile,
                                      full_time_local,
                                      config$local_tzone,
                                      working_directory,
                                      hist_days)
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
                                         downscaling_coeff = config$downscaling_coeff,
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
                                             downscaling_coeff = config$downscaling_coeff,
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

inflow_outflow_files <- create_inflow_outflow_file(full_time_local,
                                                   working_directory,
                                                   input_file_tz = "EST",
                                                   start_forecast_step,
                                                   inflow_file1 = cleaned_inflow_file,
                                                   inflow_file2,
                                                   outflow_file1,
                                                   chemistry_file = cleaned_inflow_file,
                                                   local_tzone,
                                                   met_file_names,
                                                   forecast_days,
                                                   inflow_process_uncertainty,
                                                   future_inflow_flow_coeff,
                                                   future_inflow_flow_error,
                                                   future_inflow_temp_coeff,
                                                   future_inflow_temp_error,
                                                   states_config,
                                                   include_wq)


if(is.na(specified_inflow1)){
  inflow_file_names <- cbind(inflow1 = inflow_outflow_files$inflow_file_names,
                             inflow2 = inflow_outflow_files$wetland_file_names)
  outflow_file_names <- cbind(inflow_outflow_files$spillway_file_names)
}else{
  inflow_file_names <- cbind(inflow1 = config$specified_inflow1,
                             inflow2 = config$specified_inflow2)
  outflow_file_names <- cbind(config$specified_outflow1)
}



#### END DRIVER CONTAINER ####

cleaned_observations_file_long <- paste0(working_directory,
                                         "/observations_postQAQC_long.csv")

obs <- flare::create_obs_matrix(cleaned_observations_file_long, config, start_datetime_local, end_datetime_local)


####################################################
#### STEP 8: SET UP INITIAL CONDITIONS
####################################################

init_depth <- list()
for(i in 1:nrow(config$states_config)){
  if(!is.na(config$states_config$init_obs_name[i])){
    obs_index <- which(obs_config$state_names_obs == states_config$init_obs_name[i])
    #init_obs <- z[1, ,obs_index] * (1/states_config$states_to_obs_mapping[[i]][1]) * states_config$init_obs_mapping[i]
    init_obs <- z[1, ,obs_index] * (1/config$states_config$states_to_obs_mapping_1[i]) * config$states_config$init_obs_mapping[i]
    if(length(which(!is.na(init_obs))) == 0){
      init_depth[[i]] <- rep(config$states_config$initial_conditions[i], ndepths_modeled)
      if(config$states_config$init_obs_name[i] == "temp"){
        init_obs <- approx(x = config$default_temp_init_depths, y = config$default_temp_init, xout = config$modeled_depths, rule=2)
      }
    }else if(length(which(!is.na(init_obs))) == 1){
      init_depth[[i]] <- rep(init_obs[!is.na(init_obs)], ndepths_modeled)
    }else{
      init_depth[[i]] <- approx(x = modeled_depths[!is.na(init_obs)], y = init_obs[!is.na(init_obs)], xout = modeled_depths, rule=2)$y
    }
  }else{
    init_depth[[i]] <- rep(config$states_config$initial_conditions[i], ndepths_modeled)
  }
}

wq_start <- NA
wq_end <- NA
if(include_wq){
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

####################################################
#### STEP 10: CREATE THE PROCESS UNCERTAINTY
####################################################

combined_error <- states_config$process_error
combined_init_error <- states_config$initial_error

################################################################
#### STEP 11: CREATE THE X ARRAY (STATES X TIME);INCLUDES INITIALATION
################################################################
nmembers <- config$ensemble_size

init <- list()
init$x <- array(NA, dim=c(nmembers, nstates + npars))

restart_present <- FALSE
if(!is.na(restart_file)){
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

  alpha_v <- 1 - exp(-vert_decorr_length)

  q_v <- rep(NA ,ndepths_modeled)
  w <- rep(NA, ndepths_modeled)

  combined_initial_conditions <- unlist(init_depth)


  for(m in 1:nmembers){
    q_v[] <- NA
    w[] <- NA
    for(jj in 1:length(combined_error)){
      w[] <- rnorm(ndepths_modeled, 0, 1)
      q_v[1] <- combined_error[jj] * w[1]
      for(kk in 2:ndepths_modeled){
        q_v[kk] <- alpha_v * q_v[kk-1] + sqrt(1 - alpha_v^2) * combined_init_error[jj] * w[kk]
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
    init$x[ ,(nstates+par)] <- runif(n=nmembers,pars_config$par_init_lowerbound[par], pars_config$par_init_upperbound[par])
    if(single_run){
      init$x[ ,(nstates+par)] <-  rep(pars_config$par_init[par], nmembers)
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
    init$glm_depths[m, 1:ndepths_modeled] <- modeled_depths
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

management_input <- read_sss_files(full_time_local,
                                   sss_file = config$sss_fname)

####################################################
#### STEP 12: Run Ensemble Kalman Filter
####################################################
aux_states_init <- list()
aux_states_init$snow_ice_thickness <- init$snow_ice_thickness
aux_states_init$avg_surf_temp <- init$avg_surf_temp
aux_states_init$the_sals_init <- the_sals_init
aux_states_init$mixing_vars <- init$mixing_vars
aux_states_init$glm_depths <- init$glm_depths
aux_states_init$surface_height <- init$surface_height




x_init <- init$x

enkf_output <- flare::run_EnKF(x_init,
                               obs = z,
                               psi,
                               combined_error,
                               working_directory,
                               met_file_names,
                               inflow_file_names,
                               outflow_file_names,
                               sim_start_datetime,
                               sim_end_datetime,
                               forecast_start_datetime,
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
                                           forecast_location)

#Create EML Metadata
flare::create_flare_eml(file_name = saved_file,
                        enkf_output)


#### END START ARCHIVE CONTAINER

