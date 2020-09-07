# --------------------------------------
# purpose: run downscaling processes
# Creator: Laura Puckett, December 21 2018
# contact: plaura1@vt.edu
# --------------------------------------
# summary: this calls all other functions required to downscale future GEFS forecasts to a specific site using the relationship between saved forecasts and site observations
# --------------------------------------

#' @param noaa_location
#'
#' @param input_met_file
#' @param working_directory
#' @param n_ds_members
#' @param n_met_members
#' @param file_name
#' @param local_tzone
#' @param FIT_PARAMETERS
#' @param DOWNSCALE_MET
#' @param met_downscale_uncertainty
#' @param compare_output_to_obs
#' @param VarInfo
#' @param replaceObsNames
#' @param downscaling_coeff
#' @param full_time_local
#' @param first_obs_date
#' @param last_obs_date
#' @param input_met_file_tz
#' @param weather_uncertainty
#' @param obs_met_outfile
#' @param lake_latitude
#' @param lake_longitude
#'
#' @title Download and Downscale NOAA GEFS for a single site
#' @return None
#'
#' @export
#'
#' @author Quinn Thomas
#' @author Laura Puckett
#'

process_downscale_GEFS <- function(noaa_location,
                                   input_met_file,
                                   working_directory,
                                   n_ds_members,
                                   n_met_members,
                                   file_name,
                                   local_tzone,
                                   FIT_PARAMETERS,
                                   DOWNSCALE_MET,
                                   met_downscale_uncertainty,
                                   compare_output_to_obs,
                                   VarInfo,
                                   replaceObsNames,
                                   downscaling_coeff,
                                   full_time_local,
                                   first_obs_date,
                                   last_obs_date,
                                   input_met_file_tz,
                                   weather_uncertainty,
                                   obs_met_outfile,
                                   lake_latitude,
                                   lake_longitude){

  # -----------------------------------
  # 1. Load & reformat observational data
  # -----------------------------------

  obs.file.path <- input_met_file
  for.file.path <- noaa_location

  obs.data <- readr::read_csv(obs.file.path, col_types = readr::cols())

  obs_met_glm <- readr::read_csv(paste0(working_directory,"/",obs_met_outfile))
  obs_met_glm$time <- lubridate::force_tz(obs_met_glm$time, tz = local_tzone)

  obs.data$timestamp <- lubridate::force_tz(obs.data$time, tz = local_tzone)

  VarNames <- as.vector(VarInfo$VarNames)

  observations <- obs.data %>%
    dplyr::mutate(AirTemp = AirTemp + 273.15,# convert from C to Kelvin
                  Rain = Rain* 60 * 24/1000)

  #observations$RelHum <- na.interpolation(observations$RelHum)
  #observations$AirTemp <- na.interpolation(observations$AirTemp)
  #observations$LongWave <- na.interpolation(observations$LongWave)
  #observations$WindSpeed <- na.interpolation(observations$WindSpeed)

  rm(obs.data)
  hrly.obs <- observations #%>% aggregate_obs_to_hrly()

  # -----------------------------------
  # 1. Fit Parameters
  # -----------------------------------

  if(FIT_PARAMETERS){
    print("Fit Parameters")
    fit_downscaling_parameters(observations,
                               for.file.path = noaa_location,
                               working_directory,
                               VarNames,
                               VarNamesStates,
                               replaceObsNames,
                               PLOT = FALSE,
                               local_tzone,
                               VarInfo,
                               first_obs_date,
                               last_obs_date,
                               lake_latitude,
                               lake_longitude)
  }
  # -----------------------------------
  # 2. Process GEFS
  # -----------------------------------
  met_forecast_output <- process_GEFS(file_name,
                       n_ds_members,
                       n_met_members,
                       in_directory = noaa_location,
                       out_directory = working_directory,
                       local_tzone,
                       VarInfo,
                       replaceObsNames,
                       hrly.observations = hrly.obs,
                       DOWNSCALE_MET,
                       FIT_PARAMETERS,
                       met_downscale_uncertainty,
                       WRITE_FILES = TRUE,
                       downscaling_coeff,
                       full_time_local,
                       weather_uncertainty,
                       obs_met_glm,
                       lake_latitude,
                       lake_longitude)

  files <- met_forecast_output[[1]]
  output <- met_forecast_output[[2]]


  #if(compare_output_to_obs == TRUE){
  #  "comparing forecast output to obs"
  #  compare_output_to_obs(output, hrly.obs)
  #}
  return(files)
}



