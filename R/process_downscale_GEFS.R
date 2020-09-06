# --------------------------------------
# purpose: run downscaling processes
# Creator: Laura Puckett, December 21 2018
# contact: plaura1@vt.edu
# --------------------------------------
# summary: this calls all other functions required to downscale future GEFS forecasts to a specific site using the relationship between saved forecasts and site observations
# --------------------------------------

#' @title Download and Downscale NOAA GEFS for a single site
#' @return None
#'
#' @param site_index, index of site_list, lat_list, lon_list to be downloaded
#' @param lat_list, vector of latitudes that correspond to site codes
#' @param lon_list, vector of longitudes that correspond to site codes
#' @param site_list, vector of site codes, used in directory and file name generation
#' @param downscale, logical specifying whether to downscale from 6-hr to 1-hr
#' @param overwrite, logical stating to overwrite any existing output_file
#' @param model_name, directory name for the 6-hr forecast, this will be used in directory and file name generation
#' @param model_name_ds, directory name for the 1-hr forecast, this will be used in directory and file name generation
#' @param output_directory, directory where the model output will be save
#' @export
#'
#' @author Quinn Thomas
#'

process_downscale_GEFS <- function(folder,
                                   noaa_location,
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



