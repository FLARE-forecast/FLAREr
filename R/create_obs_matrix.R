##' @title Download and Downscale NOAA GEFS for a single site
##' @return None
##'
##' @param site_index, index of site_list, lat_list, lon_list to be downloaded
##' @param lat_list, vector of latitudes that correspond to site codes
##' @param lon_list, vector of longitudes that correspond to site codes
##' @param site_list, vector of site codes, used in directory and file name generation
##' @param downscale, logical specifying whether to downscale from 6-hr to 1-hr
##' @param overwrite, logical stating to overwrite any existing output_file
##' @param model_name, directory name for the 6-hr forecast, this will be used in directory and file name generation
##' @param model_name_ds, directory name for the 1-hr forecast, this will be used in directory and file name generation
##' @param output_directory, directory where the model output will be save
##' @export
##'
##' @author Quinn Thomas
##'
##'

create_obs_matrix <- function(cleaned_observations_file_long, config, start_datetime_local, end_datetime_local){

  full_time_local <- seq(start_datetime_local, end_datetime_local, by = "1 day")

  obs_methods_temp <- cbind(config$obs_config$method_1,config$obs_config$method_2,config$obs_config$method_3,config$obs_config$method_4)
  obs_methods <- list()
  for(i in 1:nrow(obs_methods_temp)){

    values <- obs_methods_temp[i,which(!is.na(obs_methods_temp[i,]))]
    if(length(values) == 0){
      values <- NA
    }
    obs_methods[[i]] <- values
  }
  config$obs_config$obs_methods <- obs_methods


  obs_list <- list()
  for(i in 1:length(config$obs_config$state_names_obs)){
    print(paste0("Extracting ",config$obs_config$target_variable[i]))
    obs_list[[i]] <- extract_observations(fname = cleaned_observations_file_long,
                                          start_datetime_local,
                                          end_datetime_local,
                                          modeled_depths = config$modeled_depths,
                                          local_tzone = config$local_tzone,
                                          target_variable = config$obs_config$target_variable[i],
                                          time_threshold_seconds = config$obs_config$time_threshold[i],
                                          distance_threshold_meter = config$obs_config$distance_threshold[i],
                                          methods = config$obs_config$obs_methods[[i]])
  }

  ####################################################
  #### STEP 7: CREATE THE Z ARRAY (OBSERVATIONS x TIME)
  ####################################################

  obs <- array(NA, dim = c(nsteps, ndepths_modeled, length(config$obs_config$state_names_obs)))
  for(i in 1:nrow(config$obs_config)){
    obs[ , , i] <-  obs_list[[i]]
  }

  if(!config$use_obs_constraint){
    obs[, , ] <- NA
  }

  return(obs)
}
