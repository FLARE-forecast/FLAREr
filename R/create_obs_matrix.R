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

  d <- readr::read_csv(cleaned_observations_file_long,
                       col_types = readr::cols())

  d$timestamp <- lubridate::with_tz(d$timestamp, tzone = config$local_tzone)

  obs_list <- list()
  for(i in 1:length(config$obs_config$state_names_obs)){
    print(paste0("Extracting ",config$obs_config$target_variable[i]))

    obs_tmp <- array(NA,dim = c(length(full_time_local),length(config$modeled_depths)))

    for(k in 1:length(full_time_local)){
      for(j in 1:length(config$modeled_depths)){
        d1 <- d %>%
          dplyr::filter(variable == config$obs_config$target_variable[i],
                        timestamp == full_time_local[k],
                        abs(depth-config$modeled_depths[j]) < config$obs_config$distance_threshold[i])

        if(nrow(d1) == 1){
          obs_tmp[k,j] <- d1$value
        }
      }
    }

    obs_list[[i]] <- obs_tmp
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
