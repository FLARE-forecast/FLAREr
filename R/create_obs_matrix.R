##' @param cleaned_observations_file_long
##'
##' @param obs_config
##' @param config
##'
##' @title Create matrix of observations in the format required by run_ENKF
##'
##' @export
##'
##' @author Quinn Thomas
##'
##'

create_obs_matrix <- function(cleaned_observations_file_long,
                              obs_config,
                              config){

  start_datetime <- lubridate::as_datetime(config$run_config$start_datetime)
  if(is.na(config$run_config$forecast_start_datetime)){
    end_datetime <- lubridate::as_datetime(config$run_config$end_datetime)
    forecast_start_datetime <- end_datetime
  }else{
    forecast_start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime)
    end_datetime <- forecast_start_datetime + lubridate::days(config$run_config$forecast_horizon)
  }

  full_time <- seq(start_datetime, end_datetime, by = "1 day")

  d <- readr::read_csv(cleaned_observations_file_long, col_types = readr::cols(date = col_date(format = ""),
                                                                               hour = col_double(),
                                                                               depth = col_double(),
                                                                               value = col_double(),
                                                                               variable = col_character(),
                                                                               method = col_character()))

  obs_list <- list()
  for(i in 1:length(obs_config$state_names_obs)){
    message("Extracting ",obs_config$target_variable[i])

    obs_tmp <- array(NA,dim = c(length(full_time),length(config$modeled_depths)))

    for(k in 1:length(full_time)){
      for(j in 1:length(config$modeled_depths)){
        d1 <- d %>%
          dplyr::filter(variable == obs_config$target_variable[i])
        if(nrow(d1) == 0){
          warning("No observations for ", obs_config$target_variable[i])
        }
        d1 <- d1 %>%
          dplyr::filter(date == lubridate::as_date(full_time[k]))
        if(nrow(d1) == 0){
          warning("No observations for ", obs_config$target_variable[i], " on ", lubridate::as_date(full_time[k]))
        }
        d1 <- d1 %>%
          dplyr::filter((is.na(hour) | hour == lubridate::hour(full_time[k])))
        if(nrow(d1) == 0){
          warning("No observations for ", obs_config$target_variable[i], " on ", lubridate::as_date(full_time[k]),
               " at ", lubridate::hour(full_time[k]), ":00:00")
        }
        d1 <- d1 %>%
          dplyr::filter(abs(depth-config$modeled_depths[j]) < obs_config$distance_threshold[i])
        if(nrow(d1) == 0){
          # warning("No observations for ", obs_config$target_variable[i], " on ", lubridate::as_date(full_time[k]),
               # " at ", lubridate::hour(full_time[k]), ":00:00", " within ", obs_config$distance_threshold[i],
          # "m of the modeled depth ", config$modeled_depths[j], "m")
        }
        if(nrow(d1) >= 1){
          if(nrow(d1) > 1){
            warning("There are multiple observations for ", obs_config$target_variable[i], "\nUsing the first observation")
          }
          obs_tmp[k,j] <- d1$value[1]
        }
      }
    }

    # Check for NAs
    if(sum(is.na(obs_tmp)) == (dim(obs_tmp)[1] * dim(obs_tmp)[2]) ) {
      warning("All values are NA for ", obs_config$target_variable[i])
    }

    obs_list[[i]] <- obs_tmp
  }

  ####################################################
  #### STEP 7: CREATE THE Z ARRAY (OBSERVATIONS x TIME)
  ####################################################

  obs <- array(NA, dim = c(length(obs_config$state_names_obs), length(full_time), length(config$modeled_depths)))
  for(i in 1:nrow(obs_config)){
    obs[i , , ] <-  obs_list[[i]]
  }

  full_time_forecast <- seq(start_datetime, end_datetime, by = "1 day")
  obs[ , which(full_time_forecast > forecast_start_datetime), ] <- NA

  return(obs)
}
