#' Title
#'
#' @param file_name
#' @param qaqc_location
#' @param extra_historical_days
#'
#' @return
#' @export
#'
#' @examples
combine_forecast_observations <- function(file_name, qaqc_location,  extra_historical_days, ncore = 1){

  nc <- ncdf4::nc_open(file_name)
  t <- ncdf4::ncvar_get(nc,'time')
  local_tzone <- ncdf4::ncatt_get(nc, 0)$local_time_zone_of_simulation
  full_time <- as.POSIXct(t,
                          origin = '1970-01-01 00:00.00 UTC',
                          tz = "UTC")
  full_time_day <- lubridate::as_date(full_time)
  nsteps <- length(full_time_day)
  forecast <- ncdf4::ncvar_get(nc, 'forecast')
  depths <- round(ncdf4::ncvar_get(nc, 'depth'),2)

  var_names <- names(nc$var)
  combined_states <- list()
  combined_states_conversion <- list()
  obs_methods <- list()
  output_type <- rep(NA, length(var_names))
  target_variable <- rep(NA, length(var_names))
  time_threshold <- rep(NA, length(var_names))
  distance_threshold <- rep(NA, length(var_names))
  for(i in 1:length(var_names)){
    tmp <- ncdf4::ncatt_get(nc, varid = var_names[i],attname = "long_name")$value
    output_type[i] <- stringr::str_split(tmp, ":")[[1]][1]
    combined_states[i] <- c(stringr::str_split(stringr::str_split(tmp, ":")[[1]][3], "-")[1])
    combined_states_conversion[i] <- list(as.numeric(unlist(stringr::str_split(stringr::str_split(tmp, ":")[[1]][4], "-")[1])))
    target_variable[i] <- list((unlist(stringr::str_split(stringr::str_split(tmp, ":")[[1]][5], "-")[1])))
    distance_threshold[i] <- list(as.numeric(unlist(stringr::str_split(stringr::str_split(tmp, ":")[[1]][6], "-")[1])))
  }

  wq_names <- var_names[output_type  == "state"]
  combined_states_conversion_index <- which(stringr::str_detect(var_names, "total") | stringr::str_detect(var_names, "PHY_TCHLA_observed"))
  if(length(combined_states_conversion_index) > 0){
    combined_states <- combined_states[combined_states_conversion_index]
    combined_states_conversion <- combined_states_conversion[combined_states_conversion_index]


    #tmo <- stringr::str_split(var_names[combined_states_conversion_index],"_")
    combined_states_names <- stringr::str_split(var_names[combined_states_conversion_index], "_")
    for(i in 1:length(combined_states)){

      tmp <- combined_states_names[[i]][which(combined_states_names[[i]] != 'observed')]
      combined_states_names[i] <- tmp[1]
      if(length(tmp) > 1){
        for(j in 2:length(tmp)){
          combined_states_names[i] <- paste0(combined_states_names[i], "_",tmp[j])
        }
      }
    }

    names(combined_states) <- combined_states_names
    names(combined_states_conversion) <- combined_states_names

    state_names <- c(wq_names, names(combined_states))
  }else{
    combined_states <- NULL
    combined_states_conversion <- NULL
    state_names <- wq_names
  }

  diagnostics_names <- var_names[output_type  == "diagnostic"]

  obs_names <- stringr::str_split(var_names[output_type  == "observed"],"_")


  for(i in 1:length(obs_names)){
    tmp <- obs_names[[i]][which(obs_names[[i]] != 'observed')]
    obs_names[i] <- tmp[1]
    if(length(tmp) > 1){
      for(j in 2:length(tmp)){
        obs_names[i] <- paste0(obs_names[i], "_",tmp[j])
      }
    }
  }

  obs_methods <- obs_methods[output_type  == "observed"]
  target_variable <- target_variable[output_type  == "observed"]
  time_threshold <- time_threshold[output_type  == "observed"]
  distance_threshold <- distance_threshold[output_type  == "observed"]


  par_names <- var_names[output_type  == "parameter"]



  if(length(which(forecast == 1)) > 0){
    forecast_index <- which(forecast == 1)[1]
  }else{
    forecast_index <- 0
  }


  par_list <- list()
  if(length(par_names) > 0){
    for(par in 1:length(par_names)){
      par_list[[par]] <- ncdf4::ncvar_get(nc, par_names[par])
    }
  }

  state_list <- list()
  for(s in 1:length(wq_names)){
    state_list[[s]] <- ncdf4::ncvar_get(nc, wq_names[s])
  }

  if(length(combined_states) > 0){
    for(i in 1:length(combined_states)){
      for(s in 1:length(combined_states[[i]])){
        if(s > 1){
          tmp_list <- tmp_list + ncdf4::ncvar_get(nc, combined_states[[i]][s]) * combined_states_conversion[[i]][s]
        }else{
          tmp_list <- ncdf4::ncvar_get(nc, combined_states[[i]][s]) * combined_states_conversion[[i]][s]
        }
      }
      state_list[[length(wq_names)+i]] <- tmp_list
    }
  }

  names(state_list) <- state_names

  diagnostic_list <- list()
  for(s in 1:length(diagnostics_names)){
    diagnostic_list[[s]] <- ncdf4::ncvar_get(nc, diagnostics_names[s])
  }

  names(diagnostic_list) <- diagnostics_names

  ncdf4::nc_close(nc)

  #PROCESS TEMPERATURE OBSERVATIONS

  cleaned_observations_file_long <- paste0(qaqc_location,
                                           "/observations_postQAQC_long.csv")
  d <- readr::read_csv(cleaned_observations_file_long,
                       col_types = readr::cols())

  #####

  full_time_extended <- seq(full_time[1] - lubridate::days(extra_historical_days), max(full_time), by = "1 day")


  switch(Sys.info() [["sysname"]],
         Linux = { machine <- "unix" },
         Darwin = { machine <- "mac" },
         Windows = { machine <- "windows"})
  if(machine == "windows") {
    cl <- parallel::makeCluster(ncore, setup_strategy = "sequential")
    parallel::clusterEvalQ(cl,library(magrittr))
  } else {
    cl <- parallel::makeCluster(ncore, setup_strategy = "sequential")
    parallel::clusterEvalQ(cl,library(magrittr))
  }
  # Close parallel sockets on exit even if function is crashed or cancelled
  on.exit({
    tryCatch({parallel::stopCluster(cl)},
             error = function(e) {
               return(NA)
             })
  })

  parallel::clusterExport(cl, varlist = list("obs_names", "full_time", "target_variable", "depths", "full_time_extended", "d", "distance_threshold"),
                          envir = environment())

  obs_list <- parallel::parLapply(cl, 1:length(obs_names), function(i) {

    #obs_list <- lapply(1:length(obs_names), function(i) {

    print(paste0("Extracting ",target_variable[i]))

    obs_tmp <- array(NA,dim = c(length(full_time_extended),length(depths)))

    for(k in 1:length(full_time_extended)){
      #print(k)
      for(j in 1:length(depths)){

        #print(j)
        d1 <- d %>%
          dplyr::filter(variable == target_variable[i],
                        date == lubridate::as_date(full_time_extended[k]),
                        (is.na(hour) | hour == lubridate::hour(full_time_extended[k])),
                        abs(depth-depths[j]) < distance_threshold[i])



        if(nrow(d1) == 1){
          obs_tmp[k,j] <- d1$value
        }else if(nrow(d1) > 1){
          obs_tmp[k,j] <- mean(d1$value, na.rm = TRUE)
        }else{
          obs_tmp[k,j] <- NA
        }
        #print(nrow(d1))
        #print(obs_tmp[k,j])
      }
    }
    return(obs_tmp)
  })


  ####################################################
  #### STEP 7: CREATE THE Z ARRAY (OBSERVATIONS x TIME)
  ####################################################

  obs <- array(NA, dim = c(length(full_time_extended), length(depths), length(obs_names)))
  for(i in 1:length(obs_names)){
    obs[ , , i] <-  obs_list[[i]]
  }

  return(list(obs = obs,
              full_time_extended = full_time_extended,
              diagnostic_list = diagnostic_list,
              state_list = state_list,
              forecast = forecast,
              par_list = par_list,
              obs_list = obs_list,
              state_names = state_names,
              par_names = par_names,
              diagnostics_names = diagnostics_names,
              full_time = full_time,
              obs_long = d,
              depths = depths,
              obs_names = obs_names))
}

