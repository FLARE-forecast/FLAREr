##' @title Combine forecast output with observations
##' @details Combines forecast output with observations
##' @param file_name string; full path to output file produced by write_forecast_netcdf()
##' @param qaqc_data_directory string; full path to processed long-format observation file
##' @param extra_historical_days integer; number of days prior to the days in the file_name file to include in the plot
##' @param ncore integer; number of computer cores for parallel processing
##' @return list
##' @import ncdf4
##' @import lubridate
##' @import parallel
##' @import readr
##' @importFrom stringr str_split str_detect

combine_forecast_observations <- function(file_name,
                                          target_file,
                                          extra_historical_days,
                                          ncore = 1,
                                          depth_threshold = 0.15){

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
  output_type <- rep(NA, length(var_names))
  target_name <- rep(NA, length(var_names))
  for(i in 1:length(var_names)){
    tmp <- ncdf4::ncatt_get(nc, varid = var_names[i],attname = "long_name")$value
    output_type[i] <- stringr::str_split(tmp, ":")[[1]][1]
    target_name[i] <- stringr::str_split(tmp, ":")[[1]][2]
  }

  state_names <- var_names[output_type  == "state"]
  obs_names <- var_names[which(output_type == "state" & !is.na(target_name))]
  obs_names_targets <- target_name[which(output_type == "state" & !is.na(target_name))]


  diagnostics_names <- var_names[output_type  == "diagnostic"]

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
  for(s in 1:length(state_names)){
    state_list[[s]] <- ncdf4::ncvar_get(nc, state_names[s])
  }

  names(state_list) <- state_names


  diagnostic_list <- list()
  if(length(diagnostics_names) > 0){
  for(s in 1:length(diagnostics_names)){
    diagnostic_list[[s]] <- ncdf4::ncvar_get(nc, diagnostics_names[s])
  }

  names(diagnostic_list) <- diagnostics_names
  }

  ncdf4::nc_close(nc)

  #PROCESS TEMPERATURE OBSERVATIONS

  d <- readr::read_csv(target_file,
                       show_col_types = FALSE)

  if("observed" %in% names(d)){
    d <- d %>%
      dplyr::rename(value = observed)
  }else if("observation" %in% names(d)){
    d <- d %>%
      dplyr::rename(value = observation)
  }

  if("time" %in% names(d)){
    d <- d %>%
      mutate(hour = lubridate::hour(time),
             date = lubridate::as_date(time))
  }else if("datetime" %in% names(d)){
    d <- d %>%
      mutate(hour = lubridate::hour(datetime),
             date = lubridate::as_date(datetime))
  }

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


  parallel::clusterExport(cl, varlist = list("obs_names", "full_time", "depths", "full_time_extended", "d", "obs_names_targets", "depth_threshold"),
                          envir = environment())

  obs_list <- parallel::parLapply(cl, 1:length(obs_names), function(i) {

    #obs_list <- lapply(1:length(obs_names), function(i) {

    message(paste0("Extracting ",obs_names[i]))

    obs_tmp <- array(NA,dim = c(length(full_time_extended),length(depths)))

    for(k in 1:length(full_time_extended)){
      #print(k)
      for(j in 1:length(depths)){

        #print(j)
        d1 <- d %>%
          dplyr::filter(variable == obs_names_targets[i],
                        date == lubridate::as_date(full_time_extended[k]),
                        (is.na(hour) | hour == lubridate::hour(full_time_extended[k])),
                        abs(depth-depths[j]) < depth_threshold)

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

