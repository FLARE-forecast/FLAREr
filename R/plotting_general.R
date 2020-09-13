##' @param file_name
##'
##' @param qaqc_location
##'
##' @title Plotting FLARE output
##' @return None
##'
##' @export
##'
##' @author Quinn Thomas
##'
##'

plotting_general <- function(file_name,
                             qaqc_location){

  pdf_file_name <- paste0(tools::file_path_sans_ext(file_name),".pdf")

  nc <- ncdf4::nc_open(file_name)
  t <- ncdf4::ncvar_get(nc,'time')
  local_tzone <- ncdf4::ncatt_get(nc, 0)$local_time_zone_of_simulation
  full_time <- as.POSIXct(t,
                          origin = '1970-01-01 00:00.00 UTC',
                          tz = "UTC")
  full_time_local <- lubridate::with_tz(full_time, local_tzone)
  full_time_day_local <- lubridate::as_date(full_time_local)
  nsteps <- length(full_time_day_local)
  data_assimilation <- ncdf4::ncvar_get(nc, 'data_assimilation')
  depths <- round(ncdf4::ncvar_get(nc, 'depth'),2)


  focal_depths_plotting <- depths

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



  if(length(which(data_assimilation == 0)) > 0){
    forecast_index <- which(data_assimilation == 0)[1]
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

  #PROCESS TEMPERATURE OBSERVATIONS

  cleaned_observations_file_long <- paste0(qaqc_location,
                                           "/observations_postQAQC_long.csv")
  d <- readr::read_csv(cleaned_observations_file_long,
                       col_types = readr::cols())

  #####

  obs_list <- list()
  for(i in 1:length(obs_names)){
    print(paste0("Extracting ",target_variable[i]))

    obs_tmp <- array(NA,dim = c(length(full_time_local),length(depths)))

    for(k in 1:length(full_time_local)){
      for(j in 1:length(depths)){
        d1 <- d %>%
          dplyr::filter(variable == target_variable[i],
                        date == lubridate::as_date(full_time_local[k]),
                        (is.na(hour) | hour == lubridate::hour(full_time_local[k])),
                        abs(depth-depths[j]) < distance_threshold[i])

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

  obs <- array(NA, dim = c(nsteps, length(depths), length(obs_names)))
  for(i in 1:length(obs_names)){
    obs[ , , i] <-  obs_list[[i]]
  }


  if(length(focal_depths_plotting) < 4){
    plot_height <- 3
  }else{
    plot_height <- 8
  }
  pdf(pdf_file_name,width = 11, height = plot_height)

  for(i in 1:length(state_names)){

    curr_var <- state_list[[i]]
    print(state_names[i])


    mean_var <- array(NA, dim = c(length(depths), length(full_time_local)))
    upper_var <- array(NA, dim = c(length(depths), length(full_time_local)))
    lower_var <- array(NA,dim = c(length(depths), length(full_time_local)))
    for(j in 1:length(full_time_local)){
      for(ii in 1:length(depths)){
        mean_var[ii, j] <- mean(curr_var[j,ii , ], na.rm = TRUE)
        upper_var[ii, j] <- quantile(curr_var[j,ii , ], 0.1, na.rm = TRUE)
        lower_var[ii, j] <- quantile(curr_var[j,ii , ], 0.9, na.rm = TRUE)
      }
    }

    date <- c()
    for(j in 1:length(full_time_local)){
      date <- c(date, rep(full_time_local[j], length(depths)))
    }

    if(state_names[i] %in% unlist(obs_names)){
      obs_index <- which(obs_names == state_names[i])
      obs_curr <- c(t(obs[, ,obs_index]))
    }else{
      obs_curr <- as.numeric(rep(NA, length(date)))
    }

    curr_tibble <- tibble::tibble(date = lubridate::as_datetime(date),
                                  curr_var = c(mean_var),
                                  upper_var = c(upper_var),
                                  lower_var = c(lower_var),
                                  observed = obs_curr,
                                  depth = rep(depths, length(full_time_local))) %>%
      dplyr::filter(depth %in% focal_depths_plotting)

    if(forecast_index > 0){
      forecast_start_day <- full_time_local[forecast_index-1]
      forecast_start_day_alpha <- 1.0
    }else{
      forecast_start_day <- dplyr::last(full_time_local)
      forecast_start_day_alpha <- 0.0
    }

    p <- ggplot2::ggplot(curr_tibble, ggplot2::aes(x = date)) +
      ggplot2::facet_wrap(~depth, scales = "free") +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = lower_var, ymax = upper_var),
                           alpha = 0.70,
                           fill = "gray") +
      ggplot2::geom_line(ggplot2::aes(y = curr_var), size = 0.5) +
      ggplot2::geom_vline(xintercept = forecast_start_day,
                          alpha = forecast_start_day_alpha) +
      ggplot2::geom_point(ggplot2::aes(y = observed), size = 0.5, color = "red") +
      ggplot2::theme_light() +
      ggplot2::labs(x = "Date", y = state_names[i], title = state_names[i]) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 10))
    print(p)
  }

  if(length(par_names) > 0){
    plist <- list()

    for(i in 1:length(par_names)){

      print(par_names[i])


      curr_var <- par_list[[i]]

      mean_var <- array(NA, dim = c(length(full_time_local)))
      upper_var <- array(NA, dim = c(length(full_time_local)))
      lower_var <- array(NA, dim = c(length(full_time_local)))
      for(j in 1:length(full_time_local)){
        mean_var[j] <- mean(curr_var[j, ])
        upper_var[j] <- quantile(curr_var[j, ], 0.1, na.rm = TRUE)
        lower_var[j] <- quantile(curr_var[j, ], 0.9, na.rm = TRUE)
      }

      date <- full_time_local

      if(forecast_index > 0){
        forecast_start_day <- full_time_local[forecast_index-1]
        forecast_start_day_alpha <- 1.0
      }else{
        forecast_start_day <- dplyr::last(full_time_local)
        forecast_start_day_alpha <- 0.0
      }

      curr_tibble <- tibble::tibble(date = lubridate::as_datetime(date),
                                    curr_var = c(mean_var),
                                    upper_var = c(upper_var),
                                    lower_var = c(lower_var))

      plist[[i]] <- ggplot2::ggplot(curr_tibble, ggplot2::aes(x = date)) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = lower_var, ymax = upper_var),
                             alpha = 0.70,
                             fill = "gray") +
        ggplot2::geom_line(ggplot2::aes(y = curr_var), size = 0.5) +
        ggplot2::geom_vline(xintercept = forecast_start_day,
                            alpha = forecast_start_day_alpha) +
        ggplot2::theme_bw() +
        ggplot2::labs(x = "Date", y = par_names[i]) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 10))
    }

    print(patchwork::wrap_plots(plist))
  }

  if(length(diagnostics_names) > 0 )
    for(i in 1:length(diagnostics_names)){
      print(diagnostics_names[i])
      curr_var <- diagnostic_list[[i]]




      mean_var <- array(NA, dim = c(length(depths), length(full_time_local)))
      upper_var <- array(NA, dim = c(length(depths), length(full_time_local)))
      lower_var <- array(NA,dim = c(length(depths), length(full_time_local)))
      for(j in 1:length(full_time_local)){
        for(ii in 1:length(depths)){
          mean_var[ii, j] <- mean(curr_var[j,ii , ], na.rm = TRUE)
          upper_var[ii, j] <- quantile(curr_var[j,ii , ], 0.1, na.rm = TRUE)
          lower_var[ii, j] <- quantile(curr_var[j,ii , ], 0.9, na.rm = TRUE)
        }
      }

      date <- c()
      for(j in 1:length(full_time_local)){
        date <- c(date, rep(full_time_local[j], length(depths)))
      }

      curr_tibble <- tibble::tibble(date = lubridate::as_datetime(date),
                                    curr_var = c(mean_var),
                                    upper_var = c(upper_var),
                                    lower_var = c(lower_var),
                                    depth = rep(depths, length(full_time_local))) %>%
        dplyr::filter(depth %in% focal_depths_plotting)

      if(forecast_index > 0){
        forecast_start_day <- full_time_local[forecast_index-1]
        forecast_start_day_alpha <- 1.0
      }else{
        forecast_start_day <- dplyr::last(full_time_local)
        forecast_start_day_alpha <- 0.0
      }

      p <- ggplot2::ggplot(curr_tibble, ggplot2::aes(x = date)) +
        ggplot2::facet_wrap(~depth) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = lower_var, ymax = upper_var),
                             alpha = 0.70,
                             fill = "gray") +
        ggplot2::geom_line(ggplot2::aes(y = curr_var), size = 0.5) +
        ggplot2::geom_vline(xintercept = forecast_start_day,
                            alpha = forecast_start_day_alpha) +
        ggplot2::theme_light() +
        ggplot2::labs(x = "Date", y = diagnostics_names[i], title = diagnostics_names[i]) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 10))
      print(p)
    }

  if("extc_coef" %in% diagnostics_names){

    print("secchi")

    obs_curr <- rep(NA, length(full_time_local))




    i <- which(diagnostics_names == "extc_coef")
    ii <- which.min(abs(depths-1.0))
    curr_var <- diagnostic_list[[i]]


    mean_var <- array(NA, dim = c(length(full_time_local)))
    upper_var <- array(NA, dim = c(length(full_time_local)))
    lower_var <- array(NA,dim = c(length(full_time_local)))
    for(j in 1:length(full_time_local)){
      sechi <- 1.7 / curr_var[j,ii , ]
      mean_var[j] <- mean(sechi, na.rm = TRUE)
      upper_var[j] <- quantile(sechi, 0.1, na.rm = TRUE)
      lower_var[j] <- quantile(sechi, 0.9, na.rm = TRUE)
    }


    curr_tibble <- tibble::tibble(date = lubridate::as_datetime(full_time_local),
                                  curr_var = c(mean_var),
                                  upper_var = c(upper_var),
                                  lower_var = c(lower_var),
                                  observed = unlist(obs_curr))

    if(forecast_index > 0){
      forecast_start_day <- full_time_local[forecast_index-1]
      forecast_start_day_alpha <- 1.0
    }else{
      forecast_start_day <- dplyr::last(full_time_local)
      forecast_start_day_alpha <- 0.0
    }

    p <- ggplot2::ggplot(curr_tibble, ggplot2::aes(x = date)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = lower_var, ymax = upper_var),
                           alpha = 0.70,
                           fill = "gray") +
      ggplot2::geom_line(ggplot2::aes(y = curr_var), size = 0.5) +
      ggplot2::scale_y_reverse() +
      ggplot2::geom_vline(xintercept = forecast_start_day,
                          alpha = forecast_start_day_alpha) +
      ggplot2::geom_point(ggplot2::aes(y = observed), size = 1, color = "red") +
      ggplot2::theme_light() +
      ggplot2::labs(x = "Date", y = "Sechi depth (m)", title = "Sechi depth") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 10))
    print(p)
  }

  dev.off()

}

