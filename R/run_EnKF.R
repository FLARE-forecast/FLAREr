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

run_EnKF <- function(x_init,
                     obs,
                     psi,
                     process_sd,
                     working_directory,
                     met_file_names,
                     inflow_file_names,
                     outflow_file_names,
                     sim_start_datetime,
                     sim_end_datetime,
                     forecast_start_datetime = NA,
                     management_input,
                     wq_start,
                     wq_end,
                     config,
                     aux_states_init,
                     code_folder
){

  if(is.na(forecast_start_datetime)){
    forecast_start_datetime <- sim_end_datetime
  }

  hist_days <- as.numeric(forecast_start_datetime - sim_start_datetime)
  start_forecast_step <- 1 + hist_days
  full_time_local <- seq(sim_start_datetime, sim_end_datetime, by = "1 day")


  npars <- nrow(config$pars_config)
  nstates <- dim(x_init)[2] -  npars
  nsteps <- length(full_time_local)
  nmembers <- dim(x_init)[1]
  n_met_members <- length(met_file_names)
  ndepths_modeled <- length(config$modeled_depths)

  data_assimilation_flag <- rep(NA, nsteps)

  x <- array(NA, dim=c(nsteps, nmembers, nstates + npars))

  x[1, ,]  <- x_init

  q_v <- rep(NA, ndepths_modeled)
  w <- rep(NA, ndepths_modeled)

  alpha_v <- 1 - exp(-config$vert_decorr_length)


  if(config$include_wq){
    num_wq_vars <- length(wq_start)
  }

  if(length(config$diagnostics_names) > 0){
    diagnostics <- array(NA, dim=c(nsteps, nmembers, ndepths_modeled, length(config$diagnostics_names)))
  }else{
    diagnostics <- NA
  }

  num_phytos <- length(which(stringr::str_detect(config$states_config$state_names,"PHY_") & !stringr::str_detect(config$states_config$state_names,"_IP") & !stringr::str_detect(config$states_config$state_names,"_IN")))

  full_time_local_char <- strftime(full_time_local,
                                   format="%Y-%m-%d %H:%M",
                                   tz = config$local_tzone)

  x_prior <- array(NA, dim = c(nsteps, nmembers, nstates + npars))

  glm_salt <- array(NA, dim = c(nmembers, 500))

  flare::set_up_model(executable_location = paste0(find.package("flare"),"/exec/"),
                      config,
                      working_directory,
                      num_wq_vars)

  mixing_vars <- array(NA, dim = c(nsteps, nmembers, 17))
  glm_depths <- array(NA, dim = c(nsteps, nmembers, 500))
  surface_height <- array(NA, dim = c(nsteps, nmembers))
  snow_ice_thickness <- array(NA, dim = c(nsteps, nmembers, 3))
  avg_surf_temp <- array(NA, dim = c(nsteps, nmembers))

  mixing_vars[1, ,] <- aux_states_init$mixing_vars
  glm_depths[1, ,] <- aux_states_init$glm_depths
  surface_height[1, ] <- aux_states_init$surface_height
  snow_ice_thickness[1, , ] <- aux_states_init$snow_ice_thickness
  avg_surf_temp[1, ] <- aux_states_init$avg_surf_temp

  ###START EnKF
  for(i in 2:nsteps){

    curr_start <- strftime(full_time_local[i - 1],
                           format="%Y-%m-%d %H:%M",
                           tz = config$local_tzone)
    curr_stop <- strftime(full_time_local[i],
                          format="%Y-%m-%d %H:%M",
                          tz = config$local_tzone)

    print(paste0("Running time step ", i-1, " : ",
                 curr_start, " - ",
                 curr_stop))

    setwd(working_directory)

    met_index <- 1
    inflow_outflow_index <- 1


    #Create array to hold GLM predictions for each ensemble
    x_star <- array(NA, dim = c(nmembers, nstates))
    x_corr <- array(NA, dim = c(nmembers, nstates))

    #Matrix to store calculated ensemble specific deviations and innovations
    dit <- array(NA, dim = c(nmembers, nstates))

    if(npars > 0){
      pars_corr <-  array(NA, dim = c(nmembers, npars))
      dit_pars<- array(NA, dim = c(nmembers, npars))
    }

    # Start loop through ensemble members
    for(m in 1:nmembers){

      curr_met_file <- met_file_names[met_index]


      if(npars > 0){
        curr_pars <- x[i - 1, m , (nstates+1):(nstates+ npars)]
      }

      out <- flare::run_model(i,
                       m,
                       mixing_vars_start = mixing_vars[i-1, m, ],
                       curr_start,
                       curr_stop,
                       par_names = config$pars_config$par_names,
                       curr_pars,
                       working_directory,
                       par_nml = config$pars_config$par_nml,
                       num_phytos,
                       glm_depths_start = glm_depths[i-1, m, ],
                       surface_height_start = surface_height[i-1, m],
                       simulate_SSS = config$simulate_SSS,
                       x_start = x[i-1, m, ],
                       full_time_local,
                       wq_start,
                       wq_end,
                       management_input,
                       hist_days,
                       forecast_sss_on = config$forecast_sss_on,
                       sss_depth =  config$sss_depth,
                       use_specified_sss =  config$use_specified_sss,
                       modeled_depths = config$modeled_depths,
                       ndepths_modeled,
                       curr_met_file,
                       inflow_file_names,
                       inflow_outflow_index,
                       outflow_file_names,
                       glm_output_vars = config$glm_output_vars,
                       diagnostics_names = config$diagnostics_names,
                       machine =  config$machine,
                       npars,
                       num_wq_vars,
                       snow_ice_thickness_start = snow_ice_thickness[i-1, m, ],
                       avg_surf_temp_start = avg_surf_temp[i-1, m],
                       nstates,
                       states_config = config$states_config,
                       include_wq = config$include_wq,
                       specified_sss_inflow_file = config$specified_sss_inflow_file,
                       specified_sss_outflow_file =  config$specified_sss_outflow_file,
                       data_location = config$data_location)

      x_star[m, ] <- out$x_star_end
      surface_height[i ,m ] <- out$surface_height_end
      snow_ice_thickness[i,m ,] <- out$snow_ice_thickness_end
      avg_surf_temp[i , m] <- out$avg_surf_temp_end
      mixing_vars[i, m, ] <- out$mixing_vars_end
      diagnostics[i, m, , ] <- out$diagnostics_end
      glm_depths[i, m,] <- out$glm_depths_end

      ########################################
      #END GLM SPECIFIC PART
      ########################################

      #INCREMENT ThE MET_INDEX TO MOVE TO ThE NEXT NOAA ENSEMBLE
      met_index <- met_index + 1
      if(met_index > length(met_file_names)){
        met_index <- 1
      }

      inflow_outflow_index <- inflow_outflow_index + 1
      if(inflow_outflow_index > nrow(inflow_file_names)){
        inflow_outflow_index <- 1
      }


      #Add process noise

      q_v[] <- NA
      w[] <- NA
      for(jj in 1:length(process_sd)){
        w[] <- rnorm(ndepths_modeled, 0, 1)
        q_v[1] <- process_sd[jj] * w[1]
        for(kk in 2:ndepths_modeled){
          q_v[kk] <- alpha_v * q_v[kk-1] + sqrt(1 - alpha_v^2) * process_sd[jj] * w[kk]
        }

        x_corr[m, (((jj-1)*ndepths_modeled)+1):(jj*ndepths_modeled)] <-
          x_star[m, (((jj-1)*ndepths_modeled)+1):(jj*ndepths_modeled)] + q_v
      }

    } # END ENSEMBLE LOOP


    #Correct any negative water quality states
    if(config$include_wq & config$no_negative_states){
      for(m in 1:nmembers){
        index <- which(x_corr[m,] < 0.0)
        x_corr[m, index[which(index <= wq_end[num_wq_vars] & index >= wq_start[1])]] <- 0.0
      }
    }

    if(npars > 0){
      pars_corr <- x[i - 1, , (nstates + 1):(nstates+ npars)]
      if(npars == 1){
        pars_corr <- matrix(pars_corr,nrow = length(pars_corr),ncol = 1)
      }
      pars_star <- pars_corr
    }

    if(npars > 0){
        x_prior[i, , ] <- cbind(x_corr, pars_corr)
    }else{
        x_prior[i, , ] <- x_corr
    }

    z_index <- which(!is.na(c(obs[i, , ])))

    #if no observations at a time step then just propogate model uncertainity

    if(length(z_index) == 0 |
       i > (hist_days + 1) |
       hist_days == 0){

      if(i > (hist_days + 1)){
      data_assimilation_flag[i] <- 0
      }else if(i <= (hist_days + 1) & config$use_obs_constraint){
        data_assimilation_flag[i] <- 3
      }else{
        data_assimilation_flag[i] <- 1
      }

      if(npars > 0){

        if(i > (hist_days + 1)){
          #don't add the noise to parameters in future forecast mode (pars_star doesn't have noise)
          x[i, , ] <- cbind(x_corr, pars_star)
        }else{
          #add the noise to parameters if in data assimilation mode
          x[i, , ] <- cbind(x_corr, pars_corr)
        }

        if(config$process_uncertainty == FALSE & i > (hist_days + 1)){
          #don't add process noise if process uncertainty is false (x_star doesn't have noise)
          #don't add the noise to parameters in future forecast mode ()
          x[i, , ] <- cbind(x_star, pars_star)
        }

        if(i == (hist_days + 1) & config$initial_condition_uncertainty == FALSE){
          for(m in 1:nmembers){
            x[i, m, ] <- c(colMeans(x_star), pars_star[m, ])
          }
        }

      }else{
        if(i > (hist_days + 1)){
          x[i, , ] <- cbind(x_corr)
        }else{
          x[i, , ] <- x_prior[i, , ]
        }

        if(config$process_uncertainty == FALSE & i > (hist_days + 1)){
          x[i, , ] <- x_star
        }

      }
    }else{

      data_assimilation_flag[i] <- 7

      #if observation then calucate Kalman adjustment
      zt <- c(obs[i, ,])
      zt <- zt[which(!is.na(zt))]

      #Assign which states have obs in the time step
      h <- matrix(0, nrow = length(config$obs_config$state_names_obs) * ndepths_modeled, ncol = nstates)

       index <- 0
       for(k in 1:((nstates/ndepths_modeled))){
         for(j in 1:ndepths_modeled){
           index <- index + 1
           if(!is.na(dplyr::first(config$states_config$states_to_obs[[k]]))){
             for(jj in 1:length(config$states_config$states_to_obs[[k]])){
               if(!is.na((obs[i, j, config$states_config$states_to_obs[[k]][jj]]))){
                 states_to_obs_index <- config$states_config$states_to_obs[[k]][jj]
                 index2 <- (states_to_obs_index - 1) * ndepths_modeled + j
                 h[index2,index] <- config$states_config$states_to_obs_mapping[[k]][jj]
               }
             }
           }
         }
       }

      z_index <- c()
      for(j in 1:nrow(h)){
        if(sum(h[j, ]) > 0){
          z_index <- c(z_index, j)
        }
      }

      h <- h[z_index, ]

      #Extract the data uncertainity for the data
      #types present during the time-step

      if(ncol(as.matrix(psi)) > 1){
        curr_psi <- psi[z_index, 1] + psi[z_index, 2] * zt
      }else{
        curr_psi <- psi[z_index]
      }

      curr_psi <- curr_psi ^ 2

      if(length(z_index) > 1){
        psi_t <- diag(curr_psi)
      }else{
        #Special case where there is only one data
        #type during the time-step
        psi_t <- curr_psi
      }

      d_mat <- t(mvtnorm::rmvnorm(n = nmembers, mean = zt, sigma=as.matrix(psi_t)))

      #Set any negative observations of water quality variables to zero
      d_mat[which(z_index > length(config$modeled_depths) & d_mat < 0.0)] <- 0.0

      #Ensemble mean
      ens_mean <- apply(x_corr[,], 2, mean)

      if(npars > 0){
        par_mean <- apply(pars_corr, 2, mean)
        for(m in 1:nmembers){
          pars_corr[m, ] <- config$pars_config$inflat_pars * (pars_corr[m,] - par_mean) + par_mean
        }
        par_mean <- apply(pars_corr, 2, mean)
      }


      #Loop through ensemble members
      for(m in 1:nmembers){
        #  #Ensemble specific deviation
        dit[m, ] <- x_corr[m, ] - ens_mean
        if(npars > 0){
          dit_pars[m, ] <- pars_corr[m, ] - par_mean
        }
        if(m == 1){
          p_it <- dit[m, ] %*% t(dit[m, ])
          if(npars > 0){
            p_it_pars <- dit_pars[m, ] %*% t(dit[m, ])
          }
        }else{
          p_it <- dit[m, ] %*% t(dit[m, ]) +  p_it
          if(npars > 0){
            p_it_pars <- dit_pars[m, ] %*% t(dit[m, ]) + p_it_pars
          }
        }
      }

      #estimate covariance
      p_t <- p_it / (nmembers - 1)
      if(npars > 0){
        p_t_pars <- p_it_pars / (nmembers - 1)
      }

      if(!is.na(config$localization_distance)){
        p_t <- localization(p_t, nstates, config$modeled_depths, num_wq_vars, wq_start, wq_end)
      }
      #Kalman gain
      k_t <- p_t %*% t(h) %*% solve(h %*% p_t %*% t(h) + psi_t, tol = 1e-17)
      if(npars > 0){
        k_t_pars <- p_t_pars %*% t(h) %*% solve(h %*% p_t %*% t(h) + psi_t, tol = 1e-17)
      }

      #Update states array (transposes are necessary to convert
      #between the dims here and the dims in the EnKF formulations)
      update_increment <-  k_t %*% (d_mat - h %*% t(x_corr))
      x[i, , 1:nstates] <- t(t(x_corr) + update_increment)
      if(npars > 0){
        x[i, , (nstates+1):(nstates+npars)] <- t(t(pars_corr) +
                                                   k_t_pars %*% (d_mat - h %*% t(x_corr)))
      }
    }

    #IF NO INITIAL CONDITION UNCERTAINITY THEN SET EACH ENSEMBLE MEMBER TO THE MEAN
    #AT THE INITIATION OF ThE FUTURE FORECAST
    if(i == (hist_days + 1)){

      if(config$initial_condition_uncertainty == FALSE){
        state_means <- colMeans(x[i, ,1:nstates])
        for(m in 1:nmembers){
          x[i, m, 1:nstates]  <- state_means
        }
      }
      if(npars > 0){
        if(config$parameter_uncertainty == FALSE){
          par_means <- colMeans(x[i, ,(nstates + 1):(nstates + npars)])
          for(m in 1:nmembers){
            x[i, m, (nstates + 1):(nstates + npars)] <- par_means
          }
        }
      }
    }

    ###################
    ## Quality Control Step
    ##################

    #Correct any negative water quality states
    if(config$include_wq & config$no_negative_states){
      for(m in 1:nmembers){
        index <- which(x[i,m,] < 0.0)
        x[i, m, index[which(index <= wq_end[num_wq_vars] & index >= wq_start[1])]] <- 0.0
      }
    }

    #Correct any parameter values outside bounds
    if(npars > 0){
      for(par in 1:npars){
        low_index <- which(x[i, ,nstates + par] < config$pars_config$par_lowerbound[par])
        high_index <- which(x[i, ,nstates + par] > config$pars_config$par_upperbound[par])
        x[i,low_index ,nstates + par] <- config$pars_config$par_lowerbound[par]
        x[i,high_index ,nstates + par] <- config$pars_config$par_upperbound[par]
      }
    }

    ###############

    #Print parameters to screen
    if(npars > 0){
      for(par in 1:npars){
        print(paste0(config$pars_config$par_names_save[par],": mean ",
                     round(mean(pars_corr[,par]),4)," sd ",
                     round(sd(pars_corr[,par]),4)))
      }
    }

    # Save the states after the last historical (data assimilation)
    # time step (before forecasting)
    if(i == (hist_days + 1)){
      x_restart <- x[i, , ]
      qt_restart <- qt
      surface_height_restart <- surface_height[i, ]
      snow_ice_restart <- snow_ice_thickness[i, , ]
      avg_surf_temp_restart <- avg_surf_temp[i, ]
      mixing_restart <- mixing_vars[i, ,]
      glm_depths_restart <- glm_depths[i, , ]

    }else if(hist_days == 0 & i == 2){
      x_restart <- x[1, , ]
      qt_restart <- qt
      surface_height_restart <- surface_height[i, ]
      snow_ice_restart <- snow_ice_thickness[i, , ]
      avg_surf_temp_restart <- avg_surf_temp[i, ]
      mixing_restart <- mixing_vars[i, ,]
      glm_depths_restart <- glm_depths[i, , ]
    }
  }

  if(lubridate::day(full_time_local[1]) < 10){
    file_name_H_day <- paste0("0",lubridate::day(full_time_local[1]))
  }else{
    file_name_H_day <- lubridate::day(full_time_local[1])
  }
  if(lubridate::day(full_time_local[hist_days+1]) < 10){
    file_name_F_day <- paste0("0",lubridate::day(full_time_local[hist_days+1]))
  }else{
    file_name_F_day <- lubridate::day(full_time_local[hist_days+1])
  }
  if(lubridate::month(full_time_local[1]) < 10){
    file_name_H_month <- paste0("0",lubridate::month(full_time_local[1]))
  }else{
    file_name_H_month <- lubridate::month(full_time_local[1])
  }
  if(lubridate::month(full_time_local[hist_days+1]) < 10){
    file_name_F_month <- paste0("0",lubridate::month(full_time_local[hist_days+1]))
  }else{
    file_name_F_month <- lubridate::month(full_time_local[hist_days+1])
  }

  save_file_name <- paste0(run_config$sim_name, "_H_",
                           (lubridate::year(full_time_local[1])),"_",
                           file_name_H_month,"_",
                           file_name_H_day,"_",
                           (lubridate::year(full_time_local[hist_days+1])),"_",
                           file_name_F_month,"_",
                           file_name_F_day,"_F_",
                           forecast_days)

  time_of_forecast <- Sys.time()
  curr_day <- lubridate::day(time_of_forecast)
  curr_month <- lubridate::month(time_of_forecast)
  curr_year <- lubridate::year(time_of_forecast)
  curr_hour <- lubridate::hour(time_of_forecast)
  curr_minute <- lubridate::minute(time_of_forecast)
  curr_second <- round(lubridate::second(time_of_forecast),0)
  if(curr_day < 10){curr_day <- paste0("0",curr_day)}
  if(curr_month < 10){curr_month <- paste0("0",curr_month)}
  if(curr_hour < 10){curr_hour <- paste0("0",curr_hour)}
  if(curr_minute < 10){curr_minute <- paste0("0",curr_minute)}
  if(curr_second < 10){curr_second <- paste0("0",curr_second)}

  forecast_iteration_id <- paste0(curr_year,
                                  curr_month,
                                  curr_day,
                                  "T",
                                  curr_hour,
                                  curr_minute,
                                  curr_second)


  return(list(full_time_local = full_time_local,
              forecast_start_datetime = forecast_start_datetime,
              x = x,
              obs = obs,
              save_file_name = save_file_name,
              forecast_iteration_id = forecast_iteration_id,
              time_of_forecast = time_of_forecast,
              x_restart = x_restart,
              qt_restart = qt_restart,
              x_prior = x_prior,
              surface_height_restart = surface_height_restart,
              snow_ice_restart = snow_ice_restart,
              snow_ice_thickness = snow_ice_thickness,
              surface_height = surface_height,
              avg_surf_temp_restart = avg_surf_temp_restart,
              mixing_restart = mixing_restart,
              glm_depths_restart = glm_depths_restart,
              diagnostics = diagnostics,
              data_assimilation_flag = data_assimilation_flag,
              config = config,
              met_file_names = met_file_names))
}
