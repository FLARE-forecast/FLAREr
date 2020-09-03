plotting_general <- function(pdf_file_name,
                             output_file,
                             save_location,
                             qaqc_location){

  pars <- read_csv(par_file)

  obs_config <- read_csv(obs_config_file)

  states_config <- read_csv(states_config_file)

  par_names <- pars$par_names_save

  wq_names_potential <- c("temp",
                          "OXY_oxy",
                          "CAR_dic",
                          "CAR_ch4",
                          "SIL_rsi",
                          "NIT_amm",
                          "NIT_nit",
                          "PHS_frp",
                          "OGM_doc",
                          "OGM_docr",
                          "OGM_poc",
                          "OGM_don",
                          "OGM_donr",
                          "OGM_pon",
                          "OGM_dop",
                          "OGM_dopr",
                          "OGM_pop",
                          "NCS_ss1",
                          "PHS_frp_ads",
                          "PHY_cyano",
                          "PHY_cyano_IN",
                          "PHY_cyano_IP",
                          "PHY_green",
                          "PHY_green_IN",
                          "PHY_green_IP",
                          "PHY_diatom",
                          "PHY_diatom_IN",
                          "PHY_diatom_IP")

  combined_states_potential <- list(NIT_total = c("NIT_amm", "NIT_nit", "OGM_don","OGM_donr","OGM_pon"),
                          PHS_total = c("PHS_frp","OGM_dop","OGM_dopr","OGM_pop"),
                          #PHS_total = c("PHS_frp","OGM_dop","OGM_pop"),
                          OGM_doc_total = c("OGM_doc","OGM_docr"),
                          PHY_TCHLA = c("PHY_cyano","PHY_green","PHY_diatom"))

  diagnostics_potential <- c("extc_coef",
                             "PHY_cyano_fI",
                             "PHY_cyano_fNit",
                             "PHY_cyano_fPho",
                             "PHY_cyano_fT",
                             "PHY_green_fI",
                             "PHY_green_fNit",
                             "PHY_green_fPho",
                             "PHY_green_fT",
                             "PHY_diatom_fI",
                             "PHY_diatom_fNit",
                             "PHY_diatom_fPho",
                             "PHY_diatom_fT",
                             "rad")

  biomass_to_chla <- 1/states_config$states_to_obs_mapping_1[which(str_detect(states_config$state_names, "PHY"))]
  combined_states_conversion_potential <- list(NIT_total = c(1,1,1,1,1),
                                     PHS_total = c(1,1,1,1,1),
                                     OGM_doc_total = c(1,1),
                                     PHY_TCHLA = c(1/biomass_to_chla[1], 1/biomass_to_chla[2], 1/biomass_to_chla[3]))


  nc <- nc_open(output_file)
  t <- ncvar_get(nc,'time')
  local_tzone <- ncatt_get(nc, 0)$local_time_zone_of_simulation
  full_time <- as.POSIXct(t,
                                origin = '1970-01-01 00:00.00 UTC',
                                tz = "UTC")
  full_time_local <- with_tz(full_time, local_tzone)
  full_time_day_local <- as_date(full_time_local)
  nsteps <- length(full_time_day_local)
  data_assimilation <- ncvar_get(nc, 'data_assimilation')
  depths <- round(ncvar_get(nc, 'depth'),2)

  wq_names <- wq_names_potential[wq_names_potential %in% names(nc$var)]
  combined_states_conversion <- combined_states_conversion_potential[names(combined_states_conversion_potential) %in% obs_config$state_names_obs]
  combined_states <- combined_states_potential[names(combined_states_potential) %in% obs_config$state_names_obs]

  state_names <- c(wq_names, names(combined_states))

  diagnostics_names <- diagnostics_potential[diagnostics_potential %in% names(nc$var)]


  if(length(which(data_assimilation == 0)) > 0){
    forecast_index <- which(data_assimilation == 0)[1]
  }else{
    forecast_index <- 0
  }


  par_list <- list()
  if(length(par_names) > 0){
    for(par in 1:length(par_names)){
      par_list[[par]] <- ncvar_get(nc, par_names[par])
    }
  }

  state_list <- list()
  for(s in 1:length(wq_names)){
    state_list[[s]] <- ncvar_get(nc, wq_names[s])
  }

  if(length(combined_states) > 0){
  for(i in 1:length(combined_states)){
    for(s in 1:length(combined_states[[i]])){
      if(s > 1){
        tmp_list <- tmp_list + ncvar_get(nc, combined_states[[i]][s]) * combined_states_conversion[[i]][s]
      }else{
        tmp_list <- ncvar_get(nc, combined_states[[i]][s]) * combined_states_conversion[[i]][s]
      }
    }
    state_list[[length(wq_names)+i]] <- tmp_list
  }
  }

  names(state_list) <- state_names

  diagnostic_list <- list()
  for(s in 1:length(diagnostics_names)){
    diagnostic_list[[s]] <- ncvar_get(nc, diagnostics_names[s])
  }

  names(diagnostic_list) <- diagnostics_names

  #PROCESS TEMPERATURE OBSERVATIONS

  cleaned_observations_file_long <- paste0(qaqc_location,
                                           "/observations_postQAQC_long.csv")

  #####

  obs_methods_temp <- cbind(obs_config$method_1,obs_config$method_2,obs_config$method_3,obs_config$method_4)
  obs_methods <- list()
  for(i in 1:nrow(obs_methods_temp)){

    values <- obs_methods_temp[i,which(!is.na(obs_methods_temp[i,]))]
    if(length(values) == 0){
      values <- NA
    }
    obs_methods[[i]] <- values
  }
  obs_config$obs_methods <- obs_methods


  obs_list <- list()
  for(i in 1:length(obs_config$state_names_obs)){
    print(paste0("Extracting ",obs_config$target_variable[i]))
    obs_list[[i]] <- extract_observations(fname = cleaned_observations_file_long,
                                          full_time_local,
                                          modeled_depths = modeled_depths,
                                          local_tzone,
                                          target_variable = obs_config$target_variable[i],
                                          time_threshold_seconds = obs_config$time_threshold[i],
                                          distance_threshold_meter = obs_config$distance_threshold[i],
                                          methods = obs_config$obs_methods[[i]])
  }

  ####################################################
  #### STEP 7: CREATE THE Z ARRAY (OBSERVATIONS x TIME)
  ####################################################

  z <- array(NA, dim = c(nsteps, length(depths), length(obs_config$state_names_obs)))

  for(i in 1:nrow(obs_config)){
    z[ , , i] <-  obs_list[[i]]
  }

  if(length(focal_depths_plotting) < 4){
    plot_height <- 3
  }else{
    plot_height <- 8
  }
  pdf(paste0(save_location,'/',pdf_file_name, ".pdf"),width = 11, height = plot_height)

  for(i in 1:length(state_names)){

    curr_var <- state_list[[i]]


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

    if(state_names[i] %in% obs_config$state_names_obs){
      obs_index <- which(obs_config$state_names_obs == state_names[i])
      obs <- c(t(z[, ,obs_index]))
    }else{
      obs <- as.numeric(rep(NA, length(date)))
    }

    curr_tibble <- tibble(date = as_datetime(date),
                          curr_var = c(mean_var),
                          upper_var = c(upper_var),
                          lower_var = c(lower_var),
                          observed = obs,
                          depth = rep(depths, length(full_time_local))) %>%
      filter(depth %in% focal_depths_plotting)

    if(forecast_index > 0){
      forecast_start_day <- full_time_local[forecast_index-1]
      forecast_start_day_alpha <- 1.0
    }else{
      forecast_start_day <- last(full_time_local)
      forecast_start_day_alpha <- 0.0
    }

    p <- ggplot(curr_tibble, aes(x = date)) +
      facet_wrap(~depth, scales = "free") +
      geom_ribbon(aes(ymin = lower_var, ymax = upper_var),
                  alpha = 0.70,
                  fill = "gray") +
      geom_line(aes(y = curr_var), size = 0.5) +
      geom_vline(xintercept = forecast_start_day,
                 alpha = forecast_start_day_alpha) +
      geom_point(aes(y = observed), size = 0.5, color = "red") +
      theme_light() +
      labs(x = "Date", y = state_names[i], title = state_names[i]) +
      theme(axis.text.x = element_text(angle = 90, size = 10))
    print(p)
  }

  if(length(par_names) > 0){
    plist <- list()

    for(i in 1:length(par_names)){
      curr_var <- ncvar_get(nc, par_names[i])

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
        forecast_start_day <- last(full_time_local)
        forecast_start_day_alpha <- 0.0
      }

      curr_tibble <- tibble(date = as_datetime(date),
                            curr_var = c(mean_var),
                            upper_var = c(upper_var),
                            lower_var = c(lower_var))

      plist[[i]] <- ggplot(curr_tibble, aes(x = date)) +
        geom_ribbon(aes(ymin = lower_var, ymax = upper_var),
                    alpha = 0.70,
                    fill = "gray") +
        geom_line(aes(y = curr_var), size = 0.5) +
        geom_vline(xintercept = forecast_start_day,
                   alpha = forecast_start_day_alpha) +
        theme_bw() +
        labs(x = "Date", y = par_names[i]) +
        theme(axis.text.x = element_text(angle = 90, size = 10))
    }

    print(wrap_plots(plist))
  }

  if(length(diagnostics_names) > 0 )
    for(i in 1:length(diagnostics_names)){

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

      curr_tibble <- tibble(date = as_datetime(date),
                            curr_var = c(mean_var),
                            upper_var = c(upper_var),
                            lower_var = c(lower_var),
                            depth = rep(depths, length(full_time_local))) %>%
      filter(depth %in% focal_depths_plotting)

      if(forecast_index > 0){
        forecast_start_day <- full_time_local[forecast_index-1]
        forecast_start_day_alpha <- 1.0
      }else{
        forecast_start_day <- last(full_time_local)
        forecast_start_day_alpha <- 0.0
      }

      p <- ggplot(curr_tibble, aes(x = date)) +
        facet_wrap(~depth) +
        geom_ribbon(aes(ymin = lower_var, ymax = upper_var),
                    alpha = 0.70,
                    fill = "gray") +
        geom_line(aes(y = curr_var), size = 0.5) +
        geom_vline(xintercept = forecast_start_day,
                   alpha = forecast_start_day_alpha) +
        theme_light() +
        labs(x = "Date", y = diagnostics_names[i], title = diagnostics_names[i]) +
        theme(axis.text.x = element_text(angle = 90, size = 10))
      print(p)
    }

  if("extc_coef" %in% diagnostics_names){

    if(!is.na(secchi_fname)){
      obs <- read_csv(secchi_fname) %>%
        filter(Reservoir == "FCR" & Site == 50) %>%
        select(DateTime, Secchi_m) %>%
        mutate(DateTime = mdy_hm(DateTime)) %>%
        group_by(DateTime) %>%
          summarise(Secchi_m = mean(Secchi_m, na.rm = TRUE)) %>%
        mutate(date = as_date(DateTime))


      full_time_local_obs <- tibble(date = as_date(full_time_local))
      obs <- obs %>%
        right_join(full_time_local_obs, by = "date") %>%
        select(Secchi_m)
    }



    i <- which(diagnostics_names == "extc_coef")
    ii <- which.min(abs(depths-1.0))
    curr_var <- diagnostic_list[[i]]


    mean_var <- array(NA, dim = c(length(full_time_local)))
    upper_var <- array(NA, dim = c(length(full_time_local)))
    lower_var <- array(NA,dim = c(length(full_time_local)))
    for(j in 1:length(full_time_local)){
      sechi <- 1.7 / curr_var[j, , ii]
      mean_var[j] <- mean(sechi, na.rm = TRUE)
      upper_var[j] <- quantile(sechi, 0.1, na.rm = TRUE)
      lower_var[j] <- quantile(sechi, 0.9, na.rm = TRUE)
    }


    curr_tibble <- tibble(date = as_datetime(full_time_local),
                          curr_var = c(mean_var),
                          upper_var = c(upper_var),
                          lower_var = c(lower_var),
                          observed = unlist(obs))

    if(forecast_index > 0){
      forecast_start_day <- full_time_local[forecast_index-1]
      forecast_start_day_alpha <- 1.0
    }else{
      forecast_start_day <- last(full_time_local)
      forecast_start_day_alpha <- 0.0
    }

    p <- ggplot(curr_tibble, aes(x = date)) +
      geom_ribbon(aes(ymin = lower_var, ymax = upper_var),
                  alpha = 0.70,
                  fill = "gray") +
      geom_line(aes(y = curr_var), size = 0.5) +
      scale_y_reverse() +
      geom_vline(xintercept = forecast_start_day,
                 alpha = forecast_start_day_alpha) +
      geom_point(aes(y = observed), size = 1, color = "red") +
      theme_light() +
      labs(x = "Date", y = "Sechi depth (m)", title = "Sechi depth") +
      theme(axis.text.x = element_text(angle = 90, size = 10))
    print(p)
  }

  dev.off()

  if(file.exists(cleaned_observations_file_long)){
    unlink(cleaned_observations_file_long)
  }
}

