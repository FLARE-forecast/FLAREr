in_situ_qaqc <- function(insitu_obs_fname,
                         data_location,
                         maintenance_file,
                         ctd_fname,
                         nutrients_fname,
                         secchi_fname,
                         cleaned_observations_file_long,
                         lake_name_code,
                         code_folder,
                         config){




  d <- temp_oxy_chla_qaqc(realtime_file = insitu_obs_fname[1],
                          qaqc_file = insitu_obs_fname[2],
                          maintenance_file = maintenance_file,
                          input_file_tz = "EST",
                          focal_depths,
                          local_tzone = config$local_tzone)

  if(exists("ctd_fname")){
    if(!is.na(ctd_fname)){
      d_ctd <- extract_CTD(fname = ctd_fname,
                           input_file_tz = "EST",
                           local_tzone = config$local_tzone,
                           focal_depths = config$focal_depths)
      d <- rbind(d,d_ctd)
    }
  }
  if(exists("nutrients_fname")){
    if(!is.na(nutrients_fname)){
      d_nutrients <- extract_nutrients(fname = nutrients_fname,
                                       input_file_tz = "EST",
                                       local_tzone = config$local_tzone,
                                       focal_depths = config$focal_depths)
      d <- rbind(d,d_nutrients)
    }
  }
  if(exists("ch4_fname")){
    if(!is.na(ch4_fname)){
      d_ch4 <- extract_ch4(fname = ch4_fname,
                           input_file_tz = "EST",
                           local_tzone  = config$local_tzone,
                           focal_depths = config$focal_depths)
      d <- rbind(d,d_ch4)
    }
  }

  obs_methods_temp <- cbind(config$obs_config$method_1,config$obs_config$method_2,config$obs_config$method_3,config$obs_config$method_4)
  obs_methods <- list()
  for(i in 1:nrow(obs_methods_temp)){

    values <- obs_methods_temp[i,which(!is.na(obs_methods_temp[i,]))]
    if(length(values) == 0){
      values <- NA
    }
    obs_methods[[i]] <- values
  }


  first_day <- lubridate::as_datetime(paste0(lubridate::as_date(min(d$timestamp)), " ", config$run_config$start_time_local))
  first_day <- lubridate::force_tz(first_day, tzone = config$local_tzone)

  last_day <- lubridate::as_datetime(paste0(lubridate::as_date(max(d$timestamp)), " ", config$run_config$start_time_local))
  last_day <- lubridate::force_tz(last_day, tzone = config$local_tzone)

  full_time_local <- seq(first_day, last_day, by = "1 day")

  d_clean <- NULL
  for(i in 1:length(config$obs_config$state_names_obs)){
    print(paste0("Extracting ",config$obs_config$target_variable[i]))
    depth_breaks <- c(config$modeled_depths - config$obs_config$distance_threshold[i],max(config$modeled_depths) + config$obs_config$distance_threshold[i])
    time_breaks <- c(full_time_local - config$obs_config$time_threshold[i], max(full_time_local) + config$obs_config$time_threshold[i])

    d_curr <- d %>%
      dplyr::filter(variable == config$obs_config$target_variable[i],
                    method %in% obs_methods[[i]]) %>%
      dplyr::mutate(depth_class = cut(depth, breaks = depth_breaks, labels = NULL, right = TRUE)) %>%
      dplyr::mutate(time_class = cut(timestamp, breaks = time_breaks, labels = FALSE)) %>%
      dplyr::group_by(time_class, depth_class) %>%
      dplyr::summarize(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(depth = config$modeled_depths[depth_class]) %>%
      dplyr::mutate(datetime = full_time_local[time_class]) %>%
      dplyr::mutate(variable = config$obs_config$target_variable[i]) %>%
      dplyr::select(datetime, depth, variable, value) %>%
      dplyr::rename(timestamp = datetime)

      d_clean <- rbind(d_clean,  d_curr)
  }

  d_clean <- d_clean %>% tidyr::drop_na(value)

  if(!is.na(config$secchi_fname)){

    d_secchi <- extract_secchi(fname = file.path(config$data_location, config$secchi_fname),
                               input_file_tz = "EST",
                               local_tzone  = config$local_tzone,
                               focal_depths = config$focal_depths)

    d_clean <- rbind(d_clean,d_secchi)
  }

  readr::write_csv(d_clean, cleaned_observations_file_long)
}
