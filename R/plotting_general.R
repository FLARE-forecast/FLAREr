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

  output <- flare::combine_forecast_observations(file_name,
                                qaqc_location,
                                extra_historical_days = 0)
  obs <- output$obs
  full_time_local_extended <- output$full_time_local_extended
  diagnostic_list <- output$diagnostic_list
  state_list <- output$state_list
  forecast <- output$forecast
  par_list <- output$par_list
  obs_list <- output$obs_list
  state_names <- output$state_names
  par_names <- output$par_names
  diagnostics_names <- output$diagnostics_names
  full_time_local <- output$full_time_local
  obs_long <- output$obs_long
  depths <- output$depths
  obs_names <- output$obs_names


  if(length(which(forecast == 1)) > 0){
    forecast_index <- which(forecast == 1)[1]
  }else{
    forecast_index <- 0
  }

  focal_depths_plotting <- depths


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
      obs_curr <- as.numeric(c(t(obs[, ,obs_index])))
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
      ggplot2::facet_wrap(~depth) +
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

    p <- ggplot2::ggplot(curr_tibble, ggplot2::aes(y = depth, x = curr_var)) +
      ggplot2::facet_wrap(~factor(date)) +
      ggplot2::geom_ribbon(ggplot2::aes(xmin = upper_var, xmax = lower_var),
                           alpha = 0.70,
                           fill = "gray") +
      ggplot2::geom_path() +
      ggplot2::geom_point(ggplot2::aes(x = observed), size = 0.5, color = "red") +
      ggplot2::scale_y_reverse() +
      ggplot2::theme_light() +
      ggplot2::labs(y = "Depth(m)", x = state_names[i], title = state_names[i])
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

    obs_date <- tibble::tibble(date = lubridate::as_date(full_time_local))

    obs_secchi <- obs_long %>% dplyr::filter(variable == "secchi")

    if(nrow(obs_secchi) > 0){
      obs_curr <- dplyr::left_join(obs_date, obs_secchi)
      obs_curr <- obs_curr$value
    }else{
      obs_curr <- rep(NA, length(obs_date))
    }





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

