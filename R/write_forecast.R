##' @title Generate parquet output file
##' @details Function generates a parquet file from the object that is returned by run_da_forecast()
##' @param da_forecast_output list; object that is returned by run_da_forecast()
##' @param use_s3 Boolen; use s3 storage for saving scores
##' @param bucket S3 bucket
##' @param endpoint S3 endpoint
##' @param local_directory local directory of scores if not using s3
##' @return None
##' @importFrom lubridate with_tz
##' @author Quinn Thomas
##' @keywords internal
##'

write_forecast <- function(da_forecast_output,
                                 use_s3 = FALSE,
                                 bucket = NULL,
                                 endpoint = NULL,
                                 local_directory = NULL){


  if(use_s3){
    if(is.null(bucket) | is.null(endpoint)){
      stop("scoring function needs bucket and endpoint if use_s3=TRUE")
    }

    vars <- arrow_env_vars()
    output_directory <- arrow::s3_bucket(bucket = bucket,
                                         endpoint_override =  endpoint)
    on.exit(unset_arrow_vars(vars))
  }else{
    if(is.null(local_directory)){
      stop("scoring function needs local_directory if use_s3=FALSE")
    }
    output_directory <- arrow::SubTreeFileSystem$create(local_directory)
  }

  x <- da_forecast_output$states_depth
  pars <- da_forecast_output$pars
  lake_depth <- da_forecast_output$lake_depth
  snow_ice_thickness <- da_forecast_output$snow_ice_thickness
  data_assimilation_flag <- da_forecast_output$data_assimilation_flag
  forecast_flag <- da_forecast_output$forecast_flag
  full_time <- da_forecast_output$full_time
  forecast_start_datetime <- da_forecast_output$forecast_start_datetime
  config <- da_forecast_output$config
  states_config <- da_forecast_output$states_config
  obs_config <- da_forecast_output$obs_config
  pars_config <- da_forecast_output$pars_config
  diagnostics <- da_forecast_output$diagnostics
  diagnostics_daily <- da_forecast_output$diagnostics_daily
  log_particle_weights <- da_forecast_output$log_particle_weights

  forecast_flag[which(is.na(forecast_flag))] <- 0

  if(!("multi_depth" %in% names(obs_config))){
    obs_config <- obs_config |> dplyr::mutate(multi_depth = 1)
  }

  obs_config <- obs_config |>
    dplyr::filter(multi_depth == 1)

  indexes <- expand.grid(time = 1:dim(x)[1], states = 1:dim(x)[2], depths = 1:dim(x)[3])
  ensembles <- 1:dim(x)[4]

  if(config$model_settings$ncore == 1){
    future::plan("future::sequential", workers = config$model_settings$ncore)
  }else{
    future::plan("future::multisession", workers = config$model_settings$ncore)
  }

  output_list <- furrr::future_map_dfr(1:nrow(indexes), function(i, indexes){
    var1 <- indexes$time[i]
    var2 <- indexes$states[i]
    var3 <- indexes$depths[i]
    tibble::tibble(predicted = x[var1, var2, var3, ],
                   time  = full_time[var1],
                   depth = config$model_settings$modeled_depths[var3],
                   variable = states_config$state_names[var2],
                   forecast = forecast_flag[var1],
                   ensemble = ensembles,
                   variable_type = "state",
                   log_weight = log_particle_weights[var1, ])
  },
  indexes = indexes
  )

  tmp_index <- 0
  for(s in 1:length(obs_config$state_names_obs)){
    if(!(obs_config$state_names_obs[s] %in% states_config$state_names) &
       obs_config$multi_depth[s] == 1){
      tmp_index <- tmp_index + 1
      first_index <- 1
      for(ii in 1:length(states_config$state_names)){
        if(s %in% states_config$states_to_obs[[ii]]){
          temp_index <- which(states_config$states_to_obs[[ii]] == s)
          if(first_index == 1){
            temp_var <- x[ ,ii , , ] * states_config$states_to_obs_mapping[[ii]][temp_index]
            first_index <- 2
          }else{
            temp_var <- temp_var + x[, ii, , ] * states_config$states_to_obs_mapping[[ii]][temp_index]
          }
        }
      }

      indexes <- expand.grid(time = 1:dim(temp_var)[1], depth = 1:dim(temp_var)[2])

      output_list_tmp <- furrr::future_map_dfr(1:nrow(indexes), function(i, indexes){
        var1 <- indexes$time[i]
        var3 <- indexes$depth[i]
        tibble::tibble(predicted = temp_var[var1, var3, ],
                       time  = full_time[var1],
                       depth = config$model_settings$modeled_depths[var3],
                       variable = obs_config$target_variable[s],
                       forecast = forecast_flag[var1],
                       ensemble = ensembles,
                       variable_type = "state",
                       log_weight = log_particle_weights[var1, ])
      },
      indexes = indexes
      )
      output_list <- bind_rows(output_list, output_list_tmp)
    }
  }


  if(length(config$output_settings$diagnostics_names) > 0){

    indexes <- expand.grid(diag = 1:dim(diagnostics)[1], time = 1:dim(diagnostics)[2], depth = 1:dim(diagnostics)[3])

    tmp <- furrr::future_map_dfr(1:nrow(indexes), function(i, indexes){
      var1 <- indexes$diag[i]
      var2 <- indexes$time[i]
      var3 <- indexes$depth[i]
      tibble::tibble(predicted = diagnostics[var1, var2, var3, ],
                     time  = full_time[var2],
                     depth = config$model_settings$modeled_depths[var3],
                     variable = config$output_settings$diagnostics_names[var1],
                     forecast = forecast_flag[var2],
                     ensemble = ensembles,
                     variable_type = "diagnostic",
                     log_weight = log_particle_weights[var2, ])
    },
    indexes = indexes
    )
    output_list <- dplyr::bind_rows(output_list, tmp)
  }



  if(length(config$output_settings$diagnostics_daily$names) > 0){

    indexes <- expand.grid(diag = 1:dim(diagnostics_daily)[1], time = 1:dim(diagnostics_daily)[2])

    tmp <- furrr::future_map_dfr(1:nrow(indexes), function(i, indexes){
      var1 <- indexes$diag[i]
      var2 <- indexes$time[i]
      tibble::tibble(predicted = diagnostics_daily[var1, var2, ],
                     time  = full_time[var2],
                     depth = NA,
                     variable = config$output_settings$diagnostics_daily$save_names[var1],
                     forecast = forecast_flag[var2],
                     ensemble = ensembles,
                     variable_type = "diagnostic",
                     log_weight = log_particle_weights[var2, ])
    },
    indexes = indexes
    )
    output_list <- dplyr::bind_rows(output_list, tmp)
  }

  if(!is.null(pars)){
    indexes <- expand.grid(time = 1:dim(pars)[1], par = 1:dim(pars)[2])

    tmp <- furrr::future_map_dfr(1:nrow(indexes), function(i, indexes){
      var1 <- indexes$time[i]
      var2 <- indexes$par[i]
      tibble::tibble(predicted = pars[var1, var2, ],
                     time  = full_time[var1],
                     depth = NA,
                     variable = pars_config$par_names_save[var2],
                     forecast = forecast_flag[var1],
                     ensemble = ensembles,
                     variable_type = "parameter",
                     log_weight = log_particle_weights[var1, ])
    },
    indexes = indexes
    )
    output_list <- dplyr::bind_rows(output_list, tmp)
  }

  if(!is.null(da_forecast_output$restart_list)){
    lake_depth <- da_forecast_output$restart_list$lake_depth
  }

  output_list3 <- furrr::future_map_dfr(1:dim(lake_depth)[1], function(i){
    tibble::tibble(predicted = lake_depth[i, ],
                   time  = full_time[i],
                   variable = "depth",
                   depth = NA,
                   forecast = forecast_flag[i],
                   ensemble = 1:dim(lake_depth)[2],
                   variable_type = "state",
                   log_weight = log_particle_weights[i, ])
  })
  output_list <- dplyr::bind_rows(output_list, output_list3)

  if(length(config$output_settings$diagnostics_names) > 0){
    tmp <- furrr::future_map_dfr(1:dim(diagnostics)[2], function(i){
      tibble::tibble(predicted = 1.7 / diagnostics[1, i, which.min(abs(config$model_settings$modeled_depths-1.0)), ],
                     time = full_time[i],
                     variable = "secchi",
                     depth = NA,
                     forecast = forecast_flag[i],
                     ensemble = 1:dim(diagnostics)[4],
                     variable_type = "state",
                     log_weight = log_particle_weights[i, ])
    })
    output_list <- dplyr::bind_rows(output_list, tmp)
  }

  tmp <- furrr::future_map_dfr(1:dim(snow_ice_thickness)[2], function(i){
    tibble::tibble(predicted = apply(snow_ice_thickness[2:3, i, ], 2, sum),
                   time = full_time[i],
                   variable = "ice_thickness",
                   depth = NA,
                   forecast = forecast_flag[i],
                   ensemble = 1:dim(snow_ice_thickness)[3],
                   variable_type = "state",
                   log_weight = log_particle_weights[i, ])
  })

  output_list <- dplyr::bind_rows(output_list, tmp)

  time_of_forecast <- lubridate::with_tz(da_forecast_output$time_of_forecast, tzone = "UTC")

  output_list <- output_list |>
    dplyr::mutate(pub_datetime = time_of_forecast,
                  reference_datetime = forecast_start_datetime,
                  site_id = config$location$site_id,
                  model_id = config$run_config$sim_name,
                  family = "ensemble") |>
    rename(datetime = time,
           parameter = ensemble,
           prediction = predicted) |>
    dplyr::select(reference_datetime, datetime, pub_datetime, model_id, site_id, depth, family, parameter, variable, prediction, forecast, variable_type, log_weight)

  #Convert to target variable name
  for(i in 1:length(states_config$state_names)){
    if(length(which(obs_config$state_names_obs == states_config$state_names[i])) >0){
      obs_name <- obs_config$target_variable[which(obs_config$state_names_obs == states_config$state_names[i])]
      output_list <- output_list |>
        dplyr::mutate(variable = ifelse(variable == states_config$state_names[i], obs_name, variable))
    }
  }

  output_list <- output_list |>
    mutate(reference_date = lubridate::as_date(reference_datetime))

  message("starting writing dataset")
  arrow::write_dataset(dataset = output_list,
                       path = output_directory,
                       partitioning = c("site_id", "model_id","reference_date"))
  message("ending writing dataset")
  return(output_list)
}
