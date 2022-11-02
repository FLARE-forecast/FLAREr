##' @title Generate parquet output file
##' @details Function generates a netcdf file from the object that is returned by run_da_forecast()
##' @param da_forecast_output list; object that is returned by run_da_forecast()
##' @param forecast_output_directory string; full path of directory where the csv file will be written
##' @param use_short_filename use shortened file name; this results in less information in the file name and potentially overwriting existing files
##' @return None
##' @export
##' @importFrom lubridate with_tz
##' @author Quinn Thomas
##' @examples
##' \dontrun{
##' write_forecast_csv(da_forecast_output = da_forecast_output, forecast_output_directory = config$file_path$forecast_output_directory, use_short_filename = TRUE)
##' }
##'

write_forecast_parquet <- function(da_forecast_output,
                               forecast_output_directory,
                               use_short_filename = TRUE){

  x <- da_forecast_output$x
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

  forecast_flag[which(is.na(forecast_flag))] <- 0

  indexes <- expand.grid(1:dim(x)[1], 1:dim(x)[2], 1:dim(x)[3])
  ensembles <- 1:dim(x)[4]

  output_list <- purrr::map_dfr(1:nrow(indexes), function(i, indexes){
    var1 <- indexes$Var1[i]
    var2 <- indexes$Var2[i]
    var3 <- indexes$Var3[i]
    tibble::tibble(predicted = x[var1, var2, var3, ],
                   time  = full_time[var1],
                   depth = config$model_settings$modeled_depths[var3],
                   variable = states_config$state_names[var2],
                   forecast = forecast_flag[var1],
                   ensemble = ensembles,
                   variable_type = "state")
  },
  indexes = indexes
  )

  tmp_index <- 0
  for(s in 1:length(obs_config$state_names_obs)){
    if(!obs_config$state_names_obs[s] %in% states_config$state_names){
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

    indexes <- expand.grid(1:dim(temp_var)[1], 1:dim(temp_var)[2])

    output_list_tmp <- purrr::map_dfr(1:nrow(indexes), function(i, indexes){
      var1 <- indexes$Var1[i]
      var3 <- indexes$Var2[i]
      tibble::tibble(predicted = temp_var[var1, var3, ],
                     time  = full_time[var1],
                     depth = config$model_settings$modeled_depths[var3],
                     variable = obs_config$target_variable[s],
                     forecast = forecast_flag[var1],
                     ensemble = ensembles,
                     variable_type = "state")
    },
    indexes = indexes
    )
    output_list <- bind_rows(output_list, output_list_tmp)
    }
  }


  if(length(config$output_settings$diagnostics_names) > 0){

    indexes <- expand.grid(1:dim(diagnostics)[1], 1:dim(diagnostics)[2], 1:dim(diagnostics)[3])

    output_list2 <- purrr::map_dfr(1:nrow(indexes), function(i, indexes){
      var1 <- indexes$Var1[i]
      var2 <- indexes$Var2[i]
      var3 <- indexes$Var3[i]
      tibble::tibble(predicted = diagnostics[var1, var2, var3, ],
                     time  = full_time[var1],
                     depth = config$model_settings$modeled_depths[var3],
                     variable = config$output_settings$diagnostics_names[var2],
                     forecast = forecast_flag[var1],
                     ensemble = ensembles,
                     variable_type = "diagnostic")
    },
    indexes = indexes
    )
    output_list <- dplyr::bind_rows(output_list, output_list2)
  }

  if(!is.null(pars)){
    indexes <- expand.grid(1:dim(pars)[1], 1:dim(pars)[2])

    output_list3 <- purrr::map_dfr(1:nrow(indexes), function(i, indexes){
      var1 <- indexes$Var1[i]
      var2 <- indexes$Var2[i]
      tibble::tibble(predicted = pars[var1, var2, ],
                     time  = full_time[var1],
                     depth = NA,
                     variable = pars_config$par_names_save[var2],
                     forecast = forecast_flag[var1],
                     ensemble = ensembles,
                     variable_type = "parameter")
    },
    indexes = indexes
    )
    output_list <- dplyr::bind_rows(output_list, output_list3)
  }

  if(!is.null(da_forecast_output$restart_list)){
    lake_depth <- da_forecast_output$restart_list$lake_depth
  }
  for(i in 1:dim(lake_depth)[1]){
    tmp <- tibble::tibble(predicted = lake_depth[i, ],
                          time  = full_time[i],
                          variable = "depth",
                          depth = NA,
                          forecast = forecast_flag[i],
                          ensemble = 1:dim(lake_depth)[2],
                          variable_type = "state")
    output_list <- dplyr::bind_rows(output_list, tmp)
  }


  if(length(config$output_settings$diagnostics_names) > 0){
    for(i in 1:dim(diagnostics)[2]){
      tmp <- tibble::tibble(predicted = 1.7 / diagnostics[1, i, which.min(abs(config$model_settings$modeled_depths-1.0)), ],
                            time = full_time[i],
                            variable = "secchi",
                            depth = NA,
                            forecast = forecast_flag[i],
                            ensemble = 1:dim(diagnostics)[4],
                            variable_type = "state")
      output_list <- dplyr::bind_rows(output_list, tmp)
    }
  }

  for(i in 1:dim(snow_ice_thickness)[1]){
    tmp <- tibble::tibble(predicted = apply(snow_ice_thickness[2:3, i, ], 2, sum),
                          time = full_time[i],
                          variable = "ice_thickness",
                          depth = NA,
                          forecast = forecast_flag[i],
                          ensemble = 1:dim(snow_ice_thickness)[3],
                          variable_type = "state")
    output_list <- dplyr::bind_rows(output_list, tmp)
  }

  time_of_forecast <- lubridate::with_tz(da_forecast_output$time_of_forecast, tzone = "UTC")

  output_list <- output_list %>%
    dplyr::mutate(pub_time = time_of_forecast,
                  reference_datetime = forecast_start_datetime,
                  site_id = config$location$site_id,
                  model_id = config$run_config$sim_name,
                  family = "ensemble") %>%
    rename(datetime = time,
           parameter = ensemble,
           prediction = predicted) %>%
    dplyr::select(reference_datetime, datetime, pub_time, model_id, site_id, depth, family, parameter, variable, prediction, forecast, variable_type)

  if(!use_short_filename | is.na(da_forecast_output$save_file_name_short) | length(which(forecast_flag == 1)) == 0){
    fname <- file.path(forecast_output_directory, paste0(da_forecast_output$save_file_name,".csv.gz"))
  }else{
    fname <- file.path(forecast_output_directory, paste0(da_forecast_output$save_file_name_short,".csv.gz"))
  }

  #Convert to target variable name
  for(i in 1:length(states_config$state_names)){
    if(length(which(obs_config$state_names_obs == states_config$state_names[i])) >0){
      obs_name <- obs_config$target_variable[which(obs_config$state_names_obs == states_config$state_names[i])]
      output_list <- output_list %>%
        dplyr::mutate(variable = ifelse(variable == states_config$state_names[i], obs_name, variable))
    }
  }



  reference_datetime_format <- "%Y-%m-%d %H:%M:%S"

  output_list <- output_list |> mutate(reference_datetime = strftime(lubridate::as_datetime(reference_datetime),format=reference_datetime_format,tz = "UTC"))
  if(config$run_config$use_s3){
  FLAREr:::arrow_env_vars()
  output_path <- arrow::s3_bucket(config$s3$forecasts_parquet$bucket, endpoint_override = config$s3$forecasts_parquet$endpoint)
  FLAREr:::unset_arrow_vars()
  }else{
    fs::dir_create(file.path(lake_directory, "forecasts","parquet"))
    output_path <- SubTreeFileSystem$create(file.path(lake_directory, "forecasts","parquet"))
    arrow::write_dataset(dataset = output_list,
                         path = local_dir,
                         partitioning = c("model_id","reference_datetime"))
  }
  arrow::write_dataset(dataset = output_list,
                       path = output_path,
                       partitioning = c("model_id","reference_datetime"))

  return(output_list)
}
