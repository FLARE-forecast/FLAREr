#' @title Generating a list of flow files in the flare_tempdir

#' @param flow_forecast_dir location of the forecast files
#' @param flow_historical_dir location of the historical files
#' @param flow_type inflow or outflow
#' @param variables what variables are included in the flow file
#' @param out_dir the directory in which to put the flow files (e.g. flare_tempdir)
#' @param start_datetime start of simulation
#' @param end_datetime end of simulation
#' @param forecast_start_datetime start of the forecast period (break between historical + future periods)
#' @param forecast_horizon horizon
#' @param site_id site code
#' @param use_s3 logical
#' @param bucket s3 storage location
#' @param endpoint s3 storage location
#' @param local_directory local storage location
#' @param use_ler_vars T/F
#'
#' @return matrix of flow_file_names
#' @noRd
#'

create_flow_files <- function(flow_forecast_dir = NULL,
                              flow_historical_dir = NULL,
                              flow_type = "inflow",
                              variables = c("time", "FLOW", "TEMP", "SALT"),
                              out_dir,
                              start_datetime,
                              end_datetime = NA,
                              forecast_start_datetime = NA,
                              forecast_horizon = 0,
                              site_id,
                              use_s3 = FALSE,
                              bucket = NULL,
                              endpoint = NULL,
                              local_directory = NULL,
                              use_ler_vars = FALSE) {

  lake_name_code <- site_id

  round_level <- 10

  # set locations of flow drivers (s3 or local)
  if (!is.null(flow_forecast_dir) & !is.null(flow_historical_dir)) {
    if (use_s3) {
      if (is.null(bucket) | is.null(endpoint)) {
        stop("needs bucket and endpoint if use_s3=TRUE")
      }
      vars <- arrow_env_vars()
      future_s3 <- arrow::s3_bucket(bucket = file.path(bucket, flow_forecast_dir),
                                    endpoint_override = endpoint)
      hist_s3 <- arrow::s3_bucket(bucket = file.path(bucket,flow_historical_dir),
                                  endpoint_override = endpoint)
      unset_arrow_vars(vars)
    } else {
      if (is.null(local_directory)) {
        stop("needs local_directory if use_s3=FALSE")
      }
      future_s3 <- arrow::SubTreeFileSystem$create(file.path(local_directory, flow_forecast_dir))
      hist_s3 <-  arrow::SubTreeFileSystem$create(file.path(local_directory, flow_historical_dir))
    }
  }  else if (is.null(flow_forecast_dir) & !is.null(flow_historical_dir)) {
    if (use_s3) {
      if (is.null(bucket) | is.null(endpoint)) {
        stop("needs bucket and endpoint if use_s3=TRUE")
      }
      vars <- arrow_env_vars()
      future_s3 <- NULL
      hist_s3 <- arrow::s3_bucket(bucket = file.path(bucket,flow_historical_dir), endpoint_override = endpoint)
      unset_arrow_vars(vars)
    } else {
      if (is.null(local_directory)) {
        stop("needs local_directory if use_s3=FALSE")
      }
      future_s3 <- NULL
      hist_s3 <-  arrow::SubTreeFileSystem$create(file.path(local_directory, flow_historical_dir))
    }
  }else if (!is.null(flow_forecast_dir) & is.null(flow_historical_dir)) {
    if (use_s3) {
      if (is.null(bucket) | is.null(endpoint)) {
        stop("needs bucket and endpoint if use_s3=TRUE")
      }
      vars <- arrow_env_vars()
      hist_s3 <- NULL
      future_s3 <- arrow::s3_bucket(bucket = file.path(bucket,flow_forecast_dir), endpoint_override = endpoint)
      unset_arrow_vars(vars)
    } else {
      if (is.null(local_directory)) {
        stop("needs local_directory if use_s3=FALSE")
      }
      hist_s3 <- NULL
      future_s3 <-  arrow::SubTreeFileSystem$create(file.path(local_directory, flow_forecast_dir))
    }
  } else {
    future_s3 <- NULL
    hist_s3 <- NULL
  }

  # when does the simulation start and end?
  start_datetime <- lubridate::as_datetime(start_datetime)

  if (is.na(forecast_start_datetime)) {
    end_datetime <- lubridate::as_datetime(end_datetime)
    forecast_start_datetime <- end_datetime
  }  else {
    forecast_start_datetime <- lubridate::as_datetime(forecast_start_datetime)
    end_datetime <- forecast_start_datetime + lubridate::days(forecast_horizon)
  }


  # Access the data
  if (!is.null(future_s3)) {
    future_df <- dplyr::collect(arrow::open_dataset(future_s3)) |>
      filter(datetime >= forecast_start_datetime,
             datetime <= end_datetime) |>
      dplyr::distinct()
  } else {
    future_df <- NULL # No future data
  }

  if (!is.null(hist_s3)) {
    hist_df <- dplyr::collect(arrow::open_dataset(hist_s3)) |>
      dplyr::filter(datetime < forecast_start_datetime,
             datetime >= start_datetime) |>
      dplyr::distinct()


    if(!("parameter" %in% colnames(hist_df))){
      hist_df <- hist_df |> mutate(parameter = 1)
    }

    if("observation" %in% colnames(hist_df)){
      hist_df <- hist_df |> rename(prediction = observation)
    }

  } else {
    hist_df <- NULL # No historical data
  }



  if (!is.null(future_df) & !is.null(hist_df)) { # when there is historical and future data
    if (!setequal(unique(future_df$flow_number), unique(hist_df$flow_number))) { # Checks the data are consistent across the periods (same number of flows)
        print(tail(future_df))
        print(tail(hist_df))
            stop('need the same number of flows in historical and future periods')
    } else {
      num_flows <- max(future_df$flow_number)
    }


    future_ensemble_members <- unique(future_df$parameter)
    hist_ensemble_members <- unique(hist_df$parameter)


    # If there are a different number of ensemble members in the historical and future periods
    # this will resample the period with fewer ensemble members to match
    if (length(hist_ensemble_members) < length(future_ensemble_members)) {
      hist_ensemble_members <- sample(hist_ensemble_members, size = length(future_ensemble_members), replace = T)
    } else if (length(future_ensemble_members) < length(hist_ensemble_members)) {
      future_ensemble_members <- sample(future_ensemble_members, size = length(hist_ensemble_members), replace = T)
    }

    # Create an empty array to put the results in
    flow_file_names <- array(NA, dim = c(max(c(1, length(future_ensemble_members))),
                                         num_flows))

    for (j in 1:num_flows) {
      for (i in 1:length(future_ensemble_members)) {
        # generate the future period
        future_ens <- future_df |>
          dplyr::filter(flow_number == j,
                        parameter == future_ensemble_members[i],
                        datetime >= lubridate::as_date(forecast_start_datetime)) |>
          tidyr::pivot_wider(names_from = variable, values_from = prediction) |>
          dplyr::rename(time = datetime) |>
          dplyr::select(dplyr::all_of(variables)) |>
          dplyr::mutate_if(where(is.numeric), list(~round(., round_level)))

        # generate the historical period
        hist_ens <- hist_df |>
          dplyr::filter(flow_number == j,
                        parameter == hist_ensemble_members[i],
                        datetime >= start_datetime,
                        datetime < lubridate::as_date(forecast_start_datetime)) |>
          tidyr::pivot_wider(names_from = variable, values_from = prediction) |>
          dplyr::rename(time = datetime) |>
          dplyr::select(dplyr::all_of(variables)) |>
          dplyr::mutate_if(where(is.numeric), list(~round(., round_level)))

        # combine to single df
        flow <- dplyr::bind_rows(hist_ens,
                                 future_ens) |>
          arrange(time)

        if (use_ler_vars) {
          flow <- as.data.frame(flow)

          ler_vars_lookup <- c(Flow_metersCubedPerSecond = "FLOW",
                               Water_Temperature_celsius = "TEMP",
                               Salinity_practicalSalinityUnits = "SALT")

          flow[, 1] <- format(flow[, 1], format = "%Y-%m-%d %H:%M:%S")
          flow[, 1] <- lubridate::with_tz(flow[, 1]) + lubridate::hours(hour_step)
          flow[, 1] <- format(flow[, 1], format = "%Y-%m-%d %H:%M:%S")
          flow <- flow |>
            dplyr::select(any_of(c("time", "FLOW", "TEMP", "SALT"))) |>
            dplyr::rename(any_of(ler_vars_lookup))
        }        else {
          flow <- mutate(flow, time = lubridate::as_date(time))
        }

        flow_file_name <- file.path(out_dir, paste0(flow_type,
                                                    j, "_ens", i, ".csv"))
        flow_file_names[i, j] <- flow_file_name
        readr::write_csv(x = flow, file = flow_file_name,
                         quote = "none")
      }
    }
  } else if (!is.null(hist_df) & is.null(future_df)) { # do the same thing but when there is only historical data

    num_flows <- max(hist_df$flow_number)
    hist_ensemble_members <- unique(hist_df$parameter)


    flow_file_names <- array(NA, dim = c(max(c(1, length(hist_ensemble_members))),
                                         num_flows))


    for (j in 1:num_flows) {
      for (i in 1:length(hist_ensemble_members)) {
        hist_ens <- hist_df |>
          dplyr::filter(flow_number == j,
                        parameter == hist_ensemble_members[i],
                        datetime >= start_datetime,
                        datetime < lubridate::as_date(forecast_start_datetime)) |>
          tidyr::pivot_wider(names_from = variable, values_from = prediction) |>
          dplyr::rename(time = datetime) |>
          dplyr::select(dplyr::all_of(variables)) |>
          dplyr::mutate_if(where(is.numeric), list(~round(., round_level)))


        flow <- hist_ens |>
          arrange(time)

        if (use_ler_vars) {
          flow <- as.data.frame(flow)

          ler_vars_lookup <- c(Flow_metersCubedPerSecond = "FLOW",
                               Water_Temperature_celsius = "TEMP",
                               Salinity_practicalSalinityUnits = "SALT")

          flow[, 1] <- format(flow[, 1], format = "%Y-%m-%d %H:%M:%S")
          flow[, 1] <- lubridate::with_tz(flow[, 1]) + lubridate::hours(hour_step)
          flow[, 1] <- format(flow[, 1], format = "%Y-%m-%d %H:%M:%S")
          flow <- flow |>
            dplyr::select(any_of(c("time", "FLOW", "TEMP", "SALT"))) |>
            dplyr::rename(any_of(ler_vars_lookup))
        }        else {
          flow <- mutate(flow, time = lubridate::as_date(time))
        }

        flow_file_name <- file.path(out_dir, paste0(flow_type,
                                                    j, "_ens", i, ".csv"))
        flow_file_names[i, j] <- flow_file_name
        readr::write_csv(x = flow, file = flow_file_name,
                         quote = "none")
      }
    }
  } else if (is.null(hist_df) & !is.null(future_df)) { # do the same thing but when there is only future data

    num_flows <- max(future_df$flow_number)
    future_ensemble_members <- unique(future_df$parameter)

    flow_file_names <- array(NA, dim = c(max(c(1, length(future_ensemble_members))),
                                         num_flows))

    for (j in 1:num_flows) {
      for (i in 1:length(future_ensemble_members)) {
        future_ens <- future_df |>
          dplyr::filter(flow_number == j,
                        parameter == future_ensemble_members[i],
                        datetime >= lubridate::as_date(forecast_start_datetime)) |>
          tidyr::pivot_wider(names_from = variable, values_from = prediction) |>
          dplyr::rename(time = datetime) |>
          dplyr::select(dplyr::all_of(variables)) |>
          dplyr::mutate_if(where(is.numeric), list(~round(., round_level)))


        flow <- future_ens |>
          arrange(time)

        if (use_ler_vars) {
          flow <- as.data.frame(flow)

          ler_vars_lookup <- c(Flow_metersCubedPerSecond = "FLOW",
                               Water_Temperature_celsius = "TEMP",
                               Salinity_practicalSalinityUnits = "SALT")

          flow[, 1] <- format(flow[, 1], format = "%Y-%m-%d %H:%M:%S")
          flow[, 1] <- lubridate::with_tz(flow[, 1]) + lubridate::hours(hour_step)
          flow[, 1] <- format(flow[, 1], format = "%Y-%m-%d %H:%M:%S")
          flow <- flow |>
            dplyr::select(any_of(c("time", "FLOW", "TEMP", "SALT"))) |>
            dplyr::rename(any_of(ler_vars_lookup))
        }        else {
          flow <- mutate(flow, time = lubridate::as_date(time))
        }

        flow_file_name <- file.path(out_dir, paste0(flow_type,
                                                    j, "_ens", i, ".csv"))
        flow_file_names[i, j] <- flow_file_name
        readr::write_csv(x = flow, file = flow_file_name,
                         quote = "none")
      }
    }
  }

  if (!is.null(flow_historical_dir) | !is.null(flow_forecast_dir)) {
    return(flow_file_names)
  } else {
    return(NULL)
  }
}
