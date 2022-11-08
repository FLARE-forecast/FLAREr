#' @title Generating inflow and output files in the GLM format using arrow
#' @details Processes historical inflow data from inflow_obs and from files in the inflow_file_dir into the GLM format
#' @param inflow_forecast_dir string; full directory path that contains forecasted inflow and outflow files
#' @param inflow_obs string; full path to cleaned inflow observation in the specified format
#' @param working_directory string; full directory where FLARE executes
#' @param state_names vector; vector of state names that will be included in the inflow files
#' @return list with two vectors. One vector is the matrix of inflow_file_names and the other is the matrix of outflow_file_names
#' @export
#' @examples
##' \dontrun{
##' inflow_outflow_files <- create_glm_inflow_outflow_files(inflow_file_dir = inflow_forecast_path, inflow_obs = cleaned_inflow_file, working_directory = config$file_path$execute_directory, config, state_names = NULL)
##' }
create_inflow_outflow_files_arrow <- function(inflow_forecast_dir = NULL,
                                              inflow_obs,
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
                                              use_forecast = TRUE,
                                              use_ler_vars = FALSE){

  lake_name_code <- site_id

  if(!is.null(inflow_forecast_dir)){

    if(use_s3){
      if(is.null(bucket) | is.null(endpoint)){
        stop("scoring function needs bucket and endpoint if use_s3=TRUE")
      }
      vars <- FLAREr:::arrow_env_vars()
      inflow_s3 <- arrow::s3_bucket(bucket = file.path(bucket, inflow_forecast_dir),
                                    endpoint_override =  endpoint)
      FLAREr:::unset_arrow_vars(vars)
    }else{
      if(is.null(local_directory)){
        stop("scoring function needs local_directory if use_s3=FALSE")
      }
      inflow_s3 <- arrow::SubTreeFileSystem$create(local_directory)
    }
  }else{
    inflow_s3 <- NULL
  }

  start_datetime <- lubridate::as_datetime(start_datetime)
  if(is.na(forecast_start_datetime)){
    end_datetime <- lubridate::as_datetime(end_datetime)
    forecast_start_datetime <- end_datetime
  }else{
    forecast_start_datetime <- lubridate::as_datetime(forecast_start_datetime)
    end_datetime <- forecast_start_datetime + lubridate::days(forecast_horizon)
  }

  obs_inflow <- readr::read_csv(inflow_obs, show_col_types = FALSE)

  if(use_forecast){
    obs_inflow <- obs_inflow %>%
      dplyr::filter(datetime >= lubridate::as_date(start_datetime) & datetime <= lubridate::as_date(forecast_start_datetime))
  }else{
    obs_inflow <- obs_inflow %>%
      dplyr::filter(datetime >= lubridate::as_date(start_datetime) & datetime <= lubridate::as_date(end_datetime))
  }

  obs_outflow <- obs_inflow |> mutate(outflow_num = 1)

  if(!is.null(inflow_s3)){
    df <- arrow::open_dataset(inflow_s3) |>
      dplyr::collect()

    inflow_files <- df |>
      filter(flow_type == "inflow")

    outflow_files <- df |>
      filter(flow_type == "outflow")

    num_inflows <- max(inflow_files$flow_number)
    num_outflows <- max(outflow_files$flow_number)

    ensemble_members <- unique(inflow_files$parameter)

  }else{
    inflow_files <- NULL
    outflow_files <- NULL

    num_inflows <- 1
    num_outflows <- 1

    ensemble_members <- 1

  }





  inflow_file_names <- array(NA, dim = c(max(c(1, length(ensemble_members))), num_inflows))

  for(j in 1:num_inflows){

    if(length(inflow_files) == 0 | end_datetime == forecast_start_datetime){

      obs_inflow_tmp <- obs_inflow %>%
        tidyr::pivot_wider(names_from = variable, values_from = observation) |>
        dplyr::rename(time = datetime) |>
        dplyr::select(dplyr::all_of(variables))

      inflow_file_name <- file.path(out_dir, paste0("inflow",j,".csv"))

      readr::write_csv(x = obs_inflow_tmp,
                       file = inflow_file_name,
                       quote = "none")
      inflow_file_names[, j] <- inflow_file_name
    }else{

      for(i in 1:length(ensemble_members)){
        curr_ens <- inflow_files %>%
          dplyr::filter(flow_number == j,
                        parameter == ensemble_members[i]) %>%
          tidyr::pivot_wider(names_from = variable, values_from = prediction) |>
          dplyr::rename(time = datetime) |>
          dplyr::select(dplyr::all_of(variables)) %>%
          dplyr::mutate_at(dplyr::vars(dplyr::all_of(variables)), list(~round(., 4)))

        obs_inflow_tmp <- obs_inflow %>%
          dplyr::filter(datetime < lubridate::as_date(forecast_start_datetime)) %>%
          tidyr::pivot_wider(names_from = variable, values_from = observation) |>
          dplyr::rename(time = datetime) |>
          dplyr::select(dplyr::all_of(variables))


        inflow <- dplyr::bind_rows(obs_inflow_tmp, curr_ens)

        if(use_ler_vars){
          inflow <- as.data.frame(inflow)
          # inflow[, 1] <- as.POSIXct(inflow[, 1], tz = tz) + lubridate::hours(hour_step)
          inflow[, 1] <- format(inflow[, 1], format="%Y-%m-%d %H:%M:%S")
          inflow[, 1] <- lubridate::with_tz(inflow[, 1]) + lubridate::hours(hour_step)
          inflow[, 1] <- format(inflow[, 1], format="%Y-%m-%d %H:%M:%S")

          inflow <- inflow |>
            dplyr::select(c("time","FLOW", "TEMP", "SALT")) |>
            dplyr::rename(datetime = time,
                          Flow_metersCubedPerSecond = FLOW,
                          Water_Temperature_celsius = TEMP,
                          Salinity_practicalSalinityUnits = SALT)

        }

        inflow_file_name <- file.path(out_dir, paste0("inflow",j,"_ens",i,".csv"))

        inflow_file_names[i, j] <- inflow_file_name

        readr::write_csv(x = inflow,
                         file = inflow_file_name,
                         quote = "none")
      }
    }
  }

  outflow_file_names <- array(NA, dim = c(max(c(1, length(ensemble_members))),num_outflows))


  for(j in 1:num_outflows){

    if(length(outflow_files) == 0 | end_datetime == forecast_start_datetime){

      obs_outflow_tmp <- obs_outflow %>%
        tidyr::pivot_wider(names_from = variable, values_from = observation) |>
        dplyr::rename(time = datetime) |>
        dplyr::select(time, FLOW)

      if(use_ler_vars){
        obs_outflow_tmp <- as.data.frame(obs_outflow_tmp)
        obs_outflow_tmp[, 1] <- format(obs_outflow_tmp[, 1], format="%Y-%m-%d %H:%M:%S")
        obs_outflow_tmp <- obs_outflow_tmp |>
          dplyr::rename(datetime = time,
                        Flow_metersCubedPerSecond = FLOW)
      }

      outflow_file_name <- file.path(out_dir, paste0("outflow",j,".csv"))

      readr::write_csv(x = obs_outflow_tmp,
                       file = outflow_file_name,
                       quote = "none")
      outflow_file_names[, j] <- outflow_file_name
    }else{

      for(i in 1:length(ensemble_members)){

        d <- outflow_files %>%
          dplyr::filter(flow_number == j,
                        parameter == ensemble_members[i]) %>%
          tidyr::pivot_wider(names_from = variable, values_from = prediction) |>
          dplyr::rename(time = datetime) |>
          dplyr::select(time,FLOW)

        obs_outflow_tmp <- obs_outflow %>%
          dplyr::filter( datetime < lubridate::as_date(forecast_start_datetime)) %>%
          tidyr::pivot_wider(names_from = variable, values_from = observation) |>
          dplyr::rename(time = datetime) |>
          dplyr::select(time,FLOW)

        outflow <- dplyr::bind_rows(obs_outflow_tmp, d)

        if(use_ler_vars){
          outflow <- as.data.frame(outflow)
          outflow[, 1] <- format(outflow[, 1], format="%Y-%m-%d %H:%M:%S")
          outflow <- outflow |>
            dplyr::rename(datetime = time,
                          Flow_metersCubedPerSecond = FLOW)
        }

        outflow_file_name <- file.path(out_dir, paste0("outflow",j,"_ens",i,".csv"))

        outflow_file_names[i, j]  <- outflow_file_name

        if(use_forecast){
          readr::write_csv(x = outflow,
                           file = outflow_file_name,
                           quote = "none")
        }else{
          readr::write_csv(x = obs_outflow_tmp,
                           file = outflow_file_name,
                           quote = "none")
        }
      }
    }
  }

  return(list(inflow_file_names = as.character(gsub("\\\\", "/", inflow_file_names)),
              outflow_file_names = as.character(gsub("\\\\", "/", outflow_file_names))))
}
