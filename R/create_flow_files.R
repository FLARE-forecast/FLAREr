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
#' @param out_dir_fn Optional function of the ensemble positional index (integer,
#'   1-based) returning the directory for that member's flow file. When non-NULL
#'   each file is written to \code{out_dir_fn(i)}; when NULL files are written
#'   to \code{out_dir}. Default \code{NULL}.
#' @return matrix of flow_file_names
#' @export
#'

# Check that all requested variables exist in a wide-format slice; stop with
# a clear message listing what is missing and what is available in the data.
.check_flow_vars <- function(wide_df, variables, context = "flow") {
  missing <- setdiff(variables, names(wide_df))
  if (length(missing) > 0) {
    stop(
      "Variable(s) [", paste(missing, collapse = ", "), "] requested for the ",
      context, " inflow/outflow file are not present in the driver parquet.\n",
      "Available columns: ", paste(sort(names(wide_df)), collapse = ", "), "\n",
      "Check that your inflow driver parquet contains all required variables.\n",
      "If these are AED state variables (e.g. OXY_oxy), they must either be ",
      "included in the inflow parquet or removed from the variables list in ",
      "inflow_boundary_config.csv.",
      call. = FALSE
    )
  }
}

# Prepare one ensemble member's historical period data slice
prep_hist_slice <- function(df, flow_num, member, start_dt,
                            forecast_start_dt, variables, round_level) {
  wide <- df |>
    dplyr::filter(
      flow_number == flow_num,
      parameter == member,
      datetime >= start_dt,
      datetime < lubridate::as_date(forecast_start_dt)
    ) |>
    tidyr::pivot_wider(names_from = variable, values_from = prediction) |>
    dplyr::rename(time = datetime)
  .check_flow_vars(wide, variables, "historical")
  wide |>
    dplyr::select(dplyr::all_of(variables)) |>
    dplyr::mutate_if(where(is.numeric), list(~round(., round_level)))
}

# Prepare one ensemble member's future period data slice
prep_future_slice <- function(df, flow_num, member,
                              forecast_start_dt, variables, round_level) {
  wide <- df |>
    dplyr::filter(
      flow_number == flow_num,
      parameter == member,
      datetime >= lubridate::as_date(forecast_start_dt)
    ) |>
    tidyr::pivot_wider(names_from = variable, values_from = prediction) |>
    dplyr::rename(time = datetime)
  .check_flow_vars(wide, variables, "forecast")
  wide |>
    dplyr::select(dplyr::all_of(variables)) |>
    dplyr::mutate_if(where(is.numeric), list(~round(., round_level)))
}

# Apply LER variable renaming (or plain date conversion), then write the CSV
write_flow_csv <- function(flow, use_ler_vars, hour_step, flow_type,
                           flow_num, ens_index, out_dir, out_dir_fn = NULL) {
  ler_vars_lookup <- c(
    Flow_metersCubedPerSecond       = "FLOW",
    Water_Temperature_celsius       = "TEMP",
    Salinity_practicalSalinityUnits = "SALT"
  )

  if (use_ler_vars) {
    flow <- as.data.frame(flow)
    flow[, 1] <- format(flow[, 1], format = "%Y-%m-%d %H:%M:%S")
    flow[, 1] <- lubridate::with_tz(flow[, 1]) + lubridate::hours(hour_step)
    flow[, 1] <- format(flow[, 1], format = "%Y-%m-%d %H:%M:%S")
    flow <- flow |>
      dplyr::select(dplyr::any_of(c("time", "FLOW", "TEMP", "SALT"))) |>
      dplyr::rename(dplyr::any_of(ler_vars_lookup))
  } else {
    flow <- dplyr::mutate(flow, time = lubridate::as_date(time))
  }

  ens_out_dir <- if (!is.null(out_dir_fn)) out_dir_fn(ens_index) else out_dir
  dir.create(ens_out_dir, recursive = TRUE, showWarnings = FALSE)
  flow_file_name <- file.path(
    ens_out_dir, paste0(flow_type, flow_num, "_ens", ens_index, ".csv")
  )
  readr::write_csv(x = flow, file = flow_file_name, quote = "none")
  flow_file_name
}

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
                              use_ler_vars = FALSE,
                              config = config,
                              out_dir_fn = NULL) {

  server_name <- if (flow_type == "inflow") {
    "inflow_drivers"
  } else if (flow_type == "outflow") {
    "outflow_drivers"
  } else {
    stop("Invalid flow_type. Please use 'inflow' or 'outflow'.")
  }

  round_level <- 10

  # set locations of flow drivers (s3 or local)
  if (!is.null(flow_forecast_dir) && !is.null(flow_historical_dir)) {
    if (use_s3) {
      if (is.null(bucket) || is.null(endpoint)) {
        stop("needs bucket and endpoint if use_s3=TRUE")
      }
      vars <- arrow_env_vars()
      prefix <- file.path(stringr::str_split_fixed(bucket, "/", n = 2)[2], flow_forecast_dir)
      future_s3 <- flare_arrow_s3_bucket(server_name = server_name, faasr_prefix = prefix, mode_override = "s3", config = config)
      prefix <- file.path(stringr::str_split_fixed(bucket, "/", n = 2)[2], flow_historical_dir)
      hist_s3 <- flare_arrow_s3_bucket(server_name = server_name, faasr_prefix = prefix, mode_override = "s3", config = config)
      unset_arrow_vars(vars)
    } else {
      if (is.null(local_directory)) {
        stop("needs local_directory if use_s3=FALSE")
      }
      future_s3 <- arrow::SubTreeFileSystem$create(file.path(local_directory, flow_forecast_dir))
      hist_s3   <- arrow::SubTreeFileSystem$create(file.path(local_directory, flow_historical_dir))
    }
  } else if (is.null(flow_forecast_dir) && !is.null(flow_historical_dir)) {
    if (use_s3) {
      if (is.null(bucket) || is.null(endpoint)) {
        stop("needs bucket and endpoint if use_s3=TRUE")
      }
      vars <- arrow_env_vars()
      future_s3 <- NULL
      prefix <- file.path(stringr::str_split_fixed(bucket, "/", n = 2)[2], flow_historical_dir)
      hist_s3 <- flare_arrow_s3_bucket(server_name = server_name, faasr_prefix = prefix, mode_override = "s3", config = config)
      unset_arrow_vars(vars)
    } else {
      if (is.null(local_directory)) {
        stop("needs local_directory if use_s3=FALSE")
      }
      future_s3 <- NULL
      hist_s3   <- arrow::SubTreeFileSystem$create(file.path(local_directory, flow_historical_dir))
    }
  } else if (!is.null(flow_forecast_dir) && is.null(flow_historical_dir)) {
    if (use_s3) {
      if (is.null(bucket) || is.null(endpoint)) {
        stop("needs bucket and endpoint if use_s3=TRUE")
      }
      vars <- arrow_env_vars()
      hist_s3 <- NULL
      prefix <- file.path(stringr::str_split_fixed(bucket, "/", n = 2)[2], flow_forecast_dir)
      future_s3 <- flare_arrow_s3_bucket(server_name = server_name, faasr_prefix = prefix, mode_override = "s3", config = config)
      unset_arrow_vars(vars)
    } else {
      if (is.null(local_directory)) {
        stop("needs local_directory if use_s3=FALSE")
      }
      hist_s3   <- NULL
      future_s3 <- arrow::SubTreeFileSystem$create(file.path(local_directory, flow_forecast_dir))
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
  } else {
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
    future_df <- NULL
  }

  if (!is.null(hist_s3)) {
    hist_df <- dplyr::collect(arrow::open_dataset(hist_s3)) |>
      dplyr::filter(datetime < forecast_start_datetime,
                    datetime >= start_datetime) |>
      dplyr::distinct()

    if (!("parameter" %in% colnames(hist_df))) {
      hist_df <- hist_df |> dplyr::mutate(parameter = 1)
    }
    if ("observation" %in% colnames(hist_df)) {
      hist_df <- hist_df |> dplyr::rename(prediction = observation)
    }
  } else {
    hist_df <- NULL
  }

  if (!is.null(future_df) && !is.null(hist_df)) {
    if (!setequal(unique(future_df$flow_number), unique(hist_df$flow_number))) {
      print(tail(future_df))
      print(tail(hist_df))
      stop("need the same number of flows in historical and future periods")
    }
    num_flows <- max(future_df$flow_number)

    future_members <- unique(future_df$parameter)
    hist_members   <- unique(hist_df$parameter)

    # Resample whichever period has fewer ensemble members to match
    if (length(hist_members) < length(future_members)) {
      hist_members <- sample(hist_members, size = length(future_members), replace = TRUE)
    } else if (length(future_members) < length(hist_members)) {
      future_members <- sample(future_members, size = length(hist_members), replace = TRUE)
    }

    flow_file_names <- array(NA, dim = c(max(c(1, length(future_members))), num_flows))

    for (j in 1:num_flows) {
      for (i in seq_along(future_members)) {
        hist_ens   <- prep_hist_slice(hist_df, j, hist_members[i], start_datetime,
                                     forecast_start_datetime, variables, round_level)
        future_ens <- prep_future_slice(future_df, j, future_members[i],
                                       forecast_start_datetime, variables, round_level)
        flow <- dplyr::bind_rows(hist_ens, future_ens) |> dplyr::arrange(time)
        flow_file_names[i, j] <- write_flow_csv(flow, use_ler_vars, hour_step,
                                                flow_type, j, i, out_dir, out_dir_fn)
      }
    }

  } else if (!is.null(hist_df) && is.null(future_df)) {
    num_flows    <- max(hist_df$flow_number)
    hist_members <- unique(hist_df$parameter)
    flow_file_names <- array(NA, dim = c(max(c(1, length(hist_members))), num_flows))

    for (j in 1:num_flows) {
      for (i in seq_along(hist_members)) {
        flow <- prep_hist_slice(hist_df, j, hist_members[i], start_datetime,
                                forecast_start_datetime, variables, round_level) |>
          dplyr::arrange(time)
        flow_file_names[i, j] <- write_flow_csv(flow, use_ler_vars, hour_step,
                                                flow_type, j, i, out_dir, out_dir_fn)
      }
    }

  } else if (is.null(hist_df) && !is.null(future_df)) {
    num_flows      <- max(future_df$flow_number)
    future_members <- unique(future_df$parameter)
    flow_file_names <- array(NA, dim = c(max(c(1, length(future_members))), num_flows))

    for (j in 1:num_flows) {
      for (i in seq_along(future_members)) {
        flow <- prep_future_slice(future_df, j, future_members[i],
                                  forecast_start_datetime, variables, round_level) |>
          dplyr::arrange(time)
        flow_file_names[i, j] <- write_flow_csv(flow, use_ler_vars, hour_step,
                                                flow_type, j, i, out_dir, out_dir_fn)
      }
    }
  }

  if (!is.null(flow_historical_dir) || !is.null(flow_forecast_dir)) {
    flow_file_names
  } else {
    NULL
  }
}
