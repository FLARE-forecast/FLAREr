#' Score a forecast using score4cast package
#' @param targets_file observation file
#' @param forecast_file forecast file
#' @output_directory directory to save scored file
#' @return
#' @export
#'
#' @examples
generate_forecast_score <- function(targets_file,
                                    forecast_file,
                                    output_directory){

  target <- readr::read_csv(targets_file, show_col_types = FALSE) |>
    dplyr::mutate(site_id = paste0(site_id,"-",depth)) |>
    dplyr::rename(datetime = time)

  fn <- tools::file_path_sans_ext(tools::file_path_sans_ext(basename(forecast_file)))

  file_name <- file.path(output_directory,paste0(fn, ".parquet"))

  forecast <- forecast_file %>%
    read4cast::read_forecast() %>%
    select(-pub_time) %>%
    filter(variable_type == "state") %>%
    dplyr::mutate(filename = forecast_file,
                  site_id = paste0(site_id,"-",depth)) %>%
    score4cast::standardize_forecast() %>%
    score4cast::crps_logs_score(target) %>%
    mutate(horizon = datetime-lubridate::as_datetime(reference_datetime)) %>%
    mutate(horizon = as.numeric(lubridate::as.duration(horizon),
                                units = "seconds"),
           horizon = horizon / 86400) %>%
    arrow::write_parquet(file_name)

  invisible(file_name)
}
