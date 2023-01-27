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
    dplyr::mutate(site_id = paste0(site_id,"-",depth))
  
  if("time" %in% colnames(target)){
    target <- target |>
       dplyr::rename(datetime = time)
    }

  fn <- tools::file_path_sans_ext(tools::file_path_sans_ext(basename(forecast_file)))

  file_name <- file.path(output_directory,paste0(fn, ".parquet"))
  
  forecast <- forecast_file %>%
    read4cast::read_forecast() %>%
    dplyr::select(-pub_time) %>%
    dplyr::filter(variable_type %in% variable_types) %>%
    dplyr::mutate(site_id = paste0(site_id,"-",depth)) %>%
    score4cast::standardize_forecast(reference_datetime_format = "%Y-%m-%d %H:%M:%S") %>%
    dplyr::mutate(family = as.character(family)) |>
    score4cast::crps_logs_score(target) %>%
    dplyr::mutate(horizon = datetime-lubridate::as_datetime(reference_datetime)) %>%
    dplyr::mutate(horizon = as.numeric(lubridate::as.duration(horizon),
                                units = "seconds"),
           horizon = horizon / 86400) %>%
    dplyr::mutate(depth = as.numeric(str_split_fixed(site_id, "-", 2)[,2]),
           site_id = str_split_fixed(site_id, "-", 2)[,1]) %>%
    arrow::write_parquet(file_name)

  invisible(file_name)
}
