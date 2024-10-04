#' Set and create directories in the configuration file
#'
#' @param configure_run_file name of run configuration file (do not include full path)
#' @param lake_directory full path to repository directory
#' @param clean_start logical: TRUE = reset run configuration with the file in the configuration directory within repository
#' @param config_set_name name of configuration set
#' @param sim_name name of simulation
#'
#' @returns list of configuration values (Invisibly); side effect of creating necessary subdirectories in `lake_directory`
#' @export
#'
#' @examples
#' dir <- normalizePath(tempdir(),  winslash = "/")
#' lake_directory <- file.path(dir, "extdata")
#' # Copy files to temporarly directory
#' dir.create(dir,showWarnings = FALSE)
#' file.copy(system.file("extdata", package = "FLAREr"),
#'           tempdir(),
#'           recursive = TRUE)
#'
#' set_up_simulation(configure_run_file = "configure_run.yml",
#'                   lake_directory = lake_directory,
#'                    clean_start = FALSE,
#'                    config_set_name = "default",
#'                    sim_name = NA)
#'
#'
#'
set_up_simulation <- function(configure_run_file = "configure_run.yml", lake_directory, clean_start = FALSE, config_set_name = "default", sim_name = NA){

  run_config <- yaml::read_yaml(file.path(lake_directory, "configuration", config_set_name,configure_run_file))
  config <- yaml::read_yaml(file.path(lake_directory,"configuration", config_set_name, run_config$configure_flare))
  config$run_config <- run_config
  config$file_path$qaqc_data_directory <- file.path(lake_directory, "targets", config$location$site_id)
  config$file_path$data_directory <- file.path(lake_directory, "data_raw")
  config$file_path$noaa_directory <- file.path(lake_directory, "drivers")
  config$file_path$configuration_directory <- file.path(lake_directory, "configuration",config_set_name)
  config$file_path$inflow_directory <- file.path(lake_directory, "drivers")
  config$file_path$plots_directory <- file.path(lake_directory, "plots", config$location$site_id)
  config$file_path$forecast_output_directory <- file.path(lake_directory, "forecasts")
  dir.create(config$file_path$qaqc_data_directory, recursive = TRUE, showWarnings = FALSE)
  dir.create(config$file_path$forecast_output_directory, recursive = TRUE, showWarnings = FALSE)
  dir.create(config$file_path$data_directory, recursive = TRUE, showWarnings = FALSE)
  dir.create(config$file_path$noaa_directory, recursive = TRUE, showWarnings = FALSE)
  dir.create(config$file_path$inflow_directory, recursive = TRUE, showWarnings = FALSE)
  dir.create(config$file_path$plots_directory, recursive = TRUE, showWarnings = FALSE)

  if(!is.null(config$output_settings$diagnostics_daily$depth)){
    config$output_settings$diagnostics_daily$depth <- as.numeric(config$output_settings$diagnostics_daily$depth)
  }

  run_config <- get_run_config(configure_run_file, lake_directory, config, clean_start, config_set_name = config_set_name, sim_name = sim_name)

  config$run_config <- run_config

  config$file_path$restart_directory <- file.path(lake_directory, "restart", config$location$site_id, config$run_config$sim_name)

  config$file_path$execute_directory <- file.path(lake_directory, "flare_tempdir", config$location$site_id, config$run_config$sim_name)
  dir.create(config$file_path$execute_directory, recursive = TRUE, showWarnings = FALSE)

  if(Sys.getenv(x = "AWS_ACCESS_KEY_ID") == "" & config$run_config$use_s3 == TRUE){
    warning(paste0(" Use s3 is set to TRUE in ",file.path(lake_directory,"configuration",config_set_name,configure_run_file),
                   "AWS_ACCESS_KEY_ID environment variable is not set.  s3 can still be used for downloading weather forecasts"))
  }

  if(Sys.getenv(x = "AWS_SECRET_ACCESS_KEY") == "" & config$run_config$use_s3 == TRUE){
    warning(paste0(" Use s3 is set to TRUE in ",file.path(lake_directory,"configuration",config_set_name,configure_run_file),
                   "AWS_SECRET_ACCESS_KEY environment variable is not set.  s3 can still be used for downloading"))
  }

  invisible(config)
}
