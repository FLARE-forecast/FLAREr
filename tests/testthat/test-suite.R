# Met files ----
test_that("met files are generated", {

  dir <-  file.path(normalizePath(tempdir(),  winslash = "/"))
  lake_directory <- file.path(dir, "extdata")
  configure_run_file <- "configure_run.yml"
  config_set_name <- "default"
  file.copy(system.file("extdata", package = "FLAREr"), dir, recursive = TRUE)
  config <- FLAREr:::set_up_simulation(configure_run_file, lake_directory, config_set_name = config_set_name)
  config <- FLAREr:::get_restart_file(config, lake_directory)
  pars_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$par_config_file), col_types = readr::cols())
  obs_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$obs_config_file), col_types = readr::cols())
  states_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$states_config_file), col_types = readr::cols())

  met_start_datetime <- lubridate::as_datetime(config$run_config$start_datetime)
  met_forecast_start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime)

  met_out <- FLAREr:::create_met_files(config, lake_directory, met_forecast_start_datetime, met_start_datetime)


  met_file_names <- met_out$filenames
  testthat::expect_equal(file.exists(met_file_names), expected = rep(TRUE, 31))

  df <- readr::read_csv(met_file_names[1], show_col_types = FALSE)
  testthat::expect_s3_class(df, "data.frame")
})


test_that("open-meteo met files are generated", {

  skip_if_offline()

  dir <-  file.path(normalizePath(tempdir(),  winslash = "/"))
  install.packages("ropenmeteo", repos = "https://cloud.r-project.org")
  lake_directory <- file.path(dir, "extdata")
  configure_run_file <- "configure_run.yml"
  config_set_name <- "default"

  file.copy(system.file("extdata", package = "FLAREr"), dir, recursive = TRUE)

  config <- FLAREr::set_up_simulation(configure_run_file, lake_directory, config_set_name = config_set_name, clean_start = TRUE)
  pars_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$par_config_file), col_types = readr::cols())
  obs_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$obs_config_file), col_types = readr::cols())
  states_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$states_config_file), col_types = readr::cols())

  met_out <- FLAREr:::create_met_files_openmet(out_dir = config$file_path$execute_directory,
                                               start_datetime = lubridate::as_datetime(Sys.Date()) - lubridate::days(5),
                                               end_datetime = config$run_config$end_datetime,
                                               forecast_start_datetime = lubridate::as_datetime(Sys.Date()),
                                               forecast_horizon =  config$run_config$forecast_horizon,
                                               latitude = config$location$latitude,
                                               longitude = config$location$longitude,
                                               site_id = config$location$site_id,
                                               openmeteo_api = config$met$openmeteo_api,
                                               model = config$met$openmeteo_model,
                                               use_archive = config$met$use_openmeteo_archive,
                                               bucket = config$s3$drivers$bucket,
                                               endpoint = config$s3$drivers$endpoint)


  met_file_names <- met_out$filenames
  testthat::expect_equal(file.exists(met_file_names), expected = rep(TRUE, 31))

  df <- readr::read_csv(met_file_names[1], show_col_types = FALSE)
  testthat::expect_s3_class(df, "data.frame")
})


test_that("inflow files are generated", {

  dir <- file.path(normalizePath(tempdir(),  winslash = "/"))
  lake_directory <- file.path(dir, "extdata")
  configure_run_file <- "configure_run.yml"
  config_set_name <- "default"

  file.copy(system.file("extdata", package = "FLAREr"), dir, recursive = TRUE)
  config <- FLAREr:::set_up_simulation(configure_run_file, lake_directory, config_set_name = config_set_name)
  config <- FLAREr:::get_restart_file(config, lake_directory)
  pars_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$par_config_file), col_types = readr::cols())
  obs_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$obs_config_file), col_types = readr::cols())
  states_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$states_config_file), col_types = readr::cols())

  met_start_datetime <- lubridate::as_datetime(config$run_config$start_datetime)
  met_forecast_start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime)
  inflow_outflow_files <- FLAREr:::create_inflow_outflow_files(config, config_set_name, lake_directory)

  df <- readr::read_csv(inflow_outflow_files$inflow_file_names[,1], show_col_types = FALSE)
  testthat::expect_s3_class(df, "data.frame")

  df <- readr::read_csv(inflow_outflow_files$outflow_file_names[,1], show_col_types = FALSE)
  testthat::expect_s3_class(df, "data.frame")
})


test_that("observation matrix is created", {

  dir <-  file.path(normalizePath(tempdir(),  winslash = "/"))
  lake_directory <- file.path(dir, "extdata")
  configure_run_file <- "configure_run.yml"
  config_set_name <- "default"

  file.copy(system.file("extdata", package = "FLAREr"), dir, recursive = TRUE)
  config <- FLAREr:::set_up_simulation(configure_run_file, lake_directory, config_set_name = config_set_name)
  config <- FLAREr:::get_restart_file(config, lake_directory)
  pars_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$par_config_file), col_types = readr::cols())
  obs_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$obs_config_file), col_types = readr::cols())
  states_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$states_config_file), col_types = readr::cols())

  met_start_datetime <- lubridate::as_datetime(config$run_config$start_datetime)
  met_forecast_start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime)
  obs_insitu_file <- file.path(config$file_path$qaqc_data_directory, config$da_setup$obs_filename)

  obs <- FLAREr:::create_obs_matrix(cleaned_observations_file_long = obs_insitu_file,
                                    obs_config = obs_config,
                                    config)

  testthat::expect_setequal(dim(obs), c(1,21,11))

  testthat::expect_true(!is.na(obs[1,1,3]))
})


test_that("observation non-vertical list is created", {

  dir <-  file.path(normalizePath(tempdir(),  winslash = "/"))
  lake_directory <- file.path(dir, "extdata")
  configure_run_file <- "configure_run.yml"
  config_set_name <- "default"

  file.copy(system.file("extdata", package = "FLAREr"), dir, recursive = TRUE)
  config <- FLAREr:::set_up_simulation(configure_run_file, lake_directory, config_set_name = config_set_name)
  config <- FLAREr:::get_restart_file(config, lake_directory)
  pars_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$par_config_file), col_types = readr::cols())
  obs_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$obs_config_file), col_types = readr::cols())
  states_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$states_config_file), col_types = readr::cols())

  met_start_datetime <- lubridate::as_datetime(config$run_config$start_datetime)
  met_forecast_start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime)
  obs_insitu_file <- file.path(config$file_path$qaqc_data_directory, config$da_setup$obs_filename)

  obs_non_vertical <- FLAREr:::create_obs_non_vertical(cleaned_observations_file_long = file.path(config$file_path$qaqc_data_directory,paste0(config$location$site_id, "-targets-insitu.csv")),
                                                       obs_config,
                                                       start_datetime = config$run_config$start_datetime,
                                                       end_datetime = config$run_config$end_datetime,
                                                       forecast_start_datetime = config$run_config$forecast_start_datetime,
                                                       forecast_horizon =  config$run_config$forecast_horizon)

  testthat::expect_true(!is.null(obs_non_vertical$obs_secchi$obs))
  testthat::expect_true(is.null(obs_non_vertical$obs_depth))
})


test_that("state set up", {

  dir <-  file.path(normalizePath(tempdir(),  winslash = "/"))
  lake_directory <- file.path(dir, "extdata")
  configure_run_file <- "configure_run.yml"
  config_set_name <- "default"

  file.copy(system.file("extdata", package = "FLAREr"), dir, recursive = TRUE)
  config <- FLAREr:::set_up_simulation(configure_run_file, lake_directory, config_set_name = config_set_name)
  config <- FLAREr:::get_restart_file(config, lake_directory)
  pars_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$par_config_file), col_types = readr::cols())
  obs_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$obs_config_file), col_types = readr::cols())
  states_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$states_config_file), col_types = readr::cols())

  met_start_datetime <- lubridate::as_datetime(config$run_config$start_datetime)
  met_forecast_start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime)
  states_config <- FLAREr:::generate_states_to_obs_mapping(states_config, obs_config)

  testthat::expect_true(nrow(states_config) == 2)
  testthat::expect_true(states_config$states_to_obs_mapping_1[1] == 1)

  model_sd <- FLAREr:::initiate_model_error(config, states_config)

  testthat::expect_setequal(dim(model_sd), c(2,11))


})

test_that("initial conditions", {

  dir <-  file.path(normalizePath(tempdir(),  winslash = "/"))
  lake_directory <- file.path(dir, "extdata")
  configure_run_file <- "configure_run.yml"
  config_set_name <- "default"

  file.copy(system.file("extdata", package = "FLAREr"), dir, recursive = TRUE)
  config <- FLAREr:::set_up_simulation(configure_run_file, lake_directory, config_set_name = config_set_name)
  config <- FLAREr:::get_restart_file(config, lake_directory)
  pars_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$par_config_file), col_types = readr::cols())
  obs_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$obs_config_file), col_types = readr::cols())
  states_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$states_config_file), col_types = readr::cols())

  met_start_datetime <- lubridate::as_datetime(config$run_config$start_datetime)
  met_forecast_start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime)
  obs_insitu_file <- file.path(config$file_path$qaqc_data_directory, config$da_setup$obs_filename)

  obs <- FLAREr:::create_obs_matrix(cleaned_observations_file_long = obs_insitu_file,
                                    obs_config = obs_config,
                                    config)

  states_config <- FLAREr:::generate_states_to_obs_mapping(states_config, obs_config)

  obs_non_vertical <- FLAREr:::create_obs_non_vertical(cleaned_observations_file_long = file.path(config$file_path$qaqc_data_directory,paste0(config$location$site_id, "-targets-insitu.csv")),
                                                       obs_config,
                                                       start_datetime = config$run_config$start_datetime,
                                                       end_datetime = config$run_config$end_datetime,
                                                       forecast_start_datetime = config$run_config$forecast_start_datetime,
                                                       forecast_horizon =  config$run_config$forecast_horizon)

  model_sd <- FLAREr:::initiate_model_error(config, states_config)

  init <- FLAREr:::generate_initial_conditions(states_config,
                                               obs_config,
                                               pars_config,
                                               obs,
                                               config,
                                               obs_non_vertical)

  testthat::expect_true(length(init) == 3)
})


test_that("run_flare enkf and restart works", {

  skip_if_offline()
  skip_on_cran()

  remotes::install_github("rqthomas/GLM3r")
  Sys.setenv('GLM_PATH'='GLM3r')

  dir <-  file.path(normalizePath(tempdir(),  winslash = "/"))
  lake_directory <- file.path(dir, "extdata")
  configure_run_file <- "configure_run.yml"
  config_set_name <- "default"

  file.copy(system.file("extdata", package = "FLAREr"), dir, recursive = TRUE)
  config <- FLAREr:::set_up_simulation(configure_run_file, lake_directory, clean_start = TRUE, config_set_name = config_set_name)
  config <- FLAREr:::get_restart_file(config, lake_directory)
  pars_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$par_config_file), col_types = readr::cols())
  obs_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$obs_config_file), col_types = readr::cols())
  states_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$states_config_file), col_types = readr::cols())

  met_start_datetime <- lubridate::as_datetime(config$run_config$start_datetime)
  met_forecast_start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime)
  next_restart <- FLAREr::run_flare(lake_directory = lake_directory, configure_run_file = configure_run_file, config_set_name = config_set_name)

  testthat::expect_true(file.exists(file.path(lake_directory, "forecasts/parquet/site_id=fcre/model_id=test/reference_date=2022-10-02/part-0.parquet")))

  df <- arrow::open_dataset(file.path(lake_directory, "forecasts/parquet/site_id=fcre/model_id=test/reference_date=2022-10-02/part-0.parquet")) |>
    dplyr::collect()

  testthat::expect_true(min(lubridate::as_date(df$datetime)) == lubridate::as_date("2022-09-28"))

  testthat::expect_true(file.exists(file.path(config$file_path$restart_directory, "fcre-2022-10-02-test.nc")))

  FLAREr:::update_run_config(lake_directory,
                             configure_run_file,
                             restart_file = next_restart$restart_file,
                             start_datetime = "2022-09-29 00:00:00",
                             end_datetime = NA,
                             forecast_start_datetime = "2022-10-02 00:00:00",
                             forecast_horizon = 5,
                             sim_name = "test",
                             site_id = "fcre",
                             configure_flare = "configure_flare.yml",
                             configure_obs = NA,
                             use_s3 = FALSE,
                             bucket = NULL,
                             endpoint =NULL)

  new_restart <- FLAREr::run_flare(lake_directory = lake_directory, configure_run_file = configure_run_file, config_set_name = config_set_name)

  testthat::expect_true(!is.null(new_restart))

  df <- arrow::open_dataset(file.path(lake_directory, "forecasts/parquet/site_id=fcre/model_id=test/reference_date=2022-10-02/part-0.parquet")) |>
    dplyr::collect()

  testthat::expect_true(min(lubridate::as_date(df$datetime)) == lubridate::as_date("2022-09-29"))

})

test_that("run_flare aed works", {

  skip_if_offline()
  skip_on_cran()

  remotes::install_github("rqthomas/GLM3r")
  Sys.setenv('GLM_PATH'='GLM3r')

  dir <-  file.path(normalizePath(tempdir(),  winslash = "/"))
  lake_directory <- file.path(dir, "extdata")
  configure_run_file <- "configure_run.yml"
  config_set_name <- "aed"

  file.copy(system.file("extdata", package = "FLAREr"), dir, recursive = TRUE)
  config <- FLAREr:::set_up_simulation(configure_run_file, lake_directory, config_set_name = config_set_name)
  config <- FLAREr:::get_restart_file(config, lake_directory)
  pars_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$par_config_file), col_types = readr::cols())
  obs_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$obs_config_file), col_types = readr::cols())
  states_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$states_config_file), col_types = readr::cols())

  met_start_datetime <- lubridate::as_datetime(config$run_config$start_datetime)
  met_forecast_start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime)
  next_restart <- FLAREr::run_flare(lake_directory = lake_directory, configure_run_file = configure_run_file, config_set_name = config_set_name)

  testthat::expect_true(file.exists(file.path(lake_directory, "forecasts/parquet/site_id=fcre/model_id=test_aed/reference_date=2022-10-02/part-0.parquet")))

  testthat::expect_true(file.exists(file.path(lake_directory, "restart/fcre/test_aed/fcre-2022-10-02-test_aed.nc")))

  })

test_that("particle filter works", {

  skip_if_offline()
  skip_on_cran()

  remotes::install_github("rqthomas/GLM3r")
  Sys.setenv('GLM_PATH'='GLM3r')

  dir <-  file.path(normalizePath(tempdir(),  winslash = "/"))
  lake_directory <- file.path(dir, "extdata")
  configure_run_file <- "configure_run.yml"
  config_set_name <- "default_pf"

  file.copy(system.file("extdata", package = "FLAREr"), dir, recursive = TRUE)
  config <- FLAREr:::set_up_simulation(configure_run_file, lake_directory, config_set_name = config_set_name)
  config <- FLAREr:::get_restart_file(config, lake_directory)
  pars_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$par_config_file), col_types = readr::cols())
  obs_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$obs_config_file), col_types = readr::cols())
  states_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$states_config_file), col_types = readr::cols())

  met_start_datetime <- lubridate::as_datetime(config$run_config$start_datetime)
  met_forecast_start_datetime <- lubridate::as_datetime(config$run_config$forecast_start_datetime)
  next_restart <- FLAREr::run_flare(lake_directory = lake_directory, configure_run_file = configure_run_file, config_set_name = config_set_name)

  testthat::expect_true(file.exists(file.path(lake_directory, "forecasts/parquet/site_id=fcre/model_id=test_pf/reference_date=2022-10-02/part-0.parquet")))

  testthat::expect_true(file.exists(file.path(lake_directory, "restart/fcre/test_pf/fcre-2022-10-02-test_pf.nc")))

})

test_that("open meteo run works", {

  skip_if_offline()
  skip_on_cran()

  remotes::install_github("rqthomas/GLM3r")
  install.packages("ropenmeteo", repos = "https://cloud.r-project.org")
  Sys.setenv('GLM_PATH'='GLM3r')

  dir <-  file.path(normalizePath(tempdir(),  winslash = "/"))
  lake_directory <- file.path(dir, "extdata")
  configure_run_file <- "configure_run.yml"
  config_set_name <- "open_meteo"

  file.copy(system.file("extdata", package = "FLAREr"), dir, recursive = TRUE)

  run_config <- yaml::read_yaml(file.path(lake_directory, "configuration", config_set_name, configure_run_file))
  run_config$start_datetime <- lubridate::as_datetime(Sys.Date()) - lubridate::days(5)
  run_config$forecast_start_datetime <- lubridate::as_datetime(Sys.Date())
  yaml::write_yaml(run_config, file.path(lake_directory, "configuration", config_set_name, configure_run_file))


  config <- FLAREr:::set_up_simulation(configure_run_file, lake_directory, config_set_name = config_set_name, clean_start = TRUE)
  config <- FLAREr:::get_restart_file(config, lake_directory)
  pars_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$par_config_file), col_types = readr::cols())
  obs_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$obs_config_file), col_types = readr::cols())
  states_config <- readr::read_csv(file.path(config$file_path$configuration_directory, config$model_settings$states_config_file), col_types = readr::cols())

  next_restart <- FLAREr::run_flare(lake_directory = lake_directory, configure_run_file = configure_run_file, config_set_name = config_set_name)

  testthat::expect_true(file.exists(file.path(lake_directory, "forecasts/parquet/site_id=fcre/model_id=test_pf/reference_date=2022-10-02/part-0.parquet")))

  testthat::expect_true(file.exists(file.path(lake_directory, "restart/fcre/test_pf/fcre-2022-10-02-test_pf.nc")))

})











