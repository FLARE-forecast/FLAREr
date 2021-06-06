# Met files ----
test_that("met files are generated", {

  template_folder <- system.file("example", package= "FLAREr")

  source(file.path(template_folder, "R/test_met_prep.R"))

  met_out <- FLAREr::generate_glm_met_files(obs_met_file = observed_met_file,
                                           out_dir = config$run_config$execute_directory,
                                           forecast_dir = config$file_path$noaa_directory,
                                           config)
  met_file_names <- met_out$filenames
  testthat::expect_equal(file.exists(met_file_names), expected = rep(TRUE, 21))
})


# Inflow Drivers (already done) ----
test_that("inflow & outflow files are generated", {

  template_folder <- system.file("example", package= "FLAREr")

  source(file.path(template_folder, "R/test_inflow_prep.R"))


  inflow_forecast_path <- config$file_path$inflow_directory

  #### NEED A TEST HERE TO CHECK THAT INFLOW FILES ARE GENERATED AND CORRECT
  inflow_outflow_files <- FLAREr::create_glm_inflow_outflow_files(inflow_file_dir = inflow_forecast_path,
                                                                 inflow_obs = cleaned_inflow_file,
                                                                 working_directory = config$run_config$execute_directory,
                                                                 config,
                                                                 state_names = NULL)

  inflow_file_names <- inflow_outflow_files$inflow_file_name
  outflow_file_names <- inflow_outflow_files$outflow_file_name

  testthat::expect_equal(file.exists(inflow_outflow_files[[1]]), expected = rep(TRUE, 21))
  testthat::expect_equal(file.exists(inflow_outflow_files[[2]]), expected = rep(TRUE, 21))
})



# Create observation matrix ----
test_that("observation matrix is generated and correct", {

  template_folder <- system.file("example", package = "FLAREr")
  temp_dir <- tempdir()
  # dir.create("example")
  file.copy(from = template_folder, to = temp_dir, recursive = TRUE)

  # test_directory <- "C:\\Users\\mooret\\Desktop\\FLARE\\flare-1\\inst\\data"
  test_directory <- file.path(temp_dir, "example")

  source(file.path(test_directory, "R/test_met_prep.R"))

  obs <- FLAREr::create_obs_matrix(cleaned_observations_file_long,
                                  obs_config,
                                  config)
  testthat::expect_true(is.array(obs))

  testthat::expect_true(any(!is.na(obs[1, , ])))

})


# State to obs mapping ----
test_that("generate states to obs mapping", {

  template_folder <- system.file("example", package= "FLAREr")
  temp_dir <- tempdir()
  # dir.create("example")
  file.copy(from = template_folder, to = temp_dir, recursive = TRUE)

  # test_directory <- "C:\\Users\\mooret\\Desktop\\FLARE\\flare-1\\inst\\data"
  test_directory <- file.path(temp_dir, "example")

  source(file.path(test_directory, "R/test_met_prep.R"))

  states_config <- FLAREr::generate_states_to_obs_mapping(states_config, obs_config)
  testthat::expect_true(is.data.frame(states_config))
})


# Initial model error ----
test_that("initial model error is generated", {

  template_folder <- system.file("example", package= "FLAREr")
  temp_dir <- tempdir()
  # dir.create("example")
  file.copy(from = template_folder, to = temp_dir, recursive = TRUE)

  # test_directory <- "C:\\Users\\mooret\\Desktop\\FLARE\\flare-1\\inst\\data"
  test_directory <- file.path(temp_dir, "example")

  source(file.path(test_directory, "R/test_met_prep.R"))

  config_file_directory <- file.path(config$file_path$configuration_directory, "flarer")

  model_sd <- FLAREr::initiate_model_error(config, states_config, config_file_directory)
  testthat::expect_true(is.array(model_sd))
  testthat::expect_true(any(!is.na(model_sd)))
})


# Set initial conditions ----
test_that("initial conditions are generated", {

  template_folder <- system.file("example", package= "FLAREr")
  temp_dir <- tempdir()
  # dir.create("example")
  file.copy(from = template_folder, to = temp_dir, recursive = TRUE)

  # test_directory <- "C:\\Users\\mooret\\Desktop\\FLARE\\flare-1\\inst\\data"
  test_directory <- file.path(temp_dir, "example")

  source(file.path(test_directory, "R/test_met_prep.R"))

  obs <- FLAREr::create_obs_matrix(cleaned_observations_file_long,
                                  obs_config,
                                  config)

  init <- FLAREr::generate_initial_conditions(states_config,
                                             obs_config,
                                             pars_config,
                                             obs,
                                             config)
  testthat::expect_true(is.list(init))
  chk <- lapply(init, is.array)
  testthat::expect_true(any(unlist(chk)))
})

# EnKF ----
test_that("EnKF can be run", {

  template_folder <- system.file("example", package = "FLAREr")
  temp_dir <- tempdir()
  # dir.create("example")
  file.copy(from = template_folder, to = temp_dir, recursive = TRUE)

  # test_directory <- "C:\\Users\\mooret\\Desktop\\FLARE\\flare-1\\inst\\data"
  test_directory <- file.path(temp_dir, "example")

  source(file.path(test_directory, "R/test_enkf_prep.R"))

  obs <- FLAREr::create_obs_matrix(cleaned_observations_file_long,
                                  obs_config,
                                  config)

  init <- FLAREr::generate_initial_conditions(states_config,
                                             obs_config,
                                             pars_config,
                                             obs,
                                             config)

  # states_init = init$states
  # pars_init = init$pars
  # aux_states_init = init$aux_states_init
  # obs = obs
  # obs_sd = obs_config$obs_sd
  # model_sd = model_sd
  # working_directory = config$file_path$execute_directory
  # met_file_names = (met_file_names)
  # inflow_file_names = (inflow_file_names)
  # outflow_file_names = (outflow_file_names)
  # config = config
  # pars_config = pars_config
  # states_config = states_config
  # obs_config = obs_config
  # management = NULL
  # da_method = "enkf"
  # par_fit_method = "inflate"

  #Run EnKF
  enkf_output <- FLAREr::run_da_forecast(states_init = init$states,
                                          pars_init = init$pars,
                                          aux_states_init = init$aux_states_init,
                                          obs = obs,
                                          obs_sd = obs_config$obs_sd,
                                          model_sd = model_sd,
                                          working_directory = config$file_path$execute_directory,
                                          met_file_names = met_file_names,
                                          inflow_file_names = inflow_file_names,
                                          outflow_file_names = outflow_file_names,
                                          config = config,
                                          pars_config = pars_config,
                                          states_config = states_config,
                                          obs_config = obs_config
  )

  #Load in pre-prepared output
  samp_enkf_output <- readRDS(file.path(test_directory, "benchmark_data/enkf_output.RDS"))

  testthat::expect_true(is.list(enkf_output))
  chk <- lapply(1:length(enkf_output), function(x) {
    class(enkf_output[[x]]) == class(samp_enkf_output[[x]])

  })
  testthat::expect_true(any(unlist(chk)))

  # Save forecast
  saved_file <- FLAREr::write_forecast_netcdf(enkf_output,
                                             forecast_location = config$run_config$forecast_output_directory)
  testthat::expect_true(file.exists(saved_file))

  #Create EML Metadata
  FLAREr::create_flare_metadata(file_name = saved_file,
                          enkf_output)
  file_chk <- list.files(config$run_config$forecast_output_directory, pattern = ".xml")
  testthat::expect_true(length(file_chk) > 0)

  FLAREr::plotting_general(file_name = saved_file,
                          qaqc_location = config$file_path$qaqc_data_directory)
  file_chk <- list.files(config$run_config$forecast_output_directory, pattern = ".pdf")
  testthat::expect_true(length(file_chk) > 0)


})

# Particle filter ----
test_that("particle filter can be run", {

  template_folder <- system.file("example", package = "FLAREr")
  temp_dir <- tempdir()
  # dir.create("example")
  file.copy(from = template_folder, to = temp_dir, recursive = TRUE)

  # test_directory <- "C:\\Users\\mooret\\Desktop\\FLARE\\flare-1\\inst\\data"
  test_directory <- file.path(temp_dir, "example")

  source(file.path(test_directory, "R/test_enkf_prep.R"))

  obs <- FLAREr::create_obs_matrix(cleaned_observations_file_long,
                                  obs_config,
                                  config)

  init <- FLAREr::generate_initial_conditions(states_config,
                                             obs_config,
                                             pars_config,
                                             obs,
                                             config)

  # states_init = init$states
  # pars_init = init$pars
  # aux_states_init = init$aux_states_init
  # obs = obs
  # obs_sd = obs_config$obs_sd
  # model_sd = model_sd
  # working_directory = config$run_config$execute_location
  # met_file_names = (met_file_names)
  # inflow_file_names = (inflow_file_names)
  # outflow_file_names = (outflow_file_names)
  # config = config
  # pars_config = pars_config
  # states_config = states_config
  # obs_config = obs_config
  # da_method = "pf"
  # par_fit_method = "perturb"

  #Run EnKF
  enkf_output <- FLAREr::run_da_forecast(states_init = init$states,
                                         pars_init = init$pars,
                                         aux_states_init = init$aux_states_init,
                                         obs = obs,
                                         obs_sd = obs_config$obs_sd,
                                         model_sd = model_sd,
                                         working_directory = config$file_path$execute_directory,
                                         met_file_names = met_file_names,
                                         inflow_file_names = inflow_file_names,
                                         outflow_file_names = outflow_file_names,
                                         config = config,
                                         pars_config = pars_config,
                                         states_config = states_config,
                                         obs_config = obs_config,
                                         da_method = "pf",
                                         par_fit_method = "perturb"
  )

  #Load in pre-prepared output
  samp_enkf_output <- readRDS(file.path(test_directory, "benchmark_data/enkf_output.RDS"))

  testthat::expect_true(is.list(enkf_output))
  chk <- lapply(1:length(enkf_output), function(x) {
    class(enkf_output[[x]]) == class(samp_enkf_output[[x]])

  })
  testthat::expect_true(any(unlist(chk)))

  # Save forecast
  saved_file <- FLAREr::write_forecast_netcdf(enkf_output,
                                             forecast_location = config$run_config$forecast_output_directory)
  testthat::expect_true(file.exists(saved_file))

  #Create EML Metadata
  FLAREr::create_flare_metadata(file_name = saved_file,
                          enkf_output)
  file_chk <- list.files(config$run_config$forecast_output_directory, pattern = ".xml")
  testthat::expect_true(length(file_chk) > 0)

  FLAREr::plotting_general(file_name = saved_file,
                          qaqc_location = config$file_path$qaqc_data_directory)
  file_chk <- list.files(config$run_config$forecast_output_directory, pattern = ".pdf")
  testthat::expect_true(length(file_chk) > 0)

})

# EnKF no inflows/outflows ----
test_that("EnKF can be run with NO inflows/outflows", {

  template_folder <- system.file("example", package = "FLAREr")
  temp_dir <- tempdir()
  # dir.create("example")
  file.copy(from = template_folder, to = temp_dir, recursive = TRUE)

  # test_directory <- "C:\\Users\\mooret\\Desktop\\FLARE\\flare-1\\inst\\data"
  test_directory <- file.path(temp_dir, "example")

  source(file.path(test_directory, "R/test_enkf_prep.R"))

  obs <- FLAREr::create_obs_matrix(cleaned_observations_file_long,
                                   obs_config,
                                   config)

  init <- FLAREr::generate_initial_conditions(states_config,
                                              obs_config,
                                              pars_config,
                                              obs,
                                              config)

  #Run EnKF
  enkf_output <- FLAREr::run_da_forecast(states_init = init$states,
                                         pars_init = init$pars,
                                         aux_states_init = init$aux_states_init,
                                         obs = obs,
                                         obs_sd = obs_config$obs_sd,
                                         model_sd = model_sd,
                                         working_directory = config$file_path$execute_directory,
                                         met_file_names = met_file_names,
                                         inflow_file_names = NULL,
                                         outflow_file_names = NULL,
                                         config = config,
                                         pars_config = pars_config,
                                         states_config = states_config,
                                         obs_config = obs_config
  )

  #Load in pre-prepared output
  samp_enkf_output <- readRDS(file.path(test_directory, "benchmark_data/enkf_output.RDS"))

  testthat::expect_true(is.list(enkf_output))
  chk <- lapply(1:length(enkf_output), function(x) {
    class(enkf_output[[x]]) == class(samp_enkf_output[[x]])

  })
  testthat::expect_true(any(unlist(chk)))

  # Save forecast
  saved_file <- FLAREr::write_forecast_netcdf(enkf_output,
                                              forecast_location = config$run_config$forecast_output_directory)
  testthat::expect_true(file.exists(saved_file))

  #Create EML Metadata
  FLAREr::create_flare_metadata(file_name = saved_file,
                                enkf_output)
  file_chk <- list.files(config$run_config$forecast_output_directory, pattern = ".xml")
  testthat::expect_true(length(file_chk) > 0)

  FLAREr::plotting_general(file_name = saved_file,
                           qaqc_location = config$file_path$qaqc_data_directory)
  file_chk <- list.files(config$run_config$forecast_output_directory, pattern = ".pdf")
  testthat::expect_true(length(file_chk) > 0)

})

# end
