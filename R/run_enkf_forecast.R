#' @title Run ensemble Kalman filter to assimilate observations and/or produce
#' forecasts
#'
#' @details Uses the ensemble Kalman filter to predict water quality for a lake
#' or reservior.  The function requires the initial conditions (`states_init`) for each
#' state and ensemble member using an array with the following dimension order:
#' states, depth, ensembles member.  If you are fitting parameters, it also requires
#' initial conditions for each parameter and ensemble member using an array (`par_init`) with the
#' following dimension order: parameters, ensemble member.  The arrays for states_init
#' and pars_init can be created using the `generate_initial_conditions()` function, if
#' starting from initial conditions in the  `states_config` data frame or from observations
#' in first time column of the `obs` array.  The arrays for `states_init` and `par_init`
#' can be created from the output from a previous run using the `generate_restart_initial_conditions()`
#' array.
#'
#' The required columns the `states_config` data frame with the following columns:
#' - `state_names`: the name in the GLM model for the state
#' - `initial_conditions`: the default initial condition for the state if an observation is lacking. Used in `generate_initial_conditions()`.  Note:
#' the `config` list should have a variables called `default_temp_init` and `default_temp_init_depths` that allow for depth variation in the initial
#' conditions for temperature.
#' - `model_sd`: the standard deviation of the model error for the state.  Matrix with dimensions rows = length(states_names), columns = length(config$modeled_depths)
#' - `initial_model_sd`: the standard deviation of the initial conditions for the state. Used in `generate_initial_conditions()`
#' - `states_to_obs1`: the name of the observation that matches the model state
#' - `states_to_obs_mapping_1`: the multipler that converts the state to the observation (1 will be the most common)
#' - `init_obs_name`: the name of the observations that is used to generate `states_init`.  Used in `generate_initial_conditions()`
#' - `init_obs_mapping`: the multipler that converts the state to the observation (1 will be the most common). Used in `generate_initial_conditions()`
#'
#' The required columns in the `pars_config` are:
#' - `par_names`:  the name of the parameter in the GLM nml file
#' - `par_names_save`: the name of the parameter that will  be written in the output file.  This is different that
#' `par_names` because a single parameter can have multiple zones.  Therefore, `par_names` will be the same but
#' `par_names_save` will be different so that you know which zone they are associated with
#' - `par_nml`: GLM namelist that the `par_name` can be found: `glm.nml`, `aed2.nml`,`aed2_phyto_pars.nml`,`aed2_zoop_pars.nml`
#' - `par_init`:  Initial value for the parameter
#' - `par_init_lowerbound`: Lower bound when initilizing with a uniform distribution
#' - `par_init_upperbound`: Upper bound when initilizing with a uniform distribution
#' - `par_lowerbound`: Lower bound that the parameter can physically have
#' - `par_upperbound`: Upper bound that the parameter can physically have
#' - `inflat_pars`: Variance inflation factor for parameters (values are >= 1)
#' - `par_units`: Units of parameter
#'
#'  The required columns in the `obs_config` are:
#'  - `states_names_obs`: the name of the model state that the obervation represents
#'  - `obs_units`: unit of the observations
#'  - `obs_sd`:  the standard deviation of the normal distribution describing the
#'  uncertainty in the observation.
#'  - `target_variable`: the name of the observation in the in long format observation
#'  file.  Used by `create_obs_matrix()`
#'  - `distance_threshold`: the distance in meters that an observation is associated
#'  with a particular depth
#'
#'
#'
#'  The required variables in the `config` list are:
#'  - `lake_name_code`:
#'  - `lake_name`:
#'  - `lake_latitude`:
#'  - `lake_longitude`:
#'  - `local_tzone`:
#'  - `metadata`:
#'     - `generate_eml`:
#'     - `abstract`:
#'     - `forecast_title`:
#'     - `intellectualRights`:
#'     - `model_description`:
#'          - `name`:
#'          - `type`:
#'          - `repository`:
#'     - `me`:
#'          - `individualName`:
#'               - `givenName`:
#'               - `surName`:
#'          - `electronicMailAddress`:
#'          - `id`:
#'    - `forecast_project_id`
#'  - `model_name`:
#'  - `base_GLM_nml`:
#'  - `base_AED_nml`:
#'  - `base_AED_phyto_pars_nml`:
#'  - `base_AED_zoop_pars_nml`:
#'  - `use_obs_constraint`:
#'  - `observation_uncertainty`:
#'  - `process_uncertainty`:
#'  - `weather_uncertainty`:
#'  - `initial_condition_uncertainty`:
#'  - `parameter_uncertainty`:
#'  - `met_downscale_uncertainty`:
#'  - `inflow_process_uncertainty`:
#'  - `modeled_depths:`
#'  - `ensemble_size`:
#'  - `localization_distance`:
#'  - `vert_decorr_length`:
#'  - `no_negative_states`:
#'  - `diagnostics_names`:
#'
#' The `management` list is used to define the Side Stream Saturation oxygen system.  It is
#' not required (defaults to `NULL`).  If included it requires:
#' - `management_input`: A two column data frame: FLOW, OXY_oxy
#' - `simulate_sss`: logical whether to simulate the sss
#' - `forecast_sss_on`: logical whether to have sss on in forecast
#' - `sss_depth`: Depth (m) of the SSS inflow and outflow
#' - `use_specified_sss`: logical whether to use the data from `management_input`
#' or from `forecast_sss_flow` and `forecast_sss_oxy` when forecasting with sss on
#' - `specified_sss_inflow_file`: file path of the inflow sss file is supplied rather than interally generated
#' - `specified_sss_outflow_file`: file path of the outflow sss file is supplied rather than interally generated
#' - `forecast_sss_flow`: m3/day of water entering via sss
#' - `forecast_sss_oxy`: oxygen concentration (mmmol/m3) entered in the sss
#'
#'
#' @param states_init array of the initial states.  Required dimensions are `[states, depths, ensemble]`
#' @param pars_init array of the initial states.  Required dimensions are `[pars, depths, ensemble]`.  (Default = NULL)
#' @param aux_states_init list of initial conditions for auxillary states.  These are states in the GLM that
#' are require for restarting the model but are not included in data assimilation.  These are states that are not associated
#' with a value in `model_sd`.
#' @param obs array of the observaitons. Required dimensions are `[nobs, time, depth]`
#' @param obs_sd
#' @param model_sd vector of standard deviations describing the model error for each state
#' @param working_directory directory model executes
#' @param met_file_names vector of meterology file names
#' @param inflow_file_names vector of inflow file names
#' @param outflow_file_names vector of outflow file names
#' @param start_datetime datetime of beginning of the simulation.  A datetime class ("YYYY-MM-DD HH:MM:SS"). Use the local time zone for the lake
#' @param end_datetime datetime of the end of the simulation. A datetime class ("YYYY-MM-DD HH:MM:SS"). Use the local time zone for the lake
#' @param forecast_start_datetime datetime when simulation is a forecast.  Must be equal to or earlier than `end_datetime`. If after `end_datetime`, it will
#' automatically be set to `end_datetime`.
#' @param config list of configurations
#' @param pars_config list of parameter configurations  (Default = NULL)
#' @param states_config list of state configurations
#' @param obs_config list of observation configurations
#' @param management list of management inputs and configuration  (Default = NULL)
#' @param da_method data assimilation method (enkf or pf; Default = enkf)
#' @return enkf_output a list is passed to `write_forecast_netcdf()` to write the
#' netcdf output and `create_flare_eml()` to generate the EML metadata
#' @export
#' @example
#'
#'
#'  start_datetime_local <- lubridate(as_datetime("2018-07-12 07:00:00, tz = "EST"))
#'  end_datetime_local <- lubridate(as_datetime("2018-07-15 07:00:00, tz = "EST"))
#'  forecast_start_datetime <- lubridate(as_datetime("2018-07-13 07:00:00, tz = "EST"))
#'
#'enkf_output <- flare::run_enkf_forecast(states_init = init$states,
#'               pars_init = init$pars,
#'               aux_states_init = aux_states_init,
#'               obs = obs,
#'               obs_sd = obs_config$obs_sd,
#'               model_sd = states_config$model_sd,
#'               working_directory = config$run_config$execute_location,
#'               met_file_names = met_file_names,
#'               inflow_file_names = inflow_file_names,
#'               outflow_file_names = outflow_file_names,
#'               start_datetime = start_datetime_local,
#'               end_datetime = end_datetime_local,
#'               forecast_start_datetime = forecast_start_datetime_local,
#'               config = config,
#'               pars_config = pars_config,
#'               states_config = states_config,
#'               obs_config = obs_config)
#'

run_enkf_forecast <- function(states_init,
                              pars_init = NULL,
                              aux_states_init,
                              obs,
                              obs_sd,
                              model_sd,
                              working_directory,
                              met_file_names,
                              inflow_file_names = NULL,
                              outflow_file_names = NULL,
                              start_datetime,
                              end_datetime,
                              forecast_start_datetime = NA,
                              config,
                              pars_config = NULL,
                              states_config,
                              obs_config,
                              management = NULL,
                              da_method = "enkf",
                              par_fit_method = "inflate"){

  message("run_enkf_forecast can been replaced by run_da_forecast. run_enkf_forecast is a wrapper for run_da_forecast and will be removed in future flare versions.  Please update scripts with run_da_forecast.")

  out <- flare::run_da_forecast(states_init,
                              pars_init = pars_init,
                              aux_states_init,
                              obs,
                              obs_sd,
                              model_sd,
                              working_directory,
                              met_file_names,
                              inflow_file_names = inflow_file_names,
                              outflow_file_names = outflow_file_names,
                              start_datetime,
                              end_datetime,
                              forecast_start_datetime = forecast_start_datetime,
                              config,
                              pars_config = pars_config,
                              states_config,
                              obs_config,
                              management = management,
                              da_method = da_method,
                              par_fit_method = par_fit_method)

  return(out)

}
