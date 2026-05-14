##' @title Generate parquet output file
##' @details Function generates a parquet file from the object that is returned by run_da_forecast()
##' @param da_forecast_output list; object that is returned by run_da_forecast()
##' @param use_s3 Boolen; use s3 storage for saving scores
##' @param bucket S3 bucket
##' @param endpoint S3 endpoint
##' @param local_directory local directory of scores if not using s3
##' @return None
##' @importFrom lubridate with_tz
##' @author Quinn Thomas
##' @keywords internal
##'

write_forecast <- function(da_forecast_output,
                                 use_s3 = FALSE,
                                 bucket = NULL,
                                 endpoint = NULL,
                                 local_directory = NULL,config = NULL){

  if(!is.null(config) && !is.null(config$faasr)) {

    faasr_config <- config$faasr
  }

  if(use_s3){
    if(is.null(bucket) | is.null(endpoint)){
      stop("scoring function needs bucket and endpoint if use_s3=TRUE")
    }

    vars <- arrow_env_vars()
    server_name <-  "forecasts_parquet"
    prefix <- glue::glue(stringr::str_split_fixed(bucket, "/", n = 2)[2])

    output_directory <- FaaSr::faasr_arrow_s3_bucket(server_name = server_name,faasr_prefix = prefix,faasr_config=faasr_config)
    #output_directory <- arrow::s3_bucket(bucket = bucket,
                                         #endpoint_override =  endpoint)
    on.exit(unset_arrow_vars(vars))
  }else{
    if(is.null(local_directory)){
      stop("scoring function needs local_directory if use_s3=FALSE")
    }
    output_directory <- arrow::SubTreeFileSystem$create(local_directory)
  }

  x <- da_forecast_output$states_depth
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
  diagnostics_daily <- da_forecast_output$diagnostics_daily
  log_particle_weights <- da_forecast_output$log_particle_weights

  forecast_flag[which(is.na(forecast_flag))] <- 0

  if(!("multi_depth" %in% names(obs_config))){
    obs_config <- obs_config |> dplyr::mutate(multi_depth = 1)
  }

  obs_config <- obs_config |>
    dplyr::filter(multi_depth == 1)

  pieces <- list()

  # --- states: x[time, states, depths, ens] ---
  n_time   <- dim(x)[1]
  n_states <- dim(x)[2]
  n_depths <- dim(x)[3]
  n_ens    <- dim(x)[4]

  x_mat <- matrix(x, nrow = n_time * n_states * n_depths, ncol = n_ens)
  idx   <- expand.grid(time = seq_len(n_time), states = seq_len(n_states), depths = seq_len(n_depths))

  pieces$states <- tibble::tibble(
    predicted     = c(t(x_mat)),
    time          = rep(full_time[idx$time],                              each = n_ens),
    depth         = rep(config$model_settings$modeled_depths[idx$depths], each = n_ens),
    variable      = rep(states_config$state_names[idx$states],            each = n_ens),
    forecast      = rep(forecast_flag[idx$time],                          each = n_ens),
    ensemble      = rep(seq_len(n_ens), nrow(idx)),
    variable_type = "state",
    log_weight    = c(t(log_particle_weights[idx$time, ]))
  )

  tmp_index <- 0
  for(s in 1:length(obs_config$state_names_obs)){
    if(!(obs_config$state_names_obs[s] %in% states_config$state_names) &
       obs_config$multi_depth[s] == 1){
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

      # --- derived obs: temp_var[time, depths, ens] ---
      n_time_tv   <- dim(temp_var)[1]
      n_depths_tv <- dim(temp_var)[2]
      n_ens_tv    <- dim(temp_var)[3]

      tv_mat <- matrix(temp_var, nrow = n_time_tv * n_depths_tv, ncol = n_ens_tv)
      idx_tv <- expand.grid(time = seq_len(n_time_tv), depth = seq_len(n_depths_tv))

      pieces[[paste0("derived_obs_", tmp_index)]] <- tibble::tibble(
        predicted     = c(t(tv_mat)),
        time          = rep(full_time[idx_tv$time],                               each = n_ens_tv),
        depth         = rep(config$model_settings$modeled_depths[idx_tv$depth],   each = n_ens_tv),
        variable      = obs_config$target_variable[s],
        forecast      = rep(forecast_flag[idx_tv$time],                           each = n_ens_tv),
        ensemble      = rep(seq_len(n_ens_tv), nrow(idx_tv)),
        variable_type = "state",
        log_weight    = c(t(log_particle_weights[idx_tv$time, ]))
      )
    }
  }


  if(length(config$output_settings$diagnostics_names) > 0){

    # --- diagnostics: diagnostics[diag, time, depth, ens] ---
    n_diag_d  <- dim(diagnostics)[1]
    n_time_d  <- dim(diagnostics)[2]
    n_depth_d <- dim(diagnostics)[3]
    n_ens_d   <- dim(diagnostics)[4]

    d_mat <- matrix(diagnostics, nrow = n_diag_d * n_time_d * n_depth_d, ncol = n_ens_d)
    idx_d <- expand.grid(diag = seq_len(n_diag_d), time = seq_len(n_time_d), depth = seq_len(n_depth_d))

    pieces$diagnostics <- tibble::tibble(
      predicted     = c(t(d_mat)),
      time          = rep(full_time[idx_d$time],                                 each = n_ens_d),
      depth         = rep(config$model_settings$modeled_depths[idx_d$depth],     each = n_ens_d),
      variable      = rep(config$output_settings$diagnostics_names[idx_d$diag],  each = n_ens_d),
      forecast      = rep(forecast_flag[idx_d$time],                              each = n_ens_d),
      ensemble      = rep(seq_len(n_ens_d), nrow(idx_d)),
      variable_type = "diagnostic",
      log_weight    = c(t(log_particle_weights[idx_d$time, ]))
    )
  }



  if(length(config$output_settings$diagnostics_daily$names) > 0){

    # --- diagnostics_daily: diagnostics_daily[diag, time, ens] ---
    n_diag_dd <- dim(diagnostics_daily)[1]
    n_time_dd <- dim(diagnostics_daily)[2]
    n_ens_dd  <- dim(diagnostics_daily)[3]

    dd_mat <- matrix(diagnostics_daily, nrow = n_diag_dd * n_time_dd, ncol = n_ens_dd)
    idx_dd <- expand.grid(diag = seq_len(n_diag_dd), time = seq_len(n_time_dd))

    pieces$diagnostics_daily <- tibble::tibble(
      predicted     = c(t(dd_mat)),
      time          = rep(full_time[idx_dd$time],                                             each = n_ens_dd),
      depth         = NA,
      variable      = rep(config$output_settings$diagnostics_daily$save_names[idx_dd$diag],  each = n_ens_dd),
      forecast      = rep(forecast_flag[idx_dd$time],                                         each = n_ens_dd),
      ensemble      = rep(seq_len(n_ens_dd), nrow(idx_dd)),
      variable_type = "diagnostic",
      log_weight    = c(t(log_particle_weights[idx_dd$time, ]))
    )
  }

  if(!is.null(pars)){

    # --- parameters: pars[time, par, ens] ---
    n_time_p <- dim(pars)[1]
    n_par    <- dim(pars)[2]
    n_ens_p  <- dim(pars)[3]

    p_mat <- matrix(pars, nrow = n_time_p * n_par, ncol = n_ens_p)
    idx_p <- expand.grid(time = seq_len(n_time_p), par = seq_len(n_par))

    pieces$pars <- tibble::tibble(
      predicted     = c(t(p_mat)),
      time          = rep(full_time[idx_p$time],                    each = n_ens_p),
      depth         = NA,
      variable      = rep(pars_config$par_names_save[idx_p$par],    each = n_ens_p),
      forecast      = rep(forecast_flag[idx_p$time],                 each = n_ens_p),
      ensemble      = rep(seq_len(n_ens_p), nrow(idx_p)),
      variable_type = "parameter",
      log_weight    = c(t(log_particle_weights[idx_p$time, ]))
    )
  }

  if(!is.null(da_forecast_output$restart_list)){
    lake_depth <- da_forecast_output$restart_list$lake_depth
  }

  # --- lake depth: lake_depth[time, ens] ---
  n_time_ld <- dim(lake_depth)[1]
  n_ens_ld  <- dim(lake_depth)[2]

  pieces$lake_depth <- tibble::tibble(
    predicted     = c(t(lake_depth)),
    time          = rep(full_time[seq_len(n_time_ld)],      each = n_ens_ld),
    variable      = "depth",
    depth         = NA,
    forecast      = rep(forecast_flag[seq_len(n_time_ld)],  each = n_ens_ld),
    ensemble      = rep(seq_len(n_ens_ld), n_time_ld),
    variable_type = "state",
    log_weight    = c(t(log_particle_weights[seq_len(n_time_ld), ]))
  )

  if(length(config$output_settings$diagnostics_names) > 0){

    # --- secchi: derived from diagnostics[1, time, depth_idx, ens] ---
    depth_idx_secchi <- which.min(abs(config$model_settings$modeled_depths - 1.0))
    n_time_s <- dim(diagnostics)[2]
    n_ens_s  <- dim(diagnostics)[4]
    secchi_mat <- 1.7 / diagnostics[1, , depth_idx_secchi, ]  # [time, ens]

    pieces$secchi <- tibble::tibble(
      predicted     = c(t(secchi_mat)),
      time          = rep(full_time[seq_len(n_time_s)],      each = n_ens_s),
      variable      = "secchi",
      depth         = NA,
      forecast      = rep(forecast_flag[seq_len(n_time_s)],  each = n_ens_s),
      ensemble      = rep(seq_len(n_ens_s), n_time_s),
      variable_type = "state",
      log_weight    = c(t(log_particle_weights[seq_len(n_time_s), ]))
    )
  }

  # --- ice thickness: snow_ice_thickness[layers, time, ens] ---
  n_time_ice <- dim(snow_ice_thickness)[2]
  n_ens_ice  <- dim(snow_ice_thickness)[3]
  ice_mat <- snow_ice_thickness[2, , ] + snow_ice_thickness[3, , ]  # [time, ens]

  pieces$ice <- tibble::tibble(
    predicted     = c(t(ice_mat)),
    time          = rep(full_time[seq_len(n_time_ice)],      each = n_ens_ice),
    variable      = "ice_thickness",
    depth         = NA,
    forecast      = rep(forecast_flag[seq_len(n_time_ice)],  each = n_ens_ice),
    ensemble      = rep(seq_len(n_ens_ice), n_time_ice),
    variable_type = "state",
    log_weight    = c(t(log_particle_weights[seq_len(n_time_ice), ]))
  )

  output_list <- dplyr::bind_rows(pieces)

  time_of_forecast <- lubridate::with_tz(da_forecast_output$time_of_forecast, tzone = "UTC")

  output_list <- output_list |>
    dplyr::mutate(pub_datetime = time_of_forecast,
                  reference_datetime = forecast_start_datetime,
                  site_id = config$location$site_id,
                  model_id = config$run_config$sim_name,
                  family = "ensemble") |>
    rename(datetime = time,
           parameter = ensemble,
           prediction = predicted) |>
    dplyr::select(reference_datetime, datetime, pub_datetime, model_id, site_id, depth, family, parameter, variable, prediction, forecast, variable_type, log_weight)

  #Convert to target variable name
  matched <- match(states_config$state_names, obs_config$state_names_obs)
  has_match <- !is.na(matched)
  name_map <- setNames(obs_config$target_variable[matched[has_match]],
                       states_config$state_names[has_match])
  output_list <- output_list |>
    dplyr::mutate(variable = dplyr::coalesce(unname(name_map[variable]), variable))

  output_list <- output_list |>
    mutate(reference_date = lubridate::as_date(reference_datetime))

  arrow::write_dataset(dataset = output_list,
                       path = output_directory,
                       partitioning = c("site_id", "model_id","reference_date"))

  return(output_list)
}
