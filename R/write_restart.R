##' @title Generate netcdf restart file
##' @details Function generates a netcdf file from the object that is returned by run_da_forecast()
##' @param da_forecast_output list; object that is returned by run_da_forecast()
##' @param forecast_output_directory string; full path of directory where the netcdf file will be written
##' @param use_short_filename use shortened file name; this results in less informatoin in the file name and potentially overwriting existing files
##' @return None
##' @import ncdf4
##' @import ggplot2
##' @importFrom lubridate with_tz
##' @author Quinn Thomas
##' @keywords internal

write_restart <- function(da_forecast_output,
                                  forecast_output_directory,
                                  use_short_filename = TRUE){

  dir.create(forecast_output_directory, recursive = TRUE, showWarnings = FALSE)

  states_depth <- da_forecast_output$states_depth
  states_height <- da_forecast_output$states_height
  pars <- da_forecast_output$pars
  lake_depth <- da_forecast_output$lake_depth
  snow_ice_thickness <- da_forecast_output$snow_ice_thickness
  #data_assimilation_flag <- da_forecast_output$data_assimilation_flag
  #forecast_flag <- da_forecast_output$forecast_flag
  #da_qc_flag <- da_forecast_output$da_qc_flag
  full_time <- da_forecast_output$full_time
  forecast_start_datetime <- da_forecast_output$forecast_start_datetime
  model_internal_heights <- da_forecast_output$model_internal_heights
  config <- da_forecast_output$config
  states_config <- da_forecast_output$states_config
  obs_config <- da_forecast_output$obs_config
  pars_config <- da_forecast_output$pars_config
  obs <- da_forecast_output$obs
  log_particle_weights <- da_forecast_output$log_particle_weights
  inflation <- da_forecast_output$inflation

  if(!("multi_depth" %in% names(obs_config))){
    obs_config <- obs_config |> dplyr::mutate(multi_depth = 1)
  }

  obs_config <- obs_config |>
    dplyr::filter(multi_depth == 1)

  diagnostics             <- da_forecast_output$diagnostics
  diagnostics_names       <- config$output_settings$diagnostics_names
  diagnostics_daily       <- da_forecast_output$diagnostics_daily
  diagnostics_daily_names <- config$output_settings$diagnostics_daily$names
  # save_names are unique identifiers for NC variables; fall back to names if absent
  diagnostics_daily_nc_names <- config$output_settings$diagnostics_daily$save_names
  if (is.null(diagnostics_daily_nc_names)) diagnostics_daily_nc_names <- diagnostics_daily_names

  #hist_days <- as.numeric(forecast_start_datetime - full_time[1])
  #start_forecast_step <- 1 + hist_days

  if(!is.null(pars_config)){
    npars <- nrow(pars_config)
  }else{
    npars <- 0
  }

  #states_depth_efi <- aperm(states_depth, c(1,3,4,2))
  #diagnostics_efi <- diagnostics

  #Set dimensionsda_forecast_output
  ens <- seq(1,dim(states_height)[4],1)
  #depths <- config$model_settings$modeled_depths
  t <- as.numeric(as.POSIXct(lubridate::with_tz(full_time),origin = '1970-01-01 00:00.00 UTC'))
  #obs_states <- seq(1,dim(obs)[3],1)

  # Determine which timesteps to retain in the restart file
  restart_save_timesteps <- config$output_settings$restart_save_timesteps
  if (is.null(restart_save_timesteps)) restart_save_timesteps <- 0L

  if (identical(restart_save_timesteps, "all") ||
      (length(restart_save_timesteps) == 1 && as.character(restart_save_timesteps) == "all")) {
    keep_idx <- seq_along(full_time)
  } else {
    target_dates <- as.Date(forecast_start_datetime) + as.integer(restart_save_timesteps)
    full_dates <- as.Date(full_time)
    keep_idx <- which(full_dates %in% target_dates)
    if (length(keep_idx) == 0) {
      warning("restart_save_timesteps produced no matching dates; defaulting to forecast_start_datetime.")
      keep_idx <- which(full_dates == as.Date(forecast_start_datetime))
    }
  }

  t <- t[keep_idx]

  if(!use_short_filename){
    ncfname <- file.path(forecast_output_directory, paste0(da_forecast_output$save_file_name,".nc"))
  }else{
    ncfname <- file.path(forecast_output_directory, paste0(da_forecast_output$save_file_name_short,".nc"))
  }

  #Define dims
  ensdim <- ncdf4::ncdim_def("ensemble",units = "-",vals = ens, longname = 'ensemble member')
  #depthdim <- ncdf4::ncdim_def("depth",units = "meters",vals = as.double(depths), longname = 'Depth from surface')
  timedim <- ncdf4::ncdim_def("time",units = "seconds since 1970-01-01 00:00.00 UTC", longname = "",vals = t)
  snow_ice_dim <- ncdf4::ncdim_def("snow_ice_dim",units = "",vals = c(1, 2, 3), longname = 'snow ice dims')
  internal_model_depths_dim <- ncdf4::ncdim_def("internal_model_depths_dim",units = '', vals = seq(1, dim(model_internal_heights)[2]), longname = 'number of possible depths that are simulated in GLM')
  depthdim <- ncdf4::ncdim_def("depth", units = "meters",
                                vals = as.double(config$model_settings$modeled_depths),
                                longname = "Depth from surface")

  #Define variables
  fillvalue <- 1e32

  def_list <- list()
  #def_list[[1]] <- ncdf4::ncvar_def("temp","degC",list(timedim,depthdim, ensdim),fillvalue,'state:temperature',prec="single")
  def_list[[1]] <- ncdf4::ncvar_def("snow_ice_thickness","meter", list(snow_ice_dim, timedim, ensdim),missval = -99,longname = 'Ice Thickness',prec="single")
  def_list[[2]] <- ncdf4::ncvar_def("lake_depth","meter",list(timedim,ensdim),missval = -99,longname = 'Depth of lake',prec="single")
  def_list[[3]] <- ncdf4::ncvar_def("model_internal_heights","meter",list(timedim, internal_model_depths_dim, ensdim),fillvalue,longname = "depths simulated by glm that are required to restart ",prec="single")
  def_list[[4]] <- ncdf4::ncvar_def("log_particle_weights","dimensionless",list(timedim, ensdim),missval = fillvalue,longname = "log weights for each ensemble member",prec="single")
  def_list[[5]] <- ncdf4::ncvar_def("inflation","dimensionless",list(timedim),missval = fillvalue,longname = "adaptive inflation parameter",prec="single")
  index <- 5

  if(isTRUE(npars > 0)){
    for(par in 1:npars){
      def_list[[index+par]] <-ncdf4::ncvar_def(pars_config$par_names_save[par],pars_config$par_units[par],list(timedim,ensdim),fillvalue,paste0("parameter:",pars_config$par_names_save[par]),prec="single")
    }
  }

  # for(s in 1:length(states_config$state_names)){
  #   if(states_config$state_names[s] %in% obs_config$state_names_obs){
  #     tmp_index <- which(obs_config$state_names_obs == states_config$state_names[s])
  #     long_name <- paste0("state:",obs_config$target_variable[tmp_index])
  #   }else{
  #     long_name <- "state"
  #   }
  #   if(states_config$state_names[s] == "temp"){
  #     state_unit <- "degC"
  #   }else if(states_config$state_names[s] == "salt"){
  #     state_unit <- "g_kg"
  #   }else{
  #     state_unit <- "mmol m-3"
  #   }
  #   def_list[[index+npars+ s ]]<- ncdf4::ncvar_def(states_config$state_names[s],state_unit,list(timedim,depthdim, ensdim),fillvalue,long_name,prec="single")
  # }
  #
  # if(length(config$output_settings$diagnostics_names) > 0){
  #   for(s in 1:length(config$output_settings$diagnostics_names)){
  #     def_list[[index+npars+length(states_config$state_names) + s]]<- ncdf4::ncvar_def(config$output_settings$diagnostics_names[s],"-",list(timedim,depthdim, ensdim),fillvalue,paste0("diagnostic:",config$output_settings$diagnostics_names[s]),prec="single")
  #   }
  # }

  tmp_index <- index + npars
  # for(s in 1:length(obs_config$state_names_obs)){
  #   if(!obs_config$state_names_obs[s] %in% states_config$state_names){
  #     tmp_index <- tmp_index + 1
  #     longname <- paste0("state:",obs_config$target_variable[s])
  #     def_list[[tmp_index]] <- ncdf4::ncvar_def(obs_config$state_names_obs[s],obs_config$obs_units[s],list(timedim,depthdim, ensdim),fillvalue,longname,prec="single")
  #   }
  # }

  for(s in 1:length(states_config$state_names)){
    tmp_index <- tmp_index + 1
    if(states_config$state_names[s] %in% obs_config$state_names_obs){
      id <- which(obs_config$state_names_obs == states_config$state_names[s])
      long_name <- paste0("restart:",obs_config$target_variable[id])
    }else{
      long_name <- "restart"
    }
    if(states_config$state_names[s] == "temp"){
      state_unit <- "degC"
    }else if(states_config$state_names[s] == "salt"){
      state_unit <- "g_kg"
    }else{
      state_unit <- "mmol m-3"
    }
    def_list[[tmp_index]]<- ncdf4::ncvar_def(paste0(states_config$state_names[s],"_heights"),state_unit,list(timedim,internal_model_depths_dim,ensdim),fillvalue,long_name,prec="single")
  }

  if(is.array(diagnostics) && length(diagnostics_names) > 0){
    for(d in seq_along(diagnostics_names)){
      tmp_index <- tmp_index + 1
      def_list[[tmp_index]] <- ncdf4::ncvar_def(
        paste0("diag_", diagnostics_names[d]), "-",
        list(timedim, depthdim, ensdim),
        fillvalue, paste0("diagnostic:", diagnostics_names[d]), prec = "single"
      )
    }
  }

  if(is.array(diagnostics_daily) && length(diagnostics_daily_names) > 0){
    for(d in seq_along(diagnostics_daily_names)){
      tmp_index <- tmp_index + 1
      def_list[[tmp_index]] <- ncdf4::ncvar_def(
        paste0("diag_daily_", diagnostics_daily_nc_names[d]), "-",
        list(timedim, ensdim),
        fillvalue, paste0("diagnostic_daily:", diagnostics_daily_nc_names[d]), prec = "single"
      )
    }
  }

  ncout <- ncdf4::nc_create(ncfname,def_list,force_v4=T)

  # create netCDF file and put arrays
  #ncdf4::ncvar_put(ncout,def_list[[1]] ,as.array(data_assimilation_flag))
  #ncdf4::ncvar_put(ncout,def_list[[2]] ,as.array(forecast_flag))
  #ncdf4::ncvar_put(ncout,def_list[[3]] ,as.array(da_qc_flag))
  # dim layout: snow_ice_thickness [snow_ice_dim, time, ens]
  ncdf4::ncvar_put(ncout, def_list[[1]], snow_ice_thickness[, keep_idx, , drop = FALSE])
  # dim layout: lake_depth, log_particle_weights [time, ens]
  ncdf4::ncvar_put(ncout, def_list[[2]], lake_depth[keep_idx, , drop = FALSE])
  # dim layout: model_internal_heights [time, internal_model_depths_dim, ens]
  ncdf4::ncvar_put(ncout, def_list[[3]], model_internal_heights[keep_idx, , , drop = FALSE])
  ncdf4::ncvar_put(ncout, def_list[[4]], log_particle_weights[keep_idx, , drop = FALSE])
  ncdf4::ncvar_put(ncout, def_list[[5]], inflation[keep_idx])

  index <- 5

  if(isTRUE(npars > 0)){
    for(par in 1:npars){
      pars_par <- pars[, par, ]
      ncdf4::ncvar_put(ncout, def_list[[index + par]], pars_par[keep_idx, , drop = FALSE])
    }
  }

  tmp_index <- index + npars

  for(s in 1:length(states_config$state_names)){
    tmp_index <- tmp_index + 1
    # dim layout: states_height [time, internal_model_depths_dim, ens] per state
    state_data <- states_height[, s, ,]
    ncdf4::ncvar_put(ncout, def_list[[tmp_index]], state_data[keep_idx, , , drop = FALSE])
  }

  if(is.array(diagnostics) && length(diagnostics_names) > 0){
    for(d in seq_along(diagnostics_names)){
      tmp_index <- tmp_index + 1
      # diagnostics dim: [ndiag, nsteps, ndepths, nmembers]
      diag_data <- diagnostics[d, , , ]
      ncdf4::ncvar_put(ncout, def_list[[tmp_index]], diag_data[keep_idx, , , drop = FALSE])
    }
  }

  if(is.array(diagnostics_daily) && length(diagnostics_daily_names) > 0){
    for(d in seq_along(diagnostics_daily_names)){
      tmp_index <- tmp_index + 1
      # diagnostics_daily dim: [ndiag_daily, nsteps, nmembers]
      daily_data <- diagnostics_daily[d, , ]
      ncdf4::ncvar_put(ncout, def_list[[tmp_index]], daily_data[keep_idx, , drop = FALSE])
    }
  }

  time_of_forecast <- lubridate::with_tz(da_forecast_output$time_of_forecast, tzone = "UTC")

  #Global file metadata
  ncdf4::ncatt_put(ncout,0,"forecast_model_id", config$run_config$sim_name, prec =  "text")
  ncdf4::ncatt_put(ncout,0,"time_zone_of_simulation","UTC", prec =  "text")

  ncdf4::nc_close(ncout)

  # Build restart zip containing the FLARE NetCDF and per-date GLM restart files
  glm_restart_staged <- da_forecast_output$glm_restart_staged

  # Filter GLM restart dates to match the timesteps kept in the NetCDF
  keep_dates <- format(as.Date(full_time[keep_idx]), "%Y-%m-%d")
  glm_restart_staged <- glm_restart_staged[
    intersect(names(glm_restart_staged), keep_dates)
  ]

  if(!is.null(glm_restart_staged) && length(glm_restart_staged) > 0) {
    tmp_dir <- tempfile()
    dir.create(tmp_dir, recursive = TRUE)
    file.copy(ncfname, tmp_dir)

    for(date_label in names(glm_restart_staged)) {
      date_dir <- file.path(tmp_dir, date_label)
      dir.create(date_dir, recursive = TRUE)
      for(rst_name in names(glm_restart_staged[[date_label]])) {
        raw_data <- glm_restart_staged[[date_label]][[rst_name]]
        if(!is.null(raw_data)) {
          writeBin(raw_data, file.path(date_dir, rst_name))
        }
      }
    }

    zip_name <- paste0(da_forecast_output$save_file_name_short, ".zip")
    zip_path <- file.path(forecast_output_directory, zip_name)
    all_files <- list.files(tmp_dir, recursive = TRUE)
    zip::zip(zipfile = zip_path, files = all_files,
             recurse = TRUE, root = tmp_dir, mode = "mirror")
    message("GLM restart zip written to: ", zip_path)
    unlink(tmp_dir, recursive = TRUE)
    invisible(zip_path)
  } else {
    invisible(ncfname)
  }

}
