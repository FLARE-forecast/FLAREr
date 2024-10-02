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
  avg_surf_temp <- da_forecast_output$avg_surf_temp
  mixing_vars <- da_forecast_output$mixing_vars
  model_internal_heights <- da_forecast_output$model_internal_heights
  config <- da_forecast_output$config
  states_config <- da_forecast_output$states_config
  obs_config <- da_forecast_output$obs_config
  pars_config <- da_forecast_output$pars_config
  obs <- da_forecast_output$obs
  mixer_count <- da_forecast_output$mixer_count
  log_particle_weights <- da_forecast_output$log_particle_weights

  if(!("multi_depth" %in% names(obs_config))){
    obs_config <- obs_config |> dplyr::mutate(multi_depth = 1)
  }

  obs_config <- obs_config |>
    dplyr::filter(multi_depth == 1)

  #diagnostics <- da_forecast_output$diagnostics

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

  #Set variable that states whether value is forecasted
  #forecasted <- rep(1, length(t))
  #forecasted[1:(hist_days + 1)] <- 0

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
  mixing_vars_dim <- ncdf4::ncdim_def("mixing_vars_dim",units = '', vals = seq(1, dim(mixing_vars)[1], 1), longname = 'number of mixing restart variables')
  internal_model_depths_dim <- ncdf4::ncdim_def("internal_model_depths_dim",units = '', vals = seq(1, dim(model_internal_heights)[2]), longname = 'number of possible depths that are simulated in GLM')


  #Define variables
  fillvalue <- 1e32

  def_list <- list()
  #def_list[[1]] <- ncdf4::ncvar_def("temp","degC",list(timedim,depthdim, ensdim),fillvalue,'state:temperature',prec="single")
  def_list[[1]] <- ncdf4::ncvar_def("snow_ice_thickness","meter", list(snow_ice_dim, timedim, ensdim),missval = -99,longname = 'Ice Thickness',prec="single")
  def_list[[2]] <- ncdf4::ncvar_def("lake_depth","meter",list(timedim,ensdim),missval = -99,longname = 'Depth of lake',prec="single")
  def_list[[3]] <- ncdf4::ncvar_def("avg_surf_temp","degC",list(timedim, ensdim),missval = -99,longname ='Running Average of Surface Temperature',prec="single")
  def_list[[4]] <- ncdf4::ncvar_def("mixing_vars","dimensionless",list(mixing_vars_dim, timedim, ensdim),fillvalue,longname = "variables required to restart mixing",prec="single")
  def_list[[5]] <- ncdf4::ncvar_def("model_internal_heights","meter",list(timedim, internal_model_depths_dim, ensdim),fillvalue,longname = "depths simulated by glm that are required to restart ",prec="single")
  def_list[[6]] <- ncdf4::ncvar_def("mixer_count","dimensionless",list(timedim,  ensdim),missval = -99,longname = "restart for mixer count",prec="integer")
  def_list[[7]] <- ncdf4::ncvar_def("log_particle_weights","dimensionless",list(timedim, ensdim),missval = fillvalue,longname = "log weights for each ensemble member",prec="single")

  index <- 7

  if(npars > 0){
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

  ncout <- ncdf4::nc_create(ncfname,def_list,force_v4=T)

  # create netCDF file and put arrays
  #ncdf4::ncvar_put(ncout,def_list[[1]] ,as.array(data_assimilation_flag))
  #ncdf4::ncvar_put(ncout,def_list[[2]] ,as.array(forecast_flag))
  #ncdf4::ncvar_put(ncout,def_list[[3]] ,as.array(da_qc_flag))
  ncdf4::ncvar_put(ncout,def_list[[1]] ,snow_ice_thickness)
  ncdf4::ncvar_put(ncout,def_list[[2]] ,lake_depth)
  ncdf4::ncvar_put(ncout,def_list[[3]] ,avg_surf_temp)
  ncdf4::ncvar_put(ncout,def_list[[4]] ,mixing_vars)
  ncdf4::ncvar_put(ncout,def_list[[5]] ,model_internal_heights)
  ncdf4::ncvar_put(ncout,def_list[[6]] ,mixer_count)
  ncdf4::ncvar_put(ncout,def_list[[7]] ,log_particle_weights)

  index <- 7

  if(npars > 0){
    for(par in 1:npars){
      ncdf4::ncvar_put(ncout,def_list[[index + par]] ,pars[,par, ])
    }
  }

   tmp_index <- index + npars
  # for(s in 1:length(obs_config$state_names_obs)){
  #   if(!(obs_config$state_names_obs[s] %in% states_config$state_names) &
  #      obs_config$multi_depth[s] == 1){
  #     tmp_index <- tmp_index + 1
  #     first_index <- 1
  #     for(ii in 1:length(states_config$state_names)){
  #       if(s %in% states_config$states_to_obs[[ii]]){
  #         temp_index <- which(states_config$states_to_obs[[ii]] == s)
  #         if(first_index == 1){
  #           temp_var <- states_depth_efi[, , , ii] * states_config$states_to_obs_mapping[[ii]][temp_index]
  #           first_index <- 2
  #         }else{
  #           temp_var <- temp_var + states_depth_efi[, , , ii] * states_config$states_to_obs_mapping[[ii]][temp_index]
  #         }
  #       }
  #     }
  #     ncdf4::ncvar_put(ncout,def_list[[tmp_index]] , temp_var)
  #
  #   }
  # }

  for(s in 1:length(states_config$state_names)){
    tmp_index <- tmp_index + 1
    ncdf4::ncvar_put(ncout,def_list[[tmp_index]],states_height[, s, ,])
  }

  time_of_forecast <- lubridate::with_tz(da_forecast_output$time_of_forecast, tzone = "UTC")

  #Global file metadata
  ncdf4::ncatt_put(ncout,0,"forecast_model_id", config$run_config$sim_name, prec =  "text")
  ncdf4::ncatt_put(ncout,0,"time_zone_of_simulation","UTC", prec =  "text")

  ncdf4::nc_close(ncout)

  invisible(ncfname)

}
