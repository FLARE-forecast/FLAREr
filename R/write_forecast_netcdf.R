##' @title Download and Downscale NOAA GEFS for a single site
##' @return None
##'
##' @param site_index, index of site_list, lat_list, lon_list to be downloaded
##' @param lat_list, vector of latitudes that correspond to site codes
##' @param lon_list, vector of longitudes that correspond to site codes
##' @param site_list, vector of site codes, used in directory and file name generation
##' @param downscale, logical specifying whether to downscale from 6-hr to 1-hr
##' @param overwrite, logical stating to overwrite any existing output_file
##' @param model_name, directory name for the 6-hr forecast, this will be used in directory and file name generation
##' @param model_name_ds, directory name for the 1-hr forecast, this will be used in directory and file name generation
##' @param output_directory, directory where the model output will be save
##' @export
##'
##' @author Quinn Thomas
##'
##'

write_forecast_netcdf <- function(enkf_output,
                                  forecast_location){


  x <- enkf_output$x
  x_restart <- enkf_output$x_restart
  qt_restart <- enkf_output$qt_restart
  x_prior <- enkf_output$x_prior
  surface_height_restart <- enkf_output$surface_height_restart
  snow_ice_restart <- enkf_output$snow_ice_restart
  snow_ice_thickness <- enkf_output$snow_ice_thickness
  surface_height <- enkf_output$surface_height
  data_assimilation_flag <- enkf_output$data_assimilation_flag
  full_time_local <- enkf_output$full_time_local
  forecast_start_datetime <- enkf_output$forecast_start_datetime
  avg_surf_temp_restart <- enkf_output$avg_surf_temp_restart
  mixing_restart <- enkf_output$mixing_restart
  glm_depths_restart <- enkf_output$glm_depths_restart
  salt <- enkf_output$salt
  salt_restart <- enkf_output$salt_restart
  config <- enkf_output$config
  states_config <- enkf_output$states_config
  obs_config <- enkf_output$obs_config
  pars_config <- enkf_output$pars_config
  obs <- enkf_output$obs

  diagnostics <- enkf_output$diagnostics

  hist_days <- as.numeric(forecast_start_datetime - full_time_local[1])
  start_forecast_step <- 1 + hist_days

  npars <- nrow(pars_config)
  nstates <- dim(enkf_output$x)[3] - npars

  x_efi <- aperm(x, c(1,3,2))
  diagnostics_efi <- aperm(diagnostics, c(1,3,2, 4))

  ncfname <- paste0(forecast_location,"/",enkf_output$save_file_name,"_",enkf_output$forecast_iteration_id,".nc")
  #Set dimensions
  ens <- seq(1,dim(x)[2],1)
  depths <- config$modeled_depths
  t <- as.numeric(as.POSIXct(lubridate::with_tz(full_time_local),origin = '1970-01-01 00:00.00 UTC'))
  states <- seq(1,nstates,1)
  states_aug <- seq(1,dim(x)[3],1)
  #obs_states <- seq(1,dim(obs)[3],1)
  mixing_restart_vars <- seq(1, dim(mixing_restart)[2], 1)

  restart_depths <- seq(1, dim(glm_depths_restart)[2])

  #Set variable that states whether value is forecasted
  forecasted <- rep(1, length(t))
  forecasted[1:(hist_days + 1)] <- 0

  ice_thickness <- snow_ice_thickness[, ,2] + snow_ice_thickness[, , 3]

  #Define dims
  ensdim <- ncdf4::ncdim_def("ensemble",units = "-",vals = ens, longname = 'ensemble member')
  depthdim <- ncdf4::ncdim_def("depth",units = "meters",vals = as.double(depths), longname = 'Depth from surface')
  timedim <- ncdf4::ncdim_def("time",units = 'seconds', longname = 'seconds since 1970-01-01 00:00.00 UTC',vals = t)
  statedim <- ncdf4::ncdim_def("states",units = '', vals = states)
  stateagudim <- ncdf4::ncdim_def("states_aug",units = '', vals = states_aug, longname = 'length of model states plus parameters')
  #obsdim <- ncdf4::ncdim_def("obs_dim",units = '', vals = obs_states, longname = 'length of ')
  snow_ice_dim <- ncdf4::ncdim_def("snow_ice_dim",units = "",vals = c(1, 2, 3), longname = 'snow ice dims')
  mixing_restart_vars_dim <- ncdf4::ncdim_def("mixing_restart_vars_dim",units = '', vals = mixing_restart_vars, longname = 'number of mixing restart variables')
  depth_restart_vars_dim <- ncdf4::ncdim_def("depth_restart_vars_dim",units = '', vals = restart_depths, longname = 'number of possible depths that are simulated in GLM')


  #Define variables
  fillvalue <- 1e32

  def_list <- list()
  def_list[[1]] <- ncdf4::ncvar_def("temp","degC",list(timedim,depthdim, ensdim),fillvalue,'state: temperature',prec="single")
  def_list[[2]]  <- ncdf4::ncvar_def("x_restart","dimensionless",list(ensdim,stateagudim),fillvalue,'matrix for restarting EnKF',prec="float")
  def_list[[3]] <- ncdf4::ncvar_def("x_prior","dimensionless",list(timedim,ensdim,stateagudim),fillvalue,'Predicted states prior to Kalman correction',prec="float")
  def_list[[4]] <- ncdf4::ncvar_def("data_assimilation","dimensionless",list(timedim),missval = -99,longname = '0 = historical; 1 = forecasted',prec="integer")
  def_list[[5]] <- ncdf4::ncvar_def("surface_height_restart","meter",list(ensdim),missval = -99,longname =  'Surface Height',prec="single")
  def_list[[6]] <- ncdf4::ncvar_def("snow_ice_restart","meter",list(ensdim, snow_ice_dim),missval = -99,longname = 'Snow (1), White Ice (2), Blue Ice (3)',prec="single")
  def_list[[7]] <- ncdf4::ncvar_def("ice_thickness","meter", list(timedim,ensdim),missval = -99,longname = 'Ice Thickness',prec="single")
  def_list[[8]] <- ncdf4::ncvar_def("lake_depth","meter",list(timedim,ensdim),missval = -99,longname = 'Depth of lake',prec="single")
  def_list[[9]] <- ncdf4::ncvar_def("avg_surf_temp_restart","degC",list(ensdim),missval = -99,longname ='Running Average of Surface Temperature',prec="single")
  def_list[[10]] <- ncdf4::ncvar_def("mixing_restart","dimensionless",list(ensdim,mixing_restart_vars_dim),fillvalue,longname = "variables required to restart mixing",prec="single")
  def_list[[11]] <- ncdf4::ncvar_def("depths_restart","meter",list(ensdim,depth_restart_vars_dim),fillvalue,longname = "depths simulated by glm that are required to restart ",prec="single")
  def_list[[12]] <- ncdf4::ncvar_def("salt_restart","g_kg",list(ensdim,depthdim),fillvalue,longname = "salt restart ",prec="single")
  def_list[[13]] <- ncdf4::ncvar_def("salt","g_kg",list(timedim, depthdim, ensdim),fillvalue,longname = "salt",prec="single")


  index <- 13

  for(i in 1:length(obs_config$state_names_obs)){
    long_name1 <- "observed"
    long_name2 <- obs_config$state_names_obs[i]
    long_name3 <- NULL
    long_name4 <- NULL

    long_name5 <- obs_config$target_variable[i]
    long_name6 <- obs_config$distance_threshold[i]

    for(j in 1:nrow(states_config)){
      for(k in 1:length(states_config$states_to_obs[[j]])){
        if(!is.na(states_config$states_to_obs[[j]][k])){
          if(states_config$states_to_obs[[j]][k] == i){
            if(is.null(long_name3)){
              long_name3 <- states_config$state_names[[j]]
              long_name4 <- states_config$states_to_obs_mapping[[j]][k]
            }else{
              long_name3 <- paste(long_name3, states_config$state_names[[j]],sep="-")
              long_name4 <- paste(long_name4, states_config$states_to_obs_mapping[[j]][k],sep="-")
            }
          }
        }
        long_name <- paste(long_name1,long_name2,long_name3,long_name4, long_name5, long_name6, sep = ":")
      }
    }




    def_list[[index+i]] <-ncdf4::ncvar_def(paste0(obs_config$state_names_obs[i],"_observed"),obs_config$obs_units[i],list(timedim, depthdim),fillvalue,long_name,prec="single")
  }

  index <- index + length(obs_config$state_names_obs)

  if(npars > 0){
    for(par in 1:npars){
      def_list[[index+par]] <-ncdf4::ncvar_def(pars_config$par_names_save[par],pars_config$par_units[par],list(timedim,ensdim),fillvalue,paste0("parameter:",pars_config$par_names_save[par]),prec="single")
    }
  }


  if(config$include_wq){
    for(s in 2:length(states_config$state_names)){
      def_list[[index+npars+s-1]]<- ncdf4::ncvar_def(states_config$state_names[s],"mmol m-3",list(timedim,depthdim, ensdim),fillvalue,paste("state:", states_config$state_names[s]),prec="single")
    }
  }

  if(length(config$diagnostics_names) > 0){
    for(s in 1:length(config$diagnostics_names)){
      def_list[[index+npars+length(states_config$state_names)-1 + s]]<- ncdf4::ncvar_def(config$diagnostics_names[s],"-",list(timedim,depthdim, ensdim),fillvalue,paste0("diagnostic:",config$diagnostics_names[s]),prec="single")
    }
  }

  ncout <- ncdf4::nc_create(ncfname,def_list,force_v4=T)

  # create netCDF file and put arrays
  ncdf4::ncvar_put(ncout,def_list[[1]] ,x_efi[,1:length(depths),])
  ncdf4::ncvar_put(ncout,def_list[[2]] ,as.matrix(x_restart))
  ncdf4::ncvar_put(ncout,def_list[[3]] ,as.matrix(x_prior))
  ncdf4::ncvar_put(ncout,def_list[[4]] ,as.array(data_assimilation_flag))
  ncdf4::ncvar_put(ncout,def_list[[5]] ,surface_height_restart)
  ncdf4::ncvar_put(ncout,def_list[[6]] ,snow_ice_restart)
  ncdf4::ncvar_put(ncout,def_list[[7]] ,ice_thickness)
  ncdf4::ncvar_put(ncout,def_list[[8]] ,surface_height)
  ncdf4::ncvar_put(ncout,def_list[[9]] ,avg_surf_temp_restart)
  ncdf4::ncvar_put(ncout,def_list[[10]] ,mixing_restart)
  ncdf4::ncvar_put(ncout,def_list[[11]] ,glm_depths_restart)
  ncdf4::ncvar_put(ncout,def_list[[12]] ,salt_restart)
  ncdf4::ncvar_put(ncout,def_list[[13]] ,salt)

  index <- 13

  for(i in 1:length(obs_config$state_names_obs)){
    ncdf4::ncvar_put(ncout,def_list[[index + i]] ,obs[i,,])
  }

  index <- index + length(obs_config$state_names_obs)

  if(npars > 0){
    for(par in 1:npars){
      ncdf4::ncvar_put(ncout,def_list[[index + par]] ,x[,,nstates + par])
    }
  }

  if(config$include_wq){
    for(s in 2:length(states_config$state_names)){
      ncdf4::ncvar_put(ncout,def_list[[index+npars+s-1]],x_efi[,states_config$wq_start[s]:states_config$wq_end[s], ])
    }

  }

  if(length(config$diagnostics_names) > 0){
    for(s in 1:length(config$diagnostics_names)){
      ncdf4::ncvar_put(ncout, def_list[[index+npars+length(states_config$state_names) - 1 + s]],diagnostics_efi[, , ,s])
    }
  }

  time_of_forecast <- lubridate::with_tz(enkf_output$time_of_forecast, tzone = "UTC")

  #Global file metadata
  ncdf4::ncatt_put(ncout,0,"title",enkf_output$config$metadata$forecast_title, prec =  "text")
  ncdf4::ncatt_put(ncout,0,"forecast_iteration_id",enkf_output$forecast_iteration_id, prec =  "text")
  ncdf4::ncatt_put(ncout,0,"forecast_project_id",enkf_output$config$forecast_project_id, prec =  "text")
  ncdf4::ncatt_put(ncout,0,"local_time_zone_of_simulation",as.character(config$local_tzone), prec =  "text")
  ncdf4::ncatt_put(ncout,0,"forecast_issue_time",paste0(as.character(time_of_forecast),"Z"), prec =  "text")

  ncdf4::nc_close(ncout)

  invisible(ncfname)

}

