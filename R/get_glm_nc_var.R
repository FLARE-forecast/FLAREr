get_glm_nc_var <- function(ncFile, working_dir, z_out, vars_depth, vars_no_depth,
                           diagnostic_vars, diagnostics_daily_config)
{
  glm_nc <- ncdf4::nc_open(paste0(working_dir, ncFile))
  glm_vars <- names(glm_nc$var)
  tallest_layer_all <- ncdf4::ncvar_get(glm_nc, "NS")
  final_time_step <- length(tallest_layer_all)
  tallest_layer <- tallest_layer_all[final_time_step]
  heights_all <- ncdf4::ncvar_get(glm_nc, "z")
  heights <- matrix(heights_all, ncol = final_time_step)
  heights_surf <- heights[tallest_layer, final_time_step]
  heights <- heights[1:tallest_layer, final_time_step]

  snow <- matrix(ncdf4::ncvar_get(glm_nc, "snow_thickness"),
                 ncol = final_time_step)[final_time_step]
  ice_white <- matrix(ncdf4::ncvar_get(glm_nc, "white_ice_thickness"),
                      ncol = final_time_step)[final_time_step]
  ice_blue <- matrix(ncdf4::ncvar_get(glm_nc, "blue_ice_thickness"),
                     ncol = final_time_step)[final_time_step]
  avg_surf_temp <- matrix(ncdf4::ncvar_get(glm_nc, "avg_surf_temp"),
                          ncol = final_time_step)[final_time_step]
  output <- array(NA, dim = c(tallest_layer, length(vars_depth)))
  for (v in 1:length(vars_depth)) {
    var_modeled <- matrix(ncdf4::ncvar_get(glm_nc, vars_depth[v]),
                          ncol = final_time_step)
    output[, v] <- var_modeled[1:tallest_layer, final_time_step]
  }
  output_no_depth <- NA
  if (length(diagnostic_vars) > 0) {
    diagnostics_output <- array(NA, dim = c(tallest_layer,
                                            length(diagnostic_vars)))
    for (v in 1:length(diagnostic_vars)) {
      var_modeled <- matrix(ncdf4::ncvar_get(glm_nc, diagnostic_vars[v]),
                            ncol = final_time_step)
      diagnostics_output[, v] <- var_modeled[1:tallest_layer,
                                             final_time_step]
    }
  }else {
    diagnostics_output <- NA
  }

  mixing_vars <- ncdf4::ncvar_get(glm_nc, "restart_variables")
  salt <- matrix(ncdf4::ncvar_get(glm_nc, "salt"), ncol = final_time_step)[1:tallest_layer,
                                                                           final_time_step]

  mixer_count <- matrix(ncdf4::ncvar_get(glm_nc, "Mixer_Count"), ncol = final_time_step)[final_time_step]

  if(length(heights) == 1){
    output <- rbind(output, output)
    heights <- c(heights/2, heights)
    diagnostics_output <- rbind(diagnostics_output, diagnostics_output)
    salt <- c(salt, salt)
  }

  if(length(diagnostics_daily_config$names) > 0){
    diagnostics_daily_output <- array(NA, dim = c(length(diagnostics_daily_config$names)))
    for(v in 1:length(diagnostics_daily_config$names)){
      if(tools::file_ext(diagnostics_daily_config$file[v]) == "csv"){
        diagnostics_daily_output[v] <- readr::read_csv(file.path(working_dir, diagnostics_daily_config$file[v]), show_col_types = FALSE) |>
          dplyr::pull(diagnostics_daily_config$names[v])
      }else if(tools::file_ext(diagnostics_daily_config$file[v]) == "nc"){

        if(!is.na(diagnostics_daily_config$depth[v])){
          time <- ncdf4::ncvar_get(glm_nc, "time" )
          var <- ncdf4::ncvar_get(glm_nc, diagnostics_daily_config$names[v])
          if(length(time) == 0){
            stop("You requested a diagnostics_daily_config from the output.nc but
                 the nsave value in the base glm.nml only allows one output value
                 per day (thus can't calculate a daily statistic). Decrease the nsave value")
          }
          var2 <- rep(NA, length(time))
          for(t in 1:length(time)){
            max_height <- max(heights_all[1:tallest_layer_all[t], t])
            depths <- max_height - heights_all[1:tallest_layer_all[t], t]
            var2[t] <- approx(depths, var[1:tallest_layer_all[t], t], diagnostics_daily_config$depth[v])$y
          }

        }else{
          var2 <- ncdf4::ncvar_get(glm_nc, diagnostics_daily_config$names[v])
        }

        if(stringr::str_detect(diagnostics_daily_config$save_names[v], "mean")){

          diagnostics_daily_output[v] <- mean(var2, na.rm = TRUE)

        }else if(stringr::str_detect(diagnostics_daily_config$save_names[v], "max")){

          diagnostics_daily_output[v] <- max(var2, na.rm = TRUE)

        }else if(stringr::str_detect(diagnostics_daily_config$save_names[v], "min")){

          diagnostics_daily_output[v] <- min(var2, na.rm = TRUE)

        }
      }
    }
  }else{
    diagnostics_daily_output <- NA
  }

  ncdf4::nc_close(glm_nc)

  return(list(output = output,
              output_no_depth = output_no_depth,
              lake_depth = heights_surf,
              heights = heights,
              snow_wice_bice = c(snow, ice_white, ice_blue),
              avg_surf_temp = avg_surf_temp,
              mixing_vars = mixing_vars,
              salt = salt,
              diagnostics_output = diagnostics_output,
              diagnostics_daily_output = diagnostics_daily_output,
              mixer_count = mixer_count))
}


