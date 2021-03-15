get_ler_nc_var_all <- function(model, working_dir, z_out, vars_depth, vars_no_depth, diagnostic_vars){

  if( model == "GLM") {

    glm_nc <- ncdf4::nc_open(file.path(working_dir, model, "output", "output.nc"))
    tallest_layer <- ncdf4::ncvar_get(glm_nc, "NS")
    final_time_step <- length(tallest_layer)
    tallest_layer <- tallest_layer[final_time_step] # Edited
    heights <- ncdf4::ncvar_get(glm_nc, "z")
    heights_surf <- heights[tallest_layer, final_time_step]
    heights <- heights[1:tallest_layer, final_time_step]
    heights_out <- heights_surf - z_out

    snow <- ncdf4::ncvar_get(glm_nc, "hsnow")[final_time_step]
    ice_white <- ncdf4::ncvar_get(glm_nc, "hwice")[final_time_step]
    ice_blue <- ncdf4::ncvar_get(glm_nc, "hice")[final_time_step]
    avg_surf_temp <- ncdf4::ncvar_get(glm_nc, "avg_surf_temp")[final_time_step]


    glm_temps <- ncdf4::ncvar_get(glm_nc, "temp")[1:tallest_layer, final_time_step]



    output <- array(NA, dim=c(tallest_layer,length(vars_depth)))
    for(v in 1:length(vars_depth)){
      var_modeled <- ncdf4::ncvar_get(glm_nc, vars_depth[v])[, final_time_step]
      output[,v] <- var_modeled[1:tallest_layer]
    }

    output_no_depth <- NA

    if(length(diagnostic_vars) > 0){
      diagnostics_output <- array(NA,dim=c(tallest_layer, length(diagnostic_vars)))
      for(v in 1:length(diagnostic_vars)){
        var_modeled <- ncvar_get(glm_nc, diagnostic_vars[v])[, final_time_step]
        diagnostics_output[,v] <- var_modeled[1:tallest_layer]
      }
    }else{
      diagnostics_output <- NA
    }

    mixing_vars <- ncdf4::ncvar_get(glm_nc, "restart_variables")

    salt <- ncdf4::ncvar_get(glm_nc, "salt")[1:tallest_layer]

    ncdf4::nc_close(glm_nc)
  }

  # GOTM ----
  if( model == "GOTM") {

    nc <- ncdf4::nc_open(file.path(working_dir, model, "output", "output.nc"))
    heights <- abs(ncdf4::ncvar_get(nc, "z")) # WARNING if positive depths
    final_time_step <- ncol(heights)
    heights_surf <- max(heights[, final_time_step])
    heights <- heights[, final_time_step]
    heights_out <- heights_surf - z_out

    temps <- ncdf4::ncvar_get(nc, "temp")[, final_time_step]


    snow <- 0
    ice_white <- ncdf4::ncvar_get(nc, "Hice")[final_time_step]
    ice_blue <- 0
    avg_surf_temp <- NA



    output <- array(NA, dim=c(length(temps), length(vars_depth)))
    for(v in 1:length(vars_depth)){
      output[,v] <- ncdf4::ncvar_get(nc, vars_depth[v])[, final_time_step]
    }

    output_no_depth <- NA

    if(length(diagnostic_vars) > 0){
      diagnostics_output <- array(NA,dim=c(tallest_layer, length(diagnostic_vars)))
      for(v in 1:length(diagnostic_vars)){
        var_modeled <- ncdf4::ncvar_get(nc, diagnostic_vars[v])[, final_time_step]
        diagnostics_output[,v] <- var_modeled[1:tallest_layer]
      }
    }else{
      diagnostics_output <- NULL
    }

    mixing_vars <- NA # ncdf4::ncvar_get(nc, "restart_variables")

    salt <- ncdf4::ncvar_get(nc, "salt")[, final_time_step]

    ncdf4::nc_close(nc)
  }



  return(list(output = output,
              output_no_depth = output_no_depth,
              lake_depth = heights_surf,
              depths_enkf = heights[1] - heights,
              snow_wice_bice = c(snow, ice_white, ice_blue),
              avg_surf_temp = avg_surf_temp,
              mixing_vars = mixing_vars,
              salt = salt,
              diagnostics_output = diagnostics_output))
}
