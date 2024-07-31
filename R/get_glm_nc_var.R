#' Add together two numbers.
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @noRd
#' @examples
#' add(1, 1)
#' add(10, 1)
get_glm_nc_var <- function(ncFile, working_dir, z_out, vars_depth, vars_no_depth,
                           diagnostic_vars)
{
  glm_nc <- ncdf4::nc_open(paste0(working_dir, ncFile))
  glm_vars <- names(glm_nc$var)
  tallest_layer <- ncdf4::ncvar_get(glm_nc, "NS")
  final_time_step <- length(tallest_layer)
  tallest_layer <- tallest_layer[final_time_step]
  heights <- matrix(ncdf4::ncvar_get(glm_nc, "z"), ncol = final_time_step)
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
              mixer_count = mixer_count))
}


