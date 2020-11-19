#FUNCTIONS Shared between the MCMC and EnKF



#Set GLM Initial conditions for temperature at each layer from first observations
#' Add together two numbers.
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @noRd
#' @examples
#' add(1, 1)
#' add(10, 1)
update_temps <- function(curr_temps,curr_depths,working_directory){
  orig_nml = read_nml(paste0(working_directory,'/','glm3.nml'))
  index1 = NA; index2 = NA;index3 = NA; index4 = NA
  for (g in 1:length(orig_nml)) {
    for (q in 1:length(orig_nml[[g]])) {
      if (names(orig_nml[[g]][q]) == "the_temps") {
        temps = as.numeric(as.character(unlist(orig_nml[[g]][q])))
        index1 = g; index2 = q;
      }
      if (names(orig_nml[[g]][q]) == "the_depths") {
        depths = as.numeric(as.character(unlist(orig_nml[[g]][q])))
        index3 = g; index4 = q
      }
    }
  }
  temp_inter = approxfun(curr_depths,curr_temps,rule=2)
  init_temps = temp_inter(depths)
  char_temps = paste(init_temps, collapse = ', ')
  holder2 = unlist(orig_nml[[index1]][index2])
  holder2[1:length(holder2)] = init_temps
  holder2 = list(holder2)
  holder3 = unlist(orig_nml[[index3]][index4])
  holder3[1:length(holder3)] = depths
  holder3 = list(holder3)
  orig_nml[[index1]][index2] = holder2
  orig_nml[[index3]][index4] = holder3
  write_nml(orig_nml, paste0(working_directory,'/','glm3.nml'))
  return(list(depths,init_temps))
}

#' Add together two numbers.
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @noRd
#' @examples
#' add(1, 1)
#' add(10, 1)
update_var <- function(var_value,var_name,working_directory, nml){

  orig_nml <- read_nml(paste0(working_directory,'/',nml))
  index1 <- NA; index2 = NA
  for (g in 1:length(orig_nml)) {
    for (q in 1:length(orig_nml[[g]])) {
      if (names(orig_nml[[g]][q]) == var_name) {
        index1 = g; index2 = q;
      }
    }
  }
  holder2 <- unlist(orig_nml[[index1]][index2])
  if(is.character(var_value)){
    split <- strsplit(var_value, ",")
    var_value <- paste0(split[1])
    if(length(split) > 1){
      for(i in 2:length(split)){
        var_value <- paste0(var_value,",",split[i])
      }
    }
  }
  holder2[1:length(var_value)] <- var_value
  holder2 <- list(holder2[1:length(var_value)])
  orig_nml[[index1]][index2] <- holder2
  write_nml(orig_nml, paste0(working_directory,'/',nml))
}

update_nml <- function(var_list,var_name_list,working_directory, nml){
  orig_nml <- read_nml(paste0(working_directory,'/',nml))

  for(k in 1:length(var_list)){
    index1 = NA; index2 = NA
    for (g in 1:length(orig_nml)) {
      for (q in 1:length(orig_nml[[g]])) {
        if (names(orig_nml[[g]][q]) == var_name_list[k]) {
          index1 = g; index2 = q;
        }
      }
    }
    holder2 <- unlist(orig_nml[[index1]][index2])
    if(is.character(var_list[[k]])){
      split <- strsplit(var_list[[k]], ",")
      var_list[[k]] <- paste0(split[1])
      if(length(split) > 1){
        for(i in 2:length(split)){
          var_list[[k]] <- paste0(var_list[[k]],",",split[i])
        }
      }
    }
    holder2 <- unlist(orig_nml[[index1]][index2])
    holder2[1:length(var_list[[k]])] <- var_list[[k]]
    holder2 <- list(holder2[1:length(var_list[[k]])])
    orig_nml[[index1]][index2] <- holder2
  }

  write_nml(orig_nml, paste0(working_directory,'/',nml))
}


#' Add together two numbers.
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @noRd
#' @examples
#' add(1, 1)
#' add(10, 1)
get_glm_nc_var_all_wq <- function(ncFile,working_dir, z_out,vars_depth, vars_no_depth, diagnostic_vars){
  glm_nc <- ncdf4::nc_open(paste0(working_dir, ncFile))
  tallest_layer <- ncdf4::ncvar_get(glm_nc, "NS")
  final_time_step <- length(tallest_layer)
  tallest_layer <- tallest_layer[final_time_step]
  heights <- matrix(ncdf4::ncvar_get(glm_nc, "z"), ncol = final_time_step)
  heights_surf <- heights[tallest_layer, final_time_step]
  heights <- heights[1:tallest_layer, final_time_step]
  heights_out <- heights_surf - z_out

  snow <-  matrix(ncdf4::ncvar_get(glm_nc, "hsnow"), ncol = final_time_step)[final_time_step]
  ice_white <- matrix(ncdf4::ncvar_get(glm_nc, "hwice"), ncol = final_time_step)[final_time_step]
  ice_blue <- matrix(ncdf4::ncvar_get(glm_nc, "hice"), ncol = final_time_step)[final_time_step]
  avg_surf_temp <- matrix(ncdf4::ncvar_get(glm_nc, "avg_surf_temp"), ncol = final_time_step)[final_time_step]


  glm_temps <- matrix(ncdf4::ncvar_get(glm_nc, "temp"), ncol = final_time_step)[1:tallest_layer,final_time_step]

  output <- array(NA,dim=c(tallest_layer,length(vars_depth)))
  for(v in 1:length(vars_depth)){
    var_modeled <-  matrix(ncdf4::ncvar_get(glm_nc, vars_depth[v]), ncol = final_time_step)
    output[,v] <- var_modeled[1:tallest_layer,final_time_step]
  }

  output_no_depth <- NA

  if(length(diagnostic_vars) > 0){
    diagnostics_output <- array(NA,dim=c(tallest_layer,length(diagnostic_vars)))
    for(v in 1:length(diagnostic_vars)){
      var_modeled <- matrix(ncdf4::ncvar_get(glm_nc, diagnostic_vars[v]), ncol = final_time_step)
      diagnostics_output[,v] <- var_modeled[1:tallest_layer,final_time_step]
    }
  }else{
    diagnostics_output <- NA
  }

  mixing_vars <- ncdf4::ncvar_get(glm_nc, "restart_variables")

  salt <- matrix(ncdf4::ncvar_get(glm_nc, "salt"), ncol = final_time_step)[1:tallest_layer, final_time_step]

  ncdf4::nc_close(glm_nc)


  return(list(output = output,
              output_no_depth = output_no_depth,
              lake_depth = heights_surf,
              depths_enkf = rev(heights_surf - heights),
              snow_wice_bice = c(snow, ice_white, ice_blue),
              avg_surf_temp = avg_surf_temp,
              mixing_vars = mixing_vars,
              salt = salt,
              diagnostics_output = diagnostics_output))
}


