#' @title Download and Downscale NOAA GEFS for a single site
#' @return None
#'
#' @param site_index, index of site_list, lat_list, lon_list to be downloaded
#' @param lat_list, vector of latitudes that correspond to site codes
#' @param lon_list, vector of longitudes that correspond to site codes
#' @param site_list, vector of site codes, used in directory and file name generation
#' @param downscale, logical specifying whether to downscale from 6-hr to 1-hr
#' @param overwrite, logical stating to overwrite any existing output_file
#' @param model_name, directory name for the 6-hr forecast, this will be used in directory and file name generation
#' @param model_name_ds, directory name for the 1-hr forecast, this will be used in directory and file name generation
#' @param output_directory, directory where the model output will be save
#' @noRd
#'
#' @author Quinn Thomas
#'
#'

localization <- function(mat,nstates,modeled_depths, localization_distance){

  distance_matrix <- matrix(NA,nrow = nstates, ncol = nstates)

  for(i in 1:dim(distance_matrix)[1]){
    index <- 1
    for(j in 1:dim(distance_matrix)[2]){
      distance_matrix[i,j] <- modeled_depths[index]
      index <- index + 1
      if(index > length(modeled_depths)){
        index <- 1
      }
    }
  }

  distance_differ_matrix <- distance_matrix - diag(distance_matrix)

  for(i in 1:dim(distance_matrix)[1]){
  distance_differ_matrix[i, ] <- exp(-(distance_differ_matrix[i, ]/localization_distance)^2)
  }


  mat_new <- mat * distance_differ_matrix


  return(mat_new)

}
