#' Apply localization to a matrix based on distance
#'
#' @param mat matrix that the location is applied to
#' @param nstates number of states
#' @param modeled_depths vector of depths that are modeled
#' @param localization_distance distance scalar for the strenght of the locational
#' @param num_single_states number of states that to do not have values for each depth (e.g. secchi)
#'
#' @return matrix with localization applied
#' @noRd
localization <- function(mat,nstates,modeled_depths, localization_distance, num_single_states = 0){

  distance_matrix <- matrix(NA, nrow = nstates* length(modeled_depths), ncol = nstates*length(modeled_depths))

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
  if(num_single_states > 0){
    for(i in 1:num_single_states){
      new_col <- rep(1, nrow(distance_differ_matrix))
      distance_differ_matrix <- cbind(distance_differ_matrix, new_col)
      new_row <- rep(1, ncol(distance_differ_matrix))
      distance_differ_matrix <- rbind(distance_differ_matrix, new_row)
    }
  }

  mat_new <- mat * distance_differ_matrix

  return(mat_new)

}
