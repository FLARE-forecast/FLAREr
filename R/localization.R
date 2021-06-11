#' Apply localization to a matrix based on distance
#'
#' @param mat matrix that the location is applied to
#' @param nstates number of states
#' @param modeled_depths vector of depths that are modeled
#' @param localization_distance distance scalar for the strenght of the locational
#'
#' @return matrix with localization applied
#' @noRd
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
