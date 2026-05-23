#' Apply localization to a matrix based on distance
#'
#' @param mat matrix that the location is applied to
#' @param nstates number of states
#' @param modeled_depths vector of depths that are modeled
#' @param localization_distance distance scalar for the strength of the
#'   localization
#' @param num_single_states number of states that do not have values for each
#'   depth (e.g. secchi)
#'
#' @return matrix with localization applied
#' @noRd
localization <- function(mat, nstates, modeled_depths, localization_distance,
                         num_single_states = 0) {

  # Each row of the distance matrix cycles through modeled_depths repeatedly
  distance_row <- rep(modeled_depths, nstates)
  distance_matrix <- matrix(
    distance_row,
    nrow = length(distance_row),
    ncol = length(distance_row),
    byrow = TRUE
  )

  distance_differ_matrix <- abs(distance_matrix - diag(distance_matrix))

  # Apply Gaussian decay element-wise (no row loop needed)
  distance_differ_matrix <- exp(
    (-distance_differ_matrix^2) / (2 * localization_distance^2)
  )

  if (num_single_states > 0) {
    for (i in seq_len(num_single_states)) {
      new_col <- rep(1, nrow(distance_differ_matrix))
      distance_differ_matrix <- cbind(distance_differ_matrix, new_col)
      new_row <- rep(1, ncol(distance_differ_matrix))
      distance_differ_matrix <- rbind(distance_differ_matrix, new_row)
    }
  }

  mat * distance_differ_matrix
}
