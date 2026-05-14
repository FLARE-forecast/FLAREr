#' Apply in-memory updates to a parsed GLM namelist
#'
#' @param nml parsed nml list (from `read_nml`)
#' @param var_list list of values, parallel to `var_name_list`
#' @param var_name_list character vector of variable names to update
#'
#' @return the modified nml list
#' @noRd
modify_nml <- function(nml, var_list, var_name_list) {
  for (k in seq_along(var_list)) {
    index1 <- NA; index2 <- NA
    for (g in seq_along(nml)) {
      for (q in seq_along(nml[[g]])) {
        if (names(nml[[g]][q]) == var_name_list[k]) {
          index1 <- g; index2 <- q
        }
      }
    }
    if (is.na(index1) | is.na(index2)) {
      stop(paste(
        "Base nml missing the following variable name:", var_name_list[k],
        "\nSee working example for section where variable is located in nml file"
      ))
    }
    holder2 <- unlist(nml[[index1]][index2])
    if (is.character(var_list[[k]])) {
      split <- strsplit(var_list[[k]], ",")
      var_list[[k]] <- paste0(split[1])
      if (length(split) > 1) {
        for (i in 2:length(split)) {
          var_list[[k]] <- paste0(var_list[[k]], ",", split[i])
        }
      }
    }
    holder2[1:length(var_list[[k]])] <- var_list[[k]]
    nml[[index1]][index2] <- list(holder2[1:length(var_list[[k]])])
  }
  nml
}

#' Update a single variable in a GLM namelist file
#'
#' @param var_value value to write for the variable
#' @param var_name character name of the namelist variable to update
#' @param working_directory path to the directory containing the nml file
#' @param nml filename of the namelist file (e.g. "glm3.nml")
#'
#' @return invisibly, the updated nml written to disk
#' @noRd
update_var <- function(var_value, var_name, working_directory, nml) {

  orig_nml <- read_nml(paste0(working_directory, "/", nml))
  index1 <- NA; index2 <- NA
  for (g in 1:length(orig_nml)) {
    for (q in 1:length(orig_nml[[g]])) {
      if (names(orig_nml[[g]][q]) == var_name) {
        index1 <- g; index2 <- q
      }
    }
  }
  holder2 <- unlist(orig_nml[[index1]][index2])
  if (is.character(var_value)) {
    split <- strsplit(var_value, ",")
    var_value <- paste0(split[1])
    if (length(split) > 1) {
      for (i in 2:length(split)) {
        var_value <- paste0(var_value, ",", split[i])
      }
    }
  }
  holder2[1:length(var_value)] <- var_value
  holder2 <- list(holder2[1:length(var_value)])
  orig_nml[[index1]][index2] <- holder2
  write_nml(orig_nml, paste0(working_directory, "/", nml))
}

#' Update multiple variables in a GLM namelist file
#'
#' @param var_list list of values to write, parallel to `var_name_list`
#' @param var_name_list character vector of namelist variable names to update
#' @param working_directory path to the directory containing the nml file
#' @param nml filename of the namelist file (e.g. "glm3.nml")
#'
#' @return invisibly, the updated nml written to disk
#' @noRd
update_nml <- function(var_list, var_name_list, working_directory, nml) {
  orig_nml <- read_nml(paste0(working_directory, "/", nml))
  orig_nml <- modify_nml(orig_nml, var_list, var_name_list)
  write_nml(orig_nml, paste0(working_directory, "/", nml))
}
