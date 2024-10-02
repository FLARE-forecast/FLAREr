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
    if(is.na(index1) | is.na(index2)){
      stop(paste('Base nml missing the following variable name: ',var_name_list[k],
                 '\nSee working example for section where variable is located in nml file'))
    }
    orig_nml[[index1]][index2] <- holder2
  }

  write_nml(orig_nml, paste0(working_directory,'/',nml))
}
