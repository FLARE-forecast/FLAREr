#' Convert columns the map states to observations to a single vector
#'
#' @param states_config
#'
#' @return
#' @export
#'
#' @examples

generate_states_to_obs_mapping <- function(states_config, obs_config){

states_to_obs_temp <- cbind(states_config[,which(stringr::str_detect(names(states_config), "states_to_obs_") & !stringr::str_detect(names(states_config), "states_to_obs_mapping"))])
states_to_obs_mapping_temp <- cbind(states_config[,which(stringr::str_detect(names(states_config), "states_to_obs_mapping_"))])

states_to_obs <- list()
states_to_obs_mapping <- list()
for(i in 1:nrow(states_to_obs_temp)){

  names_temp <- states_to_obs_temp[i,which(!is.na(states_to_obs_temp[i,]))]
  mapping_temp <- unlist(states_to_obs_mapping_temp[i,which(!is.na(states_to_obs_mapping_temp[i,]))])
  if(length(names_temp) == 0){
    values1 <- NA
    values2 <- NA
  }else{
    values1 <- rep(NA,length(names_temp))
    for(j in 1:length(names_temp)){
      values1[j] <- which(obs_config$state_names_obs == unlist(names_temp[j]))
    }
    values2 <- c(mapping_temp)
  }
  states_to_obs[[i]] <- values1
  states_to_obs_mapping[[i]] <- values2
}

return(list(states_to_obs =states_to_obs,states_to_obs_mapping = states_to_obs_mapping))
}