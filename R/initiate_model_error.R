
#' Initiate model error from configuration files
#'
#' @param config list from reading in configuration yaml
#' @param states_config list from reading in states configuration csv
#'
#' @return
#' @export
#'
#' @examples
initiate_model_error <- function(config, states_config, forecast_location){
  if(!is.null(config$states_config_process_error)){
    model_sd <- array(NA, dim = c(nrow(states_config),length(config$modeled_depths)))
    config_process_error <- readr::read_csv(file.path(forecast_location, "configuration_files", config$states_config_process_error), col_type = readr::cols())
    for(s in 1:nrow(states_config)){
      if(states_config$state_names[s] %in% names(config_process_error)){
        model_sd[s, ] <- approx(x = config_process_error$depth, y = unlist(config_process_error[states_config$state_names[s]]), xout = config$modeled_depths)$y
      }else{
        model_sd[s, ] <- states_config$model_sd[s]
      }
    }
  }else{
    model_sd <- array(NA, dim = c(nrow(states_config),length(config$modeled_depths)))
    for(s in 1:nrow(states_config)){
      model_sd[s, ] <- states_config$model_sd[s]
    }
  }

  return(model_sd)
}
