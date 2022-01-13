#' @title Generate model error matrix from configuration files
#' @details Function uses the configuration files to generate the matrix that run_da_forecast use to add normally distributed random noise to model predictions
#' @param config list; list from reading in configuration yaml
#' @param states_config list; list from reading in states configuration csv
#' @return matrix
#' @export
#' @importFrom readr read_csv cols
##' @examples
##' \dontrun{
##' model_sd <- initiate_model_error(config, states_config)
##' }
initiate_model_error <- function(config, states_config){
  if(!is.null(config$model_settings$depth_model_sd_config_file)){
    model_sd <- array(NA, dim = c(nrow(states_config),length(config$model_settings$modeled_depths)))
    config_process_error <- readr::read_csv(file.path(config$file_path$configuration_directory,
                                                      config$model_settings$depth_model_sd_config_file), col_type = readr::cols())
    for(s in 1:nrow(states_config)){
      if(states_config$state_names[s] %in% names(config_process_error)){
        model_sd[s, ] <- approx(x = config_process_error$depth, y = unlist(config_process_error[states_config$state_names[s]]), xout = config$model_settings$modeled_depths)$y
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
