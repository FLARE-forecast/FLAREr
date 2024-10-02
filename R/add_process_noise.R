#' Add random noise to model states
#'
#' @param states_height_ens matrix of states for a particular ensemble member
#' @param model_sd matrix of process noise standard deviation for each state and depth
#' @param model_internal_heights_ens vector of heights predicted by GLM for the ensemble member
#' @param lake_depth_ens depth of lake for ensemble member
#' @param modeled_depths vector of depths modeled using FLARE
#' @param vert_decorr_length vector of vertical decorrelation length for each model state
#' @param include_uncertainty Boolen to include process uncertainty
#'
#' @noRd
#' @return list of updated states with respect to depth and height
#'
add_process_noise <- function(states_height_ens, model_sd, model_internal_heights_ens, lake_depth_ens, modeled_depths, vert_decorr_length, include_uncertainty = TRUE){

  states_depth_ens <- array(NA, dim = c(nrow(model_sd), length(modeled_depths)))

  alpha_v <- 1 - exp(-vert_decorr_length)


  non_na_heights_index <- which(!is.na(model_internal_heights_ens))

  num_out_heights <- length(non_na_heights_index)
  non_na_heights_index <- 1:num_out_heights

  glm_depths <- lake_depth_ens - model_internal_heights_ens[non_na_heights_index]


  q_v <- rep(NA, num_out_heights)
  w <- rep(NA, num_out_heights)
  w_new <- rep(NA, num_out_heights)

  for(jj in 1:nrow(model_sd)){
    model_sd_height <- approx(x = modeled_depths,
                              y = model_sd[jj, ],
                              xout = lake_depth_ens - model_internal_heights_ens[1:num_out_heights],
                              rule = 2)$y

    w[] <- rnorm(num_out_heights, 0, 1)
    if(include_uncertainty == FALSE){
      w[] <- 0.0
    }
    for(kk in 1:num_out_heights){
      if(kk == 1){
        w_new[kk] <- w[kk]
      }else{
        alpha <- exp(-vert_decorr_length[jj] / abs((model_internal_heights_ens[kk]-model_internal_heights_ens[kk-1])))
        w_new[kk] <- ((1 - alpha) * w_new[kk-1] +  alpha * w[kk])
      }
      q_v[kk] <- w_new[kk] * model_sd_height[kk]
      states_height_ens[jj, kk] <- states_height_ens[jj, kk] + q_v[kk]

      if(jj > 1 & states_height_ens[jj, kk] < 0){
        states_height_ens[jj, kk] <- 0.0
      }
    }

    states_depth_ens[jj, ] <- approx(glm_depths, states_height_ens[jj, non_na_heights_index], modeled_depths, rule = 2)$y

  }

  return(list(states_height_ens = states_height_ens, states_depth_ens = states_depth_ens))


}
