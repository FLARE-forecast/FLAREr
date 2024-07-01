#num_out_depths <- length(out[[m]]$model_internal_depths)
#model_internal_depths[i,1:num_out_depths ,m] <- out[[m]]$model_internal_depths
#non_na_depths_index <- 1:num_out_depths
add_process_noise <- function(m, x_heights, model_internal_heights, flare_depths, states_config, include_uncertainty = TRUE, hist_days){

  alpha_v <- 1 - exp(-states_config$vert_decorr_length)

  q_v <- rep(NA, num_out_depths)
  w <- rep(NA, num_out_depths)
  w_new <- rep(NA, num_out_depths)
  #Add process noise

  for(jj in 1:nrow(model_sd)){
    model_sd_depth <- approx(x = config$model_settings$modeled_depths,
                             y = model_sd[jj, ],
                             xout = model_internal_depths[i,1:num_out_depths ,m],
                             rule = 2)$y

    w[] <- rnorm(num_out_depths, 0, 1)
    if(include_uncertainty == FALSE & i > (hist_days + 1)){
      w[] <- 0.0
    }
    for(kk in 1:num_out_depths){
      if(kk == 1){
        w_new[kk] <- w[kk]
      }else{
        alpha <- exp(-states_config$vert_decorr_length[jj] / (model_internal_depths[i,kk]-model_internal_depths[i,kk-1]))
        w_new[kk] <- ((1 - alpha) * w_new[kk-1] +  alpha * w[kk])

        #w_new[kk] <- (alpha_v[jj] * w_new[kk-1] + sqrt(1 - alpha_v[jj]^2) * w[kk])
      }
      q_v[kk] <- w_new[kk] * model_sd_depth[kk]
      x_corr_heights <- glm_native_x[jj, kk] + q_v[kk]
    }
    x_corr_out <- approx(glm_depths_mid,glm_native_x[i, jj, non_na_depths_index, m], config$model_settings$modeled_depths, rule = 2)$y
  }

  return(list(x_corr_depth, x_corr_heights))


}
