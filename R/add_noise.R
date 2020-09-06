# --------------------------------------
# purpose: create ensemble members of downscaling noise
# Creator: Laura Puckett, December 21 2018
# contact: plaura1@vt.edu
# --------------------------------------
# summary: creates ensemble members with noise addition (random sample from normal distribution with standard deviation equal to saved standard deviation of residuals from downscaling process.) For all variables except Shortwave, this is the standard deviaiton of the residuals after downscaling to the hourly resolution. For Shortwave, is is the value after downscaling to the daily resolution to artificually high values that result from noise in observational data from hour to hour.
# --------------------------------------
#' @title Download and Downscale NOAA GEFS for a single site
#' @return None
#'
#' @param site_index, index of site_list, lat_list, lon_list to be downloaded
#' @param lat_list, vector of latitudes that correspond to site codes
#' @param lon_list, vector of longitudes that correspond to site codes
#' @param site_list, vector of site codes, used in directory and file name generation
#' @param downscale, logical specifying whether to downscale from 6-hr to 1-hr
#' @param overwrite, logical stating to overwrite any existing output_file
#' @param model_name, directory name for the 6-hr forecast, this will be used in directory and file name generation
#' @param model_name_ds, directory name for the 1-hr forecast, this will be used in directory and file name generation
#' @param output_directory, directory where the model output will be save
#' @noRd
#'
#' @author Quinn Thomas
#'

add_noise <- function(debiased, cov, n_ds_members, n_met_members, VarNames){

    with.noise <- debiased %>%
      dplyr::group_by_all() %>%
      tidyr::expand(dscale.member = 1:n_ds_members) %>%
      dplyr::mutate(ShortWaveOld = ShortWave,
                    RainOld = Rain) %>%
      dplyr::ungroup()

  # add option for covariance vs non covariance

  for(NOAA.ens in 1:n_met_members){
    for(dscale.ens in 1:n_ds_members){
      noise <- rmvnorm(1, mean = rep(0, length(VarNames)), sigma = cov)
      colnames(noise) <- colnames(cov)
      for(VarNum in 1:length(VarNames)){
        VarName <- VarNames[VarNum]
        with.noise[which(with.noise$NOAA.member == NOAA.ens & with.noise$dscale.member == dscale.ens),VarName] =
          with.noise[which(with.noise$NOAA.member == NOAA.ens & with.noise$dscale.member == dscale.ens),VarName] + noise[,VarName]
      }
    }
  }
  return(with.noise)
}
