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


spline_to_hourly <- function(df,VarNamesStates){
  # --------------------------------------
  # purpose: interpolates debiased forecasts from 6-hourly to hourly
  # Creator: Laura Puckett, December 16 2018
  # --------------------------------------
  # @param: df, a dataframe of debiased 6-hourly forecasts

  interpolate <- function(jday, var){
    result <- splinefun(jday, var, method = "monoH.FC")
    return(result(seq(min(as.numeric(jday)), max(as.numeric(jday)), 1/24)))
  }



  t0 <- min(df$timestamp)
  df <- df %>%
    dplyr::mutate(days_since_t0 = difftime(.$timestamp, t0, units = "days"))

  if("dscale.member" %in% colnames(df)){
    by.ens <- df %>%
      dplyr::group_by(NOAA.member, dscale.member)
  }else{
    by.ens <- df %>%
      dplyr::group_by(NOAA.member)
    }

  interp.df.days <- by.ens %>%
    dplyr::do(days = seq(min(df$days_since_t0), as.numeric(max(df$days_since_t0)), 1/24))
  interp.df <- interp.df.days

  for(Var in 1:length(VarNamesStates)){
    assign(paste0("interp.df.",VarNamesStates[Var]),  dplyr::do(by.ens, var = interpolate(.$days_since_t0,unlist(.[,VarNamesStates[Var]]))) %>% plyr::rename(c("var" = VarNamesStates[Var])))
    if("dscale.member" %in% colnames(df)){
        interp.df <- dplyr::inner_join(interp.df, get(paste0("interp.df.",VarNamesStates[Var])), by = c("NOAA.member", "dscale.member"))
    }else{
      interp.df <- dplyr::inner_join(interp.df, get(paste0("interp.df.",VarNamesStates[Var])), by = c("NOAA.member"))
    }
  }

  # converting from time difference back to timestamp
  interp.df <- interp.df %>%
    tidyr::unnest(c("days", all_of(VarNamesStates))) %>%
    dplyr::mutate(timestamp = lubridate::as_datetime(t0 + days, tz = attributes(t0)$tzone))
  return(interp.df)
}

