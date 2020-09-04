##' @title Download and Downscale NOAA GEFS for a single site
##' @return None
##'
##' @param site_index, index of site_list, lat_list, lon_list to be downloaded
##' @param lat_list, vector of latitudes that correspond to site codes
##' @param lon_list, vector of longitudes that correspond to site codes
##' @param site_list, vector of site codes, used in directory and file name generation
##' @param downscale, logical specifying whether to downscale from 6-hr to 1-hr
##' @param overwrite, logical stating to overwrite any existing output_file
##' @param model_name, directory name for the 6-hr forecast, this will be used in directory and file name generation
##' @param model_name_ds, directory name for the 1-hr forecast, this will be used in directory and file name generation
##' @param output_directory, directory where the model output will be save
##' @export
##'
##' @author Quinn Thomas
##'

daily_debias_from_coeff <- function(daily.forecast,
                                    coeff.df,
                                    VarInfo){
  # --------------------------------------
  # purpose: does linear debiasing from previously calculated coefficients
  # Creator: Laura Puckett, December 14 2018
  # --------------------------------------
  # @param: daily.forecast, dataframe of past forecasts at daily resolution
  # @param: coeff.df, the save coefficients for linear debaiasing for each meterological variable at daily resolution
  if("fday.group" %in% colnames(daily.forecast)){
    grouping <- c("fday.group", "NOAA.member")
  }else{
    grouping <- c("date", "NOAA.member")
  }
  lin_mod <- function(col.for, coeff){
    intercept <- coeff[1]
    slope <- coeff[2]
    modeled <- col.for * slope + intercept
    return(modeled)
  }

  debiased <- daily.forecast %>%
    dplyr::select(all_of(grouping))
  for(Var in 1:length(VarInfo$VarNames)){
    VarName <- VarInfo$VarNames[Var]
    assign(VarName, value = tibble::as_tibble(lin_mod(daily.forecast[,VarName],
                                                      coeff.df[,VarName])))
    debiased <- cbind(debiased, get(VarName))
  }

  return(debiased)
}

