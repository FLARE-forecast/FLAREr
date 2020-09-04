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


check_CI <- function (df, obs.col.name, for.col.name){
  # df should be joined df after downscaling (with noise)
  # obs.col.name and for.col.name are the column names of the data to compare
  quant.df <- df %>% dplyr::group_by(timestamp) %>%
    dplyr::summarize(quant.0 = quantile(get(for.col.name), probs = 0.0, na.rm = TRUE),
                     quant.025 = quantile(get(for.col.name), probs = 0.025, na.rm = TRUE),
                     quant.05 = quantile(get(for.col.name), probs = 0.05, na.rm = TRUE),
                     quant.50 = quantile(get(for.col.name), probs = 0.50, na.rm = TRUE),
                     quant.95 = quantile(get(for.col.name), probs = 0.95, na.rm = TRUE),
                     quant.975 = quantile(get(for.col.name), probs = 0.975, na.rm = TRUE),
                     quant.100 = quantile(get(for.col.name), probs = 1, na.rm = TRUE), .group = "drop") %>%
    dplyr::ungroup()

  pcnt.in.CI <- dplyr::inner_join(quant.df, df, by = "timestamp") %>%
    dplyr::mutate(check.100 = ifelse(get(obs.col.name) >= quant.0 & get(obs.col.name) <= quant.100,1,0),
           check.95 = ifelse(get(obs.col.name) >= quant.025 & get(obs.col.name) <= quant.975,1,0),
           check.90 = ifelse(get(obs.col.name) >= quant.05 & get(obs.col.name) <= quant.95,1,0)) %>%
    dplyr::summarise(check.100.pcnt = mean(check.100, na.rm = TRUE)*100,
                     check.95.pcnt = mean(check.95, na.rm = TRUE)*100,
                     check.90.pcnt = mean(check.90, na.rm = TRUE)*100, .group = "drop")
  return(pcnt.in.CI )
}
