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


plot_downscaled_met <- function(met_file_names, VarNames, working_directory){

  full.data <- NULL

  for(i in 2:length(met_file_names)){
    tmp.data <- read.csv(met_file_names[i]) %>%
      dplyr::mutate(ens = i - 1)
    full.data <- rbind(full.data, tmp.data)
  }
  pdf(paste0(working_directory, "/downscaled_met_plots.pdf"))
  for(i in 1:length(VarNames)){
   print(ggplot2::ggplot(data = full.data, aes(x = time)) +
           ggplot2::geom_line(aes(y = get(paste(VarNames[i])), color = "Downscaled", group = ens), alpha = 0.3) +
           ggplot2::ylab(paste(VarNames[i]))+
           ggplot2::xlab("time")+
           ggplot2::theme_linedraw() +
           ggplot2::theme(text = element_text(size = 14)) +
           ggplot2::scale_color_manual(values = c("firebrick2","black")))
  }
  dev.off()
}

