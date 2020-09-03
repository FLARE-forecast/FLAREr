config <- yaml::read_yaml("/Users/quinn/Dropbox/Research/SSC_forecasting/FLARE_package/flare/notebook/configure_flare.yml")

if(!file.exists(file.path(config$data_location, config$realtime_insitu_location))){
    stop("Missing temperature data GitHub repo")
}
if(!file.exists(file.path(config$data_location, config$realtime_met_station_location))){
  stop("Missing met station data GitHub repo")
}
if(!file.exists(file.path(config$data_location, config$noaa_location))){
  stop("Missing NOAA forecast GitHub repo")
}
if(!file.exists(file.path(config$data_location, config$manual_data_location))){
  stop("Missing Manual data GitHub repo")
}

if(!file.exists(file.path(config$data_location, config$realtime_inflow_data_location))){
  stop("Missing Inflow data GitHub repo")
}

setwd(file.path(config$data_location, config$realtime_insitu_location))
system(paste0("git pull"))

setwd(file.path(config$data_location, config$realtime_met_station_location))
system(paste0("git pull"))

setwd(file.path(config$data_location, config$noaa_location))
system(paste0("git pull"))

setwd(file.path(config$data_location, config$manual_data_location))
system(paste0("git pull"))

setwd(file.path(config$data_location, config$realtime_inflow_data_location))
system(paste0("git pull"))
