
if (!"mvtnorm" %in% installed.packages()) install.packages("mvtnorm")
if (!"ncdf4" %in% installed.packages()) install.packages("ncdf4")
if (!"lubridate" %in% installed.packages()) install.packages("lubridate")
if (!"testit" %in% installed.packages()) install.packages("testit")
if (!"imputeTS" %in% installed.packages()) install.packages("imputeTS")
if (!"tidyverse" %in% installed.packages()) install.packages("tidyverse")
if (!"rMR" %in% installed.packages()) install.packages("rMR")

library(mvtnorm)
library(ncdf4)
library(lubridate)
library(testit)
library(imputeTS)
library(tidyverse)
library(tools)
library(rMR)

data_location <<- "/Users/quinn/Dropbox/Research/SSC_forecasting/flare_training/flare_testing_files/SCC_data/"
code_folder <<- "/Users/quinn/Dropbox/Research/SSC_forecasting/flare_training/flare_testing_files/FLARE/"
forecast_location <<- "/Users/quinn/Dropbox/Research/SSC_forecasting/flare_training/flare_testing_files/flare_simulation_test/"
execute_location <<- forecast_location

source(paste0(forecast_location,"/","configure_FLARE.R"))
source(paste0(code_folder, "/", "Rscripts/run_flare.R"))
source(paste0(code_folder, "/", "Rscripts/visualization_analysis/plot_forecast.R"))

restart_file <- NA

sim_name <- "test" 
forecast_days <- 16
spin_up_days <- 0

start_day_local <- "2018-07-12"
start_time_local <- "07:00:00"
forecast_start_day_local <- "2018-07-15" 

start_day_local <- as.POSIXct(start_day_local, format = "%Y-%m-%d")
forecast_start_day_local <- as.POSIXct(forecast_start_day_local, format = "%Y-%m-%d")
hist_days <- as.numeric(difftime(as_date(forecast_start_day_local),as_date(start_day_local)))

forecast_sss_on <- FALSE

out <- run_flare(start_day_local,
                 start_time_local,
                 forecast_start_day_local,
                 sim_name = sim_name, 
                 hist_days = hist_days,
                 forecast_days = forecast_days,  
                 spin_up_days = spin_up_days,
                 restart_file = restart_file,
                 uncert_mode = uncert_mode,
                 forecast_sss_on = forecast_sss_on)


plot_forecast(pdf_file_name = unlist(out)[2],
              output_file = unlist(out)[1],
              include_wq = include_wq,
              forecast_days = forecast_days,
              code_folder = code_folder,
              save_location = forecast_location,
              data_location = data_location,
              plot_summaries = TRUE,
              push_to_git = push_to_git,
              pull_from_git = pull_from_git,
              modeled_depths = modeled_depths)