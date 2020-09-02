if (!"tidyverse" %in% installed.packages()) install.packages("tidyverse")
if (!"mvtnorm" %in% installed.packages()) install.packages("mvtnorm")
if (!"ncdf4" %in% installed.packages()) install.packages("ncdf4")
if (!"lubridate" %in% installed.packages()) install.packages("lubridate")
if (!"testit" %in% installed.packages()) install.packages("testit")
if (!"imputeTS" %in% installed.packages()) install.packages("imputeTS")
if (!"rMR" %in% installed.packages()) install.packages("rMR")

library(tidyverse)
library(mvtnorm)
library(ncdf4)
library(lubridate)
library(testit)
library(imputeTS)
library(tools)
library(rMR)

#data_location <<- "/Users/quinn/Dropbox/Research/SSC_forecasting/SCC_data/"
#code_folder <<- "/Users/quinn/Dropbox/Research/SSC_forecasting/FLARE/"
#forecast_location <<- "/Users/quinn/Dropbox/Research/SSC_forecasting/test_forecast_launch/"
#execute_location <<- "/Volumes/ramdisk1/"

data_location <<- "/data/SCCData"
code_folder <<- "~/applications/forecast/FLARE"
forecast_location <<- "~/applications/forecast/FCR_forecasts/v2.glm"
execute_location <<- "/data/forecast_exec"

source(paste0(forecast_location,"/","configure_FLARE.R"))
source(paste0(code_folder, "/", "Rscripts/run_flare.R"))
source(paste0(code_folder, "/", "Rscripts/visualization_analysis/plot_forecast.R"))
source(paste0(code_folder, "/", "Rscripts/visualization_analysis/combined_oxygen_plot.R"))

start_day_local <- "2019-09-20" 
restart_file <- NA

sim_name <- "v2.glm"
start_time_local <- "07:00:00"
forecast_start_day_local <- "2020-03-09"
spin_up_days <- 0
days_between_forecasts <- 1
forecast_days <- 16
num_forecast_periods <- 1000
wait_time <- 60*60

start_day_local <- as_date(start_day_local)
forecast_start_day_local <- as_date(forecast_start_day_local)

if(!file.exists(paste0(forecast_location,"/last_success.Rdata"))){
  
  hist_days <- as.numeric(difftime(as_date(forecast_start_day_local),as_date(start_day_local)))
  
  forecast_days <- 0
  
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
                use_ctd = use_ctd,
                modeled_depths = modeled_depths)
  
  #ADVANCE TO NEXT DAY
  start_day_local <- as_date(start_day_local) + days(hist_days)
  restart_file <- unlist(out)[1]
  save(restart_file,start_day_local,file = paste0(forecast_location,"/last_success.Rdata"))
}

forecast_day_count <- 1
#ALL SUBSEQUENT DAYS
if(num_forecast_periods > 0){
  repeat{
    
    load(paste0(forecast_location,"/last_success.Rdata"))
    
    
    startTime <- Sys.time()
    if(forecast_day_count == 1){
      hist_days <- 1
    }else{
      hist_days <- days_between_forecasts
    }
    
    if(use_future_met){
      #LOOP TO KEEP CHECKING FOR A NOAA FORECAST
      forecast_avialable = FALSE
      while(forecast_avialable == FALSE){
        
        forecast_start_day <- as_date(start_day_local)  + days(hist_days)
        forecast_start_time_local <- ymd_hms(paste0(start_day_local,"T",start_time_local), 
                                             tz = local_tzone) + days(hist_days)
        
        forecast_start_time_GMT <- with_tz(forecast_start_time_local, 
                                           tzone = "GMT")
        
        if(day(forecast_start_time_GMT) < 10){
          forecast_day <- paste0('0',day(forecast_start_time_GMT))
        }else{
          forecast_day <- paste0(day(forecast_start_time_GMT))
        }
        if(month(forecast_start_time_GMT) < 10){
          forecast_month <- paste0('0',month(forecast_start_time_GMT))
        }else{
          forecast_month <- paste0(month(forecast_start_time_GMT))
        }
        if(hour(forecast_start_time_GMT) == 0){
          noaa_hour <- "00"
        }
        if(hour(forecast_start_time_GMT) == 6){
          noaa_hour <- "06"
        }
        if(hour(forecast_start_time_GMT) == 12){
          noaa_hour <- "12"
        }
        if(hour(forecast_start_time_GMT) == 18){
          noaa_hour <- "18"
        }
        
        forecast_base_name <- paste0(lake_name,"_",
                                     year(forecast_start_time_GMT),
                                     forecast_month,
                                     forecast_day,"_",
                                     "gep_all_",
                                     noaa_hour,
                                     "z.csv")
        
        setwd(noaa_location)
        system(paste0('git pull'))
        
        if(!file.exists(paste0(noaa_location,'/',forecast_base_name))){
          print('Waiting for NOAA forecast')
          Sys.sleep(wait_time)
        }else{
          forecast_avialable = TRUE
        }
      }
    }
    
    spin_up_days <- 0
    
    out1 <- run_flare(start_day_local,
                      start_time_local,
                      forecast_start_day_local,
                      sim_name = sim_name, 
                      hist_days = hist_days,
                      forecast_days = forecast_days,  
                      spin_up_days = spin_up_days,
                      restart_file = restart_file,
                      uncert_mode = uncert_mode,
                      forecast_sss_on = forecast_sss_on)
    
    plot_forecast(pdf_file_name = unlist(out1)[2],
                  output_file = unlist(out1)[1],
                  include_wq = include_wq,
                  forecast_days = forecast_days,
                  code_folder = code_folder,
                  save_location = forecast_location,
                  data_location = data_location,
                  plot_summaries = TRUE,
                  push_to_git = push_to_git,
                  pull_from_git = pull_from_git,
                  modeled_depths = modeled_depths)
    
    restart_file <- unlist(out1)[1]
    
    
    
    #ADVANCE TO NEXT DAY
    start_day_local <- start_day_local + days(hist_days)
    forecast_day_count <- forecast_day_count + 1
    
    save(restart_file, 
         start_day_local, 
         forecast_day_count, 
         file = paste0(forecast_location,"/last_success.Rdata"))
    
    if(!is.na(num_forecast_periods)){
      if(forecast_day_count > num_forecast_periods){
        break
      }
    }
  }
}
