#' Create filenames used in netcdf output
#'
#' @param full_time vector of datestimes that are modeled
#' @param hist_days number of days simulated before a forecast begins
#'
#' @noRd
#' @return list of filenames used in the netcdf generation
create_filenames <- function(full_time, hist_days, forecast_days, config){


  if(lubridate::day(full_time[1]) < 10){
    file_name_H_day <- paste0("0",lubridate::day(full_time[1]))
  }else{
    file_name_H_day <- lubridate::day(full_time[1])
  }
  if(lubridate::day(full_time[hist_days+1]) < 10){
    file_name_H_end_day <- paste0("0",lubridate::day(full_time[hist_days+1]))
  }else{
    file_name_H_end_day <- lubridate::day(full_time[hist_days+1])
  }
  if(lubridate::month(full_time[1]) < 10){
    file_name_H_month <- paste0("0",lubridate::month(full_time[1]))
  }else{
    file_name_H_month <- lubridate::month(full_time[1])
  }
  if(lubridate::month(full_time[hist_days+1]) < 10){
    file_name_H_end_month <- paste0("0",lubridate::month(full_time[hist_days+1]))
  }else{
    file_name_H_end_month <- lubridate::month(full_time[hist_days+1])
  }

  time_of_forecast <- Sys.time()
  curr_day <- lubridate::day(time_of_forecast)
  curr_month <- lubridate::month(time_of_forecast)
  curr_year <- lubridate::year(time_of_forecast)
  curr_hour <- lubridate::hour(time_of_forecast)
  curr_minute <- lubridate::minute(time_of_forecast)
  curr_second <- round(lubridate::second(time_of_forecast),0)
  if(curr_day < 10){curr_day <- paste0("0",curr_day)}
  if(curr_month < 10){curr_month <- paste0("0",curr_month)}
  if(curr_hour < 10){curr_hour <- paste0("0",curr_hour)}
  if(curr_minute < 10){curr_minute <- paste0("0",curr_minute)}
  if(curr_second < 10){curr_second <- paste0("0",curr_second)}

  forecast_iteration_id <- paste0(curr_year,
                                  curr_month,
                                  curr_day,
                                  "T",
                                  curr_hour,
                                  curr_minute,
                                  curr_second)

  save_file_name <- paste0(config$run_config$sim_name, "_H_",
                           (lubridate::year(full_time[1])),"_",
                           file_name_H_month,"_",
                           file_name_H_day,"_",
                           (lubridate::year(full_time[hist_days+1])),"_",
                           file_name_H_end_month,"_",
                           file_name_H_end_day,"_F_",
                           forecast_days,"_",
                           forecast_iteration_id)



  if(lubridate::day(full_time[hist_days+1]) < 10){
    file_name_F_day <- paste0("0",lubridate::day(full_time[hist_days+1]))
  }else{
    file_name_F_day <- lubridate::day(full_time[hist_days+1])
  }
  if(lubridate::month(full_time[hist_days+1]) < 10){
    file_name_F_month <- paste0("0",lubridate::month(full_time[hist_days+1]))
  }else{
    file_name_F_month <- lubridate::month(full_time[hist_days+1])
  }

  if(length(full_time) >= hist_days+1){
    save_file_name_short <- paste0(config$location$site_id, "-",
                                   (lubridate::year(full_time[hist_days+1])),"-",
                                   file_name_F_month,"-",
                                   file_name_F_day,"-",
                                   config$run_config$sim_name)
  }else{
    save_file_name_short <- paste0(config$location$site_id, "-",
                                   (lubridate::year(full_time[hist_days+1])),"-",
                                   file_name_F_month,"-",
                                   file_name_F_day,"-",
                                   paste0(config$run_config$sim_name,"_spinup"))
  }

  return(list(save_file_name = save_file_name,
              save_file_name_short = save_file_name_short,
              forecast_iteration_id = forecast_iteration_id,
              time_of_forecast = time_of_forecast))
}
