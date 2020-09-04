met_qaqc <- function(realtime_file,
                     qaqc_file,
                     cleaned_met_file,
                     input_file_tz,
                     local_tzone){

  if(!is.na(qaqc_file)){
    d1 <- read_csv(realtime_file, skip = 3, guess_max = 100000, col_types = readr::cols())
    d_names <- read_csv(realtime_file, skip = 1, n_max = 3, col_types = readr::cols())
    names(d1) <- names(d_names)

    #d1 <- d1[-85572, ]

    TIMESTAMP_in <- force_tz(d1$TIMESTAMP, tzone = input_file_tz)

    d1$TIMESTAMP <- with_tz(TIMESTAMP_in,tz = local_tzone)

    d2 <- read_csv(qaqc_file, guess_max = 100000, col_types = readr::cols())

    TIMESTAMP_in <- force_tz(d2$DateTime, tzone = input_file_tz)

    d2$TIMESTAMP <- with_tz(TIMESTAMP_in,tz = local_tzone)

    #d3 <- read.csv( fname[3])
    #TIMESTAMP_in <- as.POSIXct(d3$time,
    #                           format= "%Y-%m-%d %H:%M",
    #                           tz = input_file_tz)


    #d3$TIMESTAMP <- with_tz(TIMESTAMP_in,tz = local_tzone)

    d1 <- data.frame(time = d1$TIMESTAMP, ShortWave = d1$SR01Up_Avg, LongWave = d1$IR01UpCo_Avg, AirTemp = d1$AirTC_Avg, RelHum = d1$RH, WindSpeed = d1$WS_ms_Avg, Rain = d1$Rain_mm_Tot)
    d2 <- data.frame(time = d2$TIMESTAMP, ShortWave = d2$ShortwaveRadiationUp_Average_W_m2, LongWave = d2$InfaredRadiationUp_Average_W_m2, AirTemp = d2$AirTemp_Average_C, RelHum = d2$RH_percent, WindSpeed = d2$WindSpeed_Average_m_s, Rain = d2$Rain_Total_mm)

    d1 <- d1[which(d1$time > d2$time[nrow(d2)] | d1$time < d2$time[1]), ]

    #d3 <- d3[which(d3$TIMESTAMP < d2$TIMESTAMP[1])]

    d <- rbind(d2, d1)

  }else{

    d1 <- read.csv(realtime_file, skip = 3)
    d_names <- read.csv(realtime_file, skip = 1)
    names(d1) <- names(d_names)

    #d1 <- d1[-85572, ]

    TIMESTAMP_in <- as.POSIXct(d1$TIMESTAMP,
                               format= "%Y-%m-%d %H:%M",
                               tz = input_file_tz)

    d1$TIMESTAMP <- with_tz(TIMESTAMP_in,tz = local_tzone)

    d <- data.frame(time = d1$TIMESTAMP, ShortWave = d1$SR01Up_Avg, LongWave = d1$IR01UpCo_Avg, AirTemp = d1$AirTC_Avg, RelHum = d1$RH, WindSpeed = d1$WS_ms_Avg, Rain = d1$Rain_mm_Tot)
  }


  wshgt <- 3
  roughlength <- 0.000114
  d$WindSpeed <- d$WindSpeed * log(10.00 / 0.000114) / log(wshgt / 0.000114)

  maxTempC = 41 # an upper bound of realistic temperature for the study site in deg C
  minTempC = -24 # an lower bound of realistic temperature for the study site in deg C

  d <- d %>%
    dplyr::mutate(ShortWave = ifelse(ShortWave < 0, 0, ShortWave),
                  RelHum = ifelse(RelHum < 0, 0, RelHum),
                  RelHum = ifelse(RelHum > 100, 100, RelHum),
                  AirTemp = ifelse(AirTemp> maxTempC, NA, AirTemp),
                  AirTemp = ifelse(AirTemp < minTempC, NA, AirTemp),
                  LongWave = ifelse(LongWave < 0, NA, LongWave),
                  WindSpeed = ifelse(WindSpeed < 0, 0, WindSpeed)) %>%
    filter(is.na(time) == FALSE)

  d <- d %>%
    mutate(day = day(time),
           year = year(time),
           hour = hour(time),
           month = month(time)) %>%
    group_by(day, year, hour, month) %>%
    summarize(ShortWave = mean(ShortWave, na.rm = TRUE),
              LongWave = mean(LongWave, na.rm = TRUE),
              AirTemp = mean(AirTemp, na.rm = TRUE),
              RelHum = mean(RelHum, na.rm = TRUE),
              WindSpeed = mean(WindSpeed, na.rm = TRUE),
              Rain = sum(Rain), .groups = "drop") %>%
    ungroup() %>%
    mutate(day = as.numeric(day),
           hour = as.numeric(hour)) %>%
    mutate(day = ifelse(as.numeric(day) < 10, paste0("0",day),day),
           hour = ifelse(as.numeric(hour) < 10, paste0("0",hour),hour)) %>%
    mutate(time = as_datetime(paste0(year,"-",month,"-",day," ",hour,":00:00"),tz = local_tzone)) %>%
    dplyr::select(time,ShortWave,LongWave,AirTemp,RelHum,WindSpeed,Rain) %>%
    arrange(time)

  colnames(d) <- noquote(c("time",
                                        "ShortWave",
                                        "LongWave",
                                        "AirTemp",
                                        "RelHum",
                                        "WindSpeed",
                                        "Rain"))

  write.csv(d, cleaned_met_file, row.names = FALSE, quote = FALSE)
}
