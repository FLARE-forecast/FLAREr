temp_oxy_chla_qaqc <- function(realtime_file,
                               qaqc_file,
                               maintenance_file,
                               input_file_tz,
                               focal_depths,
                               local_tzone){

  CATDATA_COL_NAMES <- c("DateTime", "RECORD", "CR6_Batt_V", "CR6Panel_Temp_C", "ThermistorTemp_C_surface",
                         "ThermistorTemp_C_1", "ThermistorTemp_C_2", "ThermistorTemp_C_3", "ThermistorTemp_C_4",
                         "ThermistorTemp_C_5", "ThermistorTemp_C_6", "ThermistorTemp_C_7", "ThermistorTemp_C_8",
                         "ThermistorTemp_C_9", "RDO_mgL_5", "RDOsat_percent_5", "RDOTemp_C_5", "RDO_mgL_9",
                         "RDOsat_percent_9", "RDOTemp_C_9", "EXO_Date", "EXO_Time", "EXOTemp_C_1", "EXOCond_uScm_1",
                         "EXOSpCond_uScm_1", "EXOTDS_mgL_1", "EXODOsat_percent_1", "EXODO_mgL_1", "EXOChla_RFU_1",
                         "EXOChla_ugL_1", "EXOBGAPC_RFU_1", "EXOBGAPC_ugL_1", "EXOfDOM_RFU_1", "EXOfDOM_QSU_1",
                         "EXO_pressure", "EXO_depth", "EXO_battery", "EXO_cablepower", "EXO_wiper")

  # after maintenance, DO values will continue to be replaced by NA until DO_mgL returns within this threshold (in mg/L)
  # of the pre-maintenance value
  DO_RECOVERY_THRESHOLD <- 1

  # columns where certain values are stored
  DO_MGL_COLS <- c(28, 15, 18)
  DO_SAT_COLS <- c(27, 16, 19)
  DO_FLAG_COLS <- c(41, 42, 43)

  # depths at which DO is measured
  DO_DEPTHS <- c(1, 5, 9)

  # EXO sonde sensor data that differs from the mean by more than the standard deviation multiplied by this factor will
  # either be replaced with NA and flagged (if between 2018-10-01 and 2019-03-01) or just flagged (otherwise)
  EXO_FOULING_FACTOR <- 4



  # read catwalk data and maintenance log
  # NOTE: date-times throughout this script are processed as UTC
  catdata <- readr::read_csv(realtime_file, skip = 4, col_names = CATDATA_COL_NAMES,
                      col_types = readr::cols(.default = readr::col_double(), DateTime = readr::col_datetime()))

  log <- readr::read_csv(maintenance_file, col_types = readr::cols(
    .default = readr::col_character(),
    TIMESTAMP_start = readr::col_datetime("%Y-%m-%d %H:%M:%S%*"),
    TIMESTAMP_end = readr::col_datetime("%Y-%m-%d %H:%M:%S%*"),
    flag = readr::col_integer()
  ))

  # remove NaN data at beginning
  catdata <- catdata %>% dplyr::filter(DateTime >= ymd_hms("2018-07-05 14:50:00"))

  # add flag columns
  catdata$Flag_All <- 0
  catdata$Flag_DO_1 <- 0
  catdata$Flag_DO_5 <- 0
  catdata$Flag_DO_9 <- 0
  catdata$Flag_Chla <- 0
  catdata$Flag_Phyco <- 0
  catdata$Flag_TDS <- 0

  # replace negative DO values with 0
  catdata <- catdata %>%
    dplyr::mutate(Flag_DO_1 = ifelse((! is.na(EXODO_mgL_1) & EXODO_mgL_1 < 0)
                              | (! is.na(EXODOsat_percent_1) & EXODOsat_percent_1 < 0), 3, Flag_DO_1)) %>%
    dplyr::mutate(EXODO_mgL_1 = ifelse(EXODO_mgL_1 < 0, 0, EXODO_mgL_1)) %>%
    dplyr::mutate(EXODOsat_percent_1 = ifelse(EXODOsat_percent_1 <0, 0, EXODOsat_percent_1))

  catdata <- catdata %>%
    dplyr::mutate(Flag_DO_5 = ifelse((! is.na(RDO_mgL_5) & RDO_mgL_5 < 0)
                              | (! is.na(RDOsat_percent_5) & RDOsat_percent_5 < 0), 3, Flag_DO_5)) %>%
    dplyr::mutate(RDO_mgL_5 = ifelse(RDO_mgL_5 < 0, 0, RDO_mgL_5)) %>%
    dplyr::mutate(RDOsat_percent_5 = ifelse(RDOsat_percent_5 < 0, 0, RDOsat_percent_5))

  catdata <- catdata %>%
    dplyr::mutate(Flag_DO_9 = ifelse((! is.na(RDO_mgL_9) & RDO_mgL_9 < 0)
                              | (! is.na(RDOsat_percent_9) & RDOsat_percent_9 < 0), 3, Flag_DO_9)) %>%
    dplyr::mutate(RDO_mgL_9 = ifelse(RDO_mgL_9 < 0, 0, RDO_mgL_9)) %>%
    dplyr::mutate(RDOsat_percent_9 = ifelse(RDOsat_percent_9 < 0, 0, RDOsat_percent_9))

  # modify catdata based on the information in the log
  for(i in 1:nrow(log))
  {
    # get start and end time of one maintenance event
    start <- log$TIMESTAMP_start[i]
    end <- log$TIMESTAMP_end[i]

    # get indices of columns affected by maintenance
    if(grepl("^\\d+$", log$colnumber[i])) # single num
    {
      maintenance_cols <- intersect(c(2:39), as.integer(log$colnumber[i]))
    }
    else if(grepl("^c\\(\\s*\\d+\\s*(;\\s*\\d+\\s*)*\\)$", log$colnumber[i])) # c(x;y;...)
    {
      maintenance_cols <- intersect(c(2:39), as.integer(unlist(regmatches(log$colnumber[i],
                                                                          gregexpr("\\d+", log$colnumber[i])))))
    }
    else if(grepl("^c\\(\\s*\\d+\\s*:\\s*\\d+\\s*\\)$", log$colnumber[i])) # c(x:y)
    {
      bounds <- as.integer(unlist(regmatches(log$colnumber[i], gregexpr("\\d+", log$colnumber[i]))))
      maintenance_cols <- intersect(c(2:39), c(bounds[1]:bounds[2]))
    }
    else
    {
      warning(paste("Could not parse column colnumber in row", i, "of the maintenance log. Skipping maintenance for",
                    "that row. The value of colnumber should be in one of three formats: a single number (\"47\"), a",
                    "semicolon-separated list of numbers in c() (\"c(47;48;49)\"), or a range of numbers in c() (\"c(47:74)\").",
                    "Other values (even valid calls to c()) will not be parsed properly."))
      next
    }

    # remove EXO_Date and EXO_Time columns from the list of maintenance columns, because they will be deleted later
    maintenance_cols <- setdiff(maintenance_cols, c(21, 22))

    if(length(maintenance_cols) == 0)
    {
      warning(paste("Did not parse any valid data columns in row", i, "of the maintenance log. Valid columns have",
                    "indices 2 through 39, excluding 21 and 22, which are deleted by this script. Skipping maintenance for that row."))
      next
    }

    # replace relevant data with NAs and set "all" flag while maintenance was in effect
    catdata[catdata$DateTime >= start & catdata$DateTime <= end, maintenance_cols] <- NA
    catdata[catdata$DateTime >= start & catdata$DateTime <= end, "Flag_All"] <- 1

    # if DO data was affected by maintenance, set the appropriate DO flags, and replace DO data with NAs after maintenance
    # was in effect until value returns to within a threshold of the value when maintenance began, because the sensors take
    # time to re-adjust to ambient conditions
    last_row_before_maintenance <- tail(catdata %>% filter(DateTime < start), 1)
    for(j in 1:3)
    {
      # if maintenance was not in effect on DO data, then skip
      if(! (DO_MGL_COLS[j] %in% maintenance_cols | DO_SAT_COLS[j] %in% maintenance_cols))
      {
        next
      }

      # set the appropriate DO flag while maintenance was in effect
      catdata[catdata$DateTime >= start & catdata$DateTime <= end, DO_FLAG_COLS[j]] <- 1

      last_DO_before_maintenance <- last_row_before_maintenance[[DO_MGL_COLS[j]]][1]
      if(is.na(last_DO_before_maintenance))
      {
        warning(paste("For row", i, "of the maintenance log, the pre-maintenance DO value at depth", DO_DEPTHS[j],
                      "could not be found. Not replacing DO values after the end of maintenance. This could occur because the start",
                      "date-time for maintenance is at or before the first date-time in the data, or simply because the value was",
                      "missing or replaced in prior maintenance."))
      }
      else
      {
        DO_recovery_time <- (catdata %>%
                               dplyr::filter(DateTime > end &
                                        abs(catdata[[DO_MGL_COLS[j]]] - last_DO_before_maintenance) <= DO_RECOVERY_THRESHOLD)
        )$DateTime[1]

        # if the recovery time cannot be found, then raise a warning and replace post-maintenance DO values until the end of
        # the file
        if(is.na(DO_recovery_time))
        {
          warning(paste("For row", i, "of the maintenance log, post-maintenance DO levels at depth", DO_DEPTHS[j],
                        "never returned within the given threshold of the pre-maintenance DO value. All post-maintenance DO values",
                        "have been replaced with NA. This could occur because the end date-time for maintenance is at or after the",
                        "last date-time in the data, or simply because post-maintenance levels never returned within the threshold."))
          catdata[catdata$DateTime > end, intersect(maintenance_cols, c(DO_MGL_COLS[j], DO_SAT_COLS[j]))] <- NA
          catdata[catdata$DateTime > end, DO_FLAG_COLS[j]] <- 1
        }
        else
        {
          catdata[catdata$DateTime > end & catdata$DateTime < DO_recovery_time,
                  intersect(maintenance_cols, c(DO_MGL_COLS[j], DO_SAT_COLS[j]))] <- NA
          catdata[catdata$DateTime > end & catdata$DateTime < DO_recovery_time, DO_FLAG_COLS[j]] <- 1
        }
      }
    }
  }

  # find EXO sonde sensor data that differs from the mean by more than the standard deviation times a given factor, and
  # replace with NAs between October and March, due to sensor fouling
  Chla_RFU_1_mean <- mean(catdata$EXOChla_RFU_1, na.rm = TRUE)
  Chla_ugL_1_mean <- mean(catdata$EXOChla_ugL_1, na.rm = TRUE)
  BGAPC_RFU_1_mean <- mean(catdata$EXOBGAPC_RFU_1, na.rm = TRUE)
  BGAPC_ugL_1_mean <- mean(catdata$EXOBGAPC_ugL_1, na.rm = TRUE)
  Chla_RFU_1_threshold <- EXO_FOULING_FACTOR * sd(catdata$EXOChla_RFU_1, na.rm = TRUE)
  Chla_ugL_1_threshold <- EXO_FOULING_FACTOR * sd(catdata$EXOChla_ugL_1, na.rm = TRUE)
  BGAPC_RFU_1_threshold <- EXO_FOULING_FACTOR * sd(catdata$EXOBGAPC_RFU_1, na.rm = TRUE)
  BGAPC_ugL_1_threshold <- EXO_FOULING_FACTOR * sd(catdata$EXOBGAPC_ugL_1, na.rm = TRUE)

  catdata <- catdata %>%
    dplyr::mutate(Flag_Chla = ifelse(DateTime >= ymd("2018-10-01") & DateTime < ymd("2019-03-01") &
                                (! is.na(EXOChla_RFU_1) & abs(EXOChla_RFU_1 - Chla_RFU_1_mean) > Chla_RFU_1_threshold |
                                   ! is.na(EXOChla_ugL_1) & abs(EXOChla_ugL_1 - Chla_ugL_1_mean) > Chla_ugL_1_threshold),
                              4, Flag_Chla)) %>%
    dplyr::mutate(Flag_Phyco = ifelse(DateTime >= ymd("2018-10-01") & DateTime < ymd("2019-03-01") &
                                 (! is.na(EXOBGAPC_RFU_1) & abs(EXOBGAPC_RFU_1 - BGAPC_RFU_1_mean) > BGAPC_RFU_1_threshold |
                                    ! is.na(EXOBGAPC_ugL_1) & abs(EXOBGAPC_ugL_1 - BGAPC_ugL_1_mean) > BGAPC_ugL_1_threshold),
                               4, Flag_Phyco)) %>%
    dplyr::mutate(EXOChla_RFU_1 = ifelse(DateTime >= ymd("2018-10-01") & DateTime < ymd("2019-03-01") &
                                    abs(EXOChla_RFU_1 - Chla_RFU_1_mean) > Chla_RFU_1_threshold, NA, EXOChla_RFU_1)) %>%
    dplyr::mutate(EXOChla_ugL_1 = ifelse(DateTime >= ymd("2018-10-01") & DateTime < ymd("2019-03-01") &
                                    abs(EXOChla_ugL_1 - Chla_ugL_1_mean) > Chla_ugL_1_threshold, NA, EXOChla_ugL_1)) %>%
    dplyr::mutate(EXOBGAPC_RFU_1 = ifelse(DateTime >= ymd("2018-10-01") & DateTime < ymd("2019-03-01") &
                                     abs(EXOBGAPC_RFU_1 - BGAPC_RFU_1_mean) > BGAPC_RFU_1_threshold, NA, EXOBGAPC_RFU_1)) %>%
    dplyr::mutate(EXOBGAPC_ugL_1 = ifelse(DateTime >= ymd("2018-10-01") & DateTime < ymd("2019-03-01") &
                                     abs(EXOBGAPC_ugL_1 - BGAPC_ugL_1_mean) > BGAPC_ugL_1_threshold, NA, EXOBGAPC_ugL_1))

  # flag EXO sonde sensor data of value above 4 * standard deviation at other times
  catdata <- catdata %>%
    dplyr::mutate(Flag_Phyco = ifelse(! is.na(EXOBGAPC_RFU_1) & abs(EXOBGAPC_RFU_1 - BGAPC_RFU_1_mean) > BGAPC_RFU_1_threshold |
                                 ! is.na(EXOBGAPC_ugL_1) & abs(EXOBGAPC_ugL_1 - BGAPC_ugL_1_mean) > BGAPC_ugL_1_threshold,
                               5, Flag_Phyco))

  # delete EXO_Date and EXO_Time columns
  catdata <- catdata %>% dplyr::select(-EXO_Date, -EXO_Time)

  # add Reservoir and Site columns
  #catdata$Reservoir <- "FCR"
  #catdata$Site <- "50"

  # reorder columns
  catdata <- catdata %>% dplyr::select(-RECORD, -CR6_Batt_V, -CR6Panel_Temp_C, -Flag_All, -Flag_DO_1, -Flag_DO_5,
                                -Flag_DO_9, -Flag_Chla, -Flag_Phyco, -Flag_TDS, -EXO_wiper, -EXO_cablepower,
                                -EXO_battery,-EXO_pressure)

  # replace NaNs with NAs
  catdata[is.na(catdata)] <- NA

  # convert datetimes to characters so that they are properly formatted in the output file
  catdata$DateTime <- as.character(catdata$DateTime)


  if(!is.na(qaqc_file)){
    #Different lakes are going to have to modify this for their temperature data format

    d1 <- catdata

    d2 <- read.csv(qaqc_file, na.strings = 'NA', stringsAsFactors = FALSE)

    TIMESTAMP_in <- as_datetime(d1$DateTime,tz = input_file_tz)
    d1$TIMESTAMP <- with_tz(TIMESTAMP_in,tz = local_tzone)

    TIMESTAMP_in <- as_datetime(d2$DateTime,tz = input_file_tz)
    d2$TIMESTAMP <- with_tz(TIMESTAMP_in,tz = local_tzone)

    d1 <- d1[which(d1$TIMESTAMP > d2$TIMESTAMP[nrow(d2)] | d1$TIMESTAMP < d2$TIMESTAMP[1]), ]

    #d3 <- data.frame(TIMESTAMP = d1$TIMESTAMP, wtr_surface = d1$wtr_surface, wtr_1 = d1$wtr_1, wtr_2 = d1$wtr_2, wtr_3 = d1$wtr_3, wtr_4 = d1$wtr_4,
    #                 wtr_5 = d1$wtr_5, wtr_6 = d1$wtr_6, wtr_7 = d1$wtr_7, wtr_8 = d1$wtr_8, wtr_9 = d1$wtr_9, wtr_1_exo = d1$EXO_wtr_1, wtr_5_do = d1$dotemp_5, wtr_9_do = d1$dotemp_9)

    d3 <-  data.frame(TIMESTAMP = d1$TIMESTAMP, wtr_surface = d1$ThermistorTemp_C_surface,
                      wtr_1 = d1$ThermistorTemp_C_1, wtr_2 = d1$ThermistorTemp_C_2,
                      wtr_3 = d1$ThermistorTemp_C_3, wtr_4 = d1$ThermistorTemp_C_4,
                      wtr_5 = d1$ThermistorTemp_C_5, wtr_6 = d1$ThermistorTemp_C_6,
                      wtr_7 = d1$ThermistorTemp_C_7, wtr_8 = d1$ThermistorTemp_C_8,
                      wtr_9 = d1$ThermistorTemp_C_9, wtr_1_exo = d1$EXOTemp_C_1,
                      wtr_5_do = d1$RDOTemp_C_5, wtr_9_do = d1$RDOTemp_C_9,
                      Chla_1 = d1$EXOChla_ugL_1, doobs_1 = d1$EXODO_mgL_1,
                      doobs_5 = d1$RDO_mgL_5, doobs_9 = d1$RDO_mgL_9,
                      fDOM_1 = d1$EXOfDOM_QSU_1, bgapc_1 = d1$EXOBGAPC_ugL_1,
                      depth_1.6 = d1$EXO_depth)

    d4 <- data.frame(TIMESTAMP = d2$TIMESTAMP, wtr_surface = d2$ThermistorTemp_C_surface,
                     wtr_1 = d2$ThermistorTemp_C_1, wtr_2 = d2$ThermistorTemp_C_2,
                     wtr_3 = d2$ThermistorTemp_C_3, wtr_4 = d2$ThermistorTemp_C_4,
                     wtr_5 = d2$ThermistorTemp_C_5, wtr_6 = d2$ThermistorTemp_C_6,
                     wtr_7 = d2$ThermistorTemp_C_7, wtr_8 = d2$ThermistorTemp_C_8,
                     wtr_9 = d2$ThermistorTemp_C_9, wtr_1_exo = d2$EXOTemp_C_1,
                     wtr_5_do = d2$RDOTemp_C_5, wtr_9_do = d2$RDOTemp_C_9,
                     Chla_1 = d2$EXOChla_ugL_1, doobs_1 = d2$EXODO_mgL_1,
                     doobs_5 = d2$RDO_mgL_5, doobs_9 = d2$RDO_mgL_9,
                     fDOM_1 = d2$EXOfDOM_QSU_1, bgapc_1 = d2$EXOBGAPC_ugL_1,
                     depth_1.6 = d2$EXO_depth)

    d <- rbind(d3,d4)

  }else{
    #Different lakes are going to have to modify this for their temperature data format
    d1 <- catdata

    TIMESTAMP_in <- as_datetime(d1$DateTime,tz = input_file_tz)
    d1$TIMESTAMP <- with_tz(TIMESTAMP_in,tz = local_tzone)

    d <-  data.frame(TIMESTAMP = d1$TIMESTAMP, wtr_surface = d1$ThermistorTemp_C_surface,
                     wtr_1 = d1$ThermistorTemp_C_1, wtr_2 = d1$ThermistorTemp_C_2,
                     wtr_3 = d1$ThermistorTemp_C_3, wtr_4 = d1$ThermistorTemp_C_4,
                     wtr_5 = d1$ThermistorTemp_C_5, wtr_6 = d1$ThermistorTemp_C_6,
                     wtr_7 = d1$ThermistorTemp_C_7, wtr_8 = d1$ThermistorTemp_C_8,
                     wtr_9 = d1$ThermistorTemp_C_9, wtr_1_exo = d1$EXOTemp_C_1,
                     wtr_5_do = d1$RDOTemp_C_5, wtr_9_do = d1$RDOTemp_C_9,
                     Chla_1 = d1$EXOChla_ugL_1, doobs_1 = d1$EXODO_mgL_1,
                     doobs_5 = d1$RDO_mgL_5, doobs_9 = d1$RDO_mgL_9,
                     fDOM_1 = d1$EXOfDOM_QSU_1, bgapc_1 = d1$EXOBGAPC_ugL_1,
                     depth_1.6 = d1$EXO_depth)
  }


  d$fDOM_1 <- config$exo_sensor_2_grab_sample_fdom[1] + config$exo_sensor_2_grab_sample_fdom[2] * d$fDOM_1

  #oxygen unit conversion
  d$doobs_1 <- d$doobs_1*1000/32  #mg/L (obs units) -> mmol/m3 (glm units)
  d$doobs_5 <- d$doobs_5*1000/32  #mg/L (obs units) -> mmol/m3 (glm units)
  d$doobs_9 <- d$doobs_9*1000/32  #mg/L (obs units) -> mmol/m3 (glm units)

  d$Chla_1 <-  config$exo_sensor_2_ctd_chla[1] +  d$Chla_1 *  config$exo_sensor_2_ctd_chla[2]
  d$doobs_1 <- config$exo_sensor_2_ctd_do[1]  +   d$doobs_1 * config$exo_sensor_2_ctd_do[2]
  d$doobs_5 <- config$do_sensor_2_ctd_do_5[1] +   d$doobs_5 * config$do_sensor_2_ctd_do_5[2]
  d$doobs_9 <- config$do_sensor_2_ctd_do_9[1] +   d$doobs_9 * config$do_sensor_2_ctd_do_9[2]

  d <- d %>%
    dplyr::mutate(day = day(TIMESTAMP),
           year = year(TIMESTAMP),
           hour = hour(TIMESTAMP),
           month = month(TIMESTAMP)) %>%
    dplyr::group_by(day, year, hour, month) %>%
    dplyr::select(-TIMESTAMP) %>%
    dplyr::summarise_all(mean, na.rm = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(day = as.numeric(day),
           hour = as.numeric(hour)) %>%
    dplyr::mutate(day = ifelse(as.numeric(day) < 10, paste0("0",day),day),
           hour = ifelse(as.numeric(hour) < 10, paste0("0",hour),hour)) %>%
    dplyr::mutate(timestamp = as_datetime(paste0(year,"-",month,"-",day," ",hour,":00:00"),tz = local_tzone)) %>%
    dplyr::arrange(timestamp)


  d_therm <- d %>%
    dplyr::select(timestamp, wtr_surface, wtr_1, wtr_2, wtr_3, wtr_4, wtr_5, wtr_6,
           wtr_7,wtr_8, wtr_9) %>%
    dplyr::rename("0.0" = wtr_surface,
           "1.0" = wtr_1,
           "2.0" = wtr_2,
           "3.0" = wtr_3,
           "4.0" = wtr_4,
           "5.0" = wtr_5,
           "6.0" = wtr_6,
           "7.0" = wtr_7,
           "8.0" = wtr_8,
           "9.0" = wtr_9) %>%
    tidyr::pivot_longer(cols = -timestamp, names_to = "depth", values_to = "value") %>%
    dplyr::mutate(variable = "temperature",
           method = "thermistor",
           value = ifelse(is.nan(value), NA, value))


  d_do_temp <- d %>%
    dplyr::select(timestamp, wtr_5_do, wtr_9_do) %>%
    dplyr::rename("5.0" = wtr_5_do,
           "9.0" = wtr_9_do) %>%
    tidyr::pivot_longer(cols = -timestamp, names_to = "depth", values_to = "value") %>%
    dplyr::mutate(variable = "temperature",
           method = "do_sensor",
           value = ifelse(is.nan(value), NA, value))

  d_exo_temp <- d %>%
    dplyr::select(timestamp, wtr_1_exo) %>%
    rename("1.6" = wtr_1_exo) %>%
    pivot_longer(cols = -timestamp, names_to = "depth", values_to = "value") %>%
    mutate(variable = "temperature",
           method = "exo_sensor",
           value = ifelse(is.nan(value), NA, value))

  d_do_do <- d %>%
    select(timestamp, doobs_5, doobs_9) %>%
    rename("5.0" = doobs_5,
           "9.0" = doobs_9) %>%
    pivot_longer(cols = -timestamp, names_to = "depth", values_to = "value") %>%
    mutate(variable = "oxygen",
           method = "do_sensor",
           value = ifelse(is.nan(value), NA, value))

  d_exo_do <- d %>%
    select(timestamp, doobs_1) %>%
    rename("1.6" = doobs_1) %>%
    pivot_longer(cols = -timestamp, names_to = "depth", values_to = "value") %>%
    mutate(variable = "oxygen",
           method = "exo_sensor",
           value = ifelse(is.nan(value), NA, value))

  d_exo_fdom <- d %>%
    select(timestamp, fDOM_1) %>%
    rename("1.6" = fDOM_1) %>%
    pivot_longer(cols = -timestamp, names_to = "depth", values_to = "value") %>%
    mutate(variable = "fdom",
           method = "exo_sensor",
           value = ifelse(is.nan(value), NA, value))

  d_exo_chla <- d %>%
    select(timestamp, Chla_1) %>%
    rename("1.6" = Chla_1) %>%
    pivot_longer(cols = -timestamp, names_to = "depth", values_to = "value") %>%
    mutate(variable = "chla",
           method = "exo_sensor",
           value = ifelse(is.nan(value), NA, value))

  d_exo_bgapc <- d %>%
    select(timestamp, bgapc_1) %>%
    rename("1.6" = bgapc_1) %>%
    pivot_longer(cols = -timestamp, names_to = "depth", values_to = "value") %>%
    mutate(variable = "bgapc",
           method = "exo_sensor",
           value = ifelse(is.nan(value), NA, value))

  if(config$variable_obsevation_depths == TRUE){

    d_depth <- d %>% mutate(wtr_surface = depth_1.6 - 0.6 - 0.9,
                            wtr_1 = depth_1.6 - 0.6,
                            wtr_2 = depth_1.6 + 0.4,
                            wtr_3 = depth_1.6 + 1.4,
                            wtr_4 = depth_1.6 + 2.4,
                            wtr_5 = depth_1.6 + 3.4,
                            wtr_6 = depth_1.6 + 4.4,
                            wtr_7 = depth_1.6 + 5.4,
                            wtr_8 = depth_1.6 + 6.4,
                            wtr_9 = depth_1.6 + 7.4,
                            wtr_1_exo = depth_1.6,
                            wtr_5_do = depth_1.6 + 3.4,
                            wtr_9_do = depth_1.6 + 7.4,
                            Chla_1 = depth_1.6,
                            doobs_1 = depth_1.6 - 0.6,
                            doobs_5 = depth_1.6 + 3.4,
                            doobs_9 = depth_1.6 + 7.4,
                            fDOM_1 = depth_1.6,
                            bgapc_1 = depth_1.6) %>%
      select(-c(depth_1.6, day,year, hour, month))


    d_therm_depth <- d_depth %>%
      select(timestamp, wtr_surface, wtr_1, wtr_2, wtr_3, wtr_4, wtr_5, wtr_6,
             wtr_7,wtr_8, wtr_9) %>%
      rename("0.0" = wtr_surface,
             "1.0" = wtr_1,
             "2.0" = wtr_2,
             "3.0" = wtr_3,
             "4.0" = wtr_4,
             "5.0" = wtr_5,
             "6.0" = wtr_6,
             "7.0" = wtr_7,
             "8.0" = wtr_8,
             "9.0" = wtr_9) %>%
      pivot_longer(cols = -timestamp, names_to = "depth", values_to = "depth_exo") %>%
      mutate(variable = "temperature",
             method = "thermistor",
             depth_exo = ifelse(is.nan(depth_exo), NA, depth_exo))

    d_therm <- d_therm %>%
      left_join(d_therm_depth, by = c("timestamp", "depth","variable", "method")) %>%
      mutate(depth = ifelse(!is.na(depth_exo),depth_exo,depth),
             depth = as.numeric(depth),
             depth = round(depth, 2)) %>%
      select(-depth_exo) %>%
      filter(depth > 0.0)

    d_do_temp_depth <- d_depth %>%
      select(timestamp, wtr_5_do, wtr_9_do) %>%
      rename("5.0" = wtr_5_do,
             "9.0" = wtr_9_do) %>%
      pivot_longer(cols = -timestamp, names_to = "depth", values_to = "depth_exo") %>%
      mutate(variable = "temperature",
             method = "do_sensor")

    d_do_temp <- d_do_temp %>%
      left_join(d_do_temp_depth, by = c("timestamp", "depth","variable", "method")) %>%
      mutate(depth = ifelse(!is.na(depth_exo),depth_exo,depth),
             depth = as.numeric(depth),
             depth = round(depth, 2)) %>%
      select(-depth_exo) %>%
      filter(depth > 0.0)

    d_exo_temp_depth <- d_depth %>%
      select(timestamp, wtr_1_exo) %>%
      rename("1.6" = wtr_1_exo) %>%
      pivot_longer(cols = -timestamp, names_to = "depth", values_to = "depth_exo") %>%
      mutate(variable = "temperature",
             method = "exo_sensor")

    d_exo_temp <- d_exo_temp %>%
      left_join(d_do_temp_depth, by = c("timestamp", "depth","variable", "method")) %>%
      mutate(depth = ifelse(!is.na(depth_exo),depth_exo,depth),
             depth = as.numeric(depth),
             depth = round(depth, 2)) %>%
      select(-depth_exo) %>%
      filter(depth > 0.0)

    d_do_do_depth <- d_depth %>%
      select(timestamp, doobs_5, doobs_9) %>%
      rename("5.0" = doobs_5,
             "9.0" = doobs_9) %>%
      pivot_longer(cols = -timestamp, names_to = "depth", values_to = "depth_exo") %>%
      mutate(variable = "oxygen",
             method = "do_sensor")

    d_do_do <- d_do_do %>%
      left_join(d_do_do_depth, by = c("timestamp", "depth","variable", "method")) %>%
      mutate(depth = ifelse(!is.na(depth_exo),depth_exo,depth),
             depth = as.numeric(depth),
             depth = round(depth, 2)) %>%
      select(-depth_exo) %>%
      filter(depth > 0.0)

    d_exo_do_depth <- d_depth %>%
      select(timestamp, doobs_1) %>%
      rename("1.6" = doobs_1) %>%
      pivot_longer(cols = -timestamp, names_to = "depth", values_to = "depth_exo") %>%
      mutate(variable = "oxygen",
             method = "exo_sensor")

    d_exo_do <- d_exo_do %>%
      left_join(d_exo_do_depth, by = c("timestamp", "depth","variable", "method")) %>%
      mutate(depth = ifelse(!is.na(depth_exo),depth_exo,depth),
             depth = as.numeric(depth),
             depth = round(depth, 2)) %>%
      select(-depth_exo) %>%
      filter(depth > 0.0)

    d_exo_fdom_depth <- d_depth %>%
      select(timestamp, fDOM_1) %>%
      rename("1.6" = fDOM_1) %>%
      pivot_longer(cols = -timestamp, names_to = "depth", values_to = "depth_exo") %>%
      mutate(variable = "oxygen",
             method = "exo_sensor")

    d_exo_fdom <- d_exo_fdom %>%
      left_join(d_exo_fdom_depth, by = c("timestamp", "depth","variable", "method")) %>%
      mutate(depth = ifelse(!is.na(depth_exo),depth_exo,depth),
             depth = as.numeric(depth),
             depth = round(depth, 2)) %>%
      select(-depth_exo) %>%
      filter(depth > 0.0)

    d_exo_chla_depth <- d_depth %>%
      select(timestamp, Chla_1) %>%
      rename("1.6" = Chla_1) %>%
      pivot_longer(cols = -timestamp, names_to = "depth", values_to = "depth_exo") %>%
      mutate(variable = "chla",
             method = "exo_sensor")

    d_exo_chla <- d_exo_chla %>%
      left_join(d_exo_chla_depth, by = c("timestamp", "depth","variable", "method")) %>%
      mutate(depth = ifelse(!is.na(depth_exo),depth_exo,depth),
             depth = as.numeric(depth),
             depth = round(depth, 2)) %>%
      select(-depth_exo) %>%
      filter(depth > 0.0)

    d_exo_bgapc_depth <- d_depth %>%
      select(timestamp, bgapc_1) %>%
      rename("1.6" = bgapc_1) %>%
      pivot_longer(cols = -timestamp, names_to = "depth", values_to = "depth_exo") %>%
      mutate(variable = "bgapc",
             method = "exo_sensor")

    d_exo_bgapc <- d_exo_bgapc %>%
      left_join(d_exo_chla_depth, by = c("timestamp", "depth","variable", "method")) %>%
      mutate(depth = ifelse(!is.na(depth_exo),depth_exo,depth),
             depth = as.numeric(depth),
             depth = round(depth, 2)) %>%
      select(-depth_exo) %>%
      filter(depth > 0.0)
  }


  d <- rbind(d_therm,d_do_temp,d_exo_temp,d_do_do,d_exo_do,d_exo_fdom,
             d_exo_chla,d_exo_bgapc)

  d <- d %>% mutate(depth = as.numeric(depth))



  # write to output file
  return(d)
}

# example usage
# qaqc("https://raw.githubusercontent.com/CareyLabVT/SCCData/mia-data/Catwalk.csv",
#      "https://raw.githubusercontent.com/CareyLabVT/SCCData/mia-data/CAT_MaintenanceLog.txt",
#      "Catwalk.csv")
