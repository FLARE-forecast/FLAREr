###Information and extra lines that might be helpful###
#####
### This function aggregates historical and real time inflow data at FCR,
### converts flow to cms. This creates one output file named "inflow_postQAQC.csv"

#Inputs to function:
#  Historical flow data. Here, I need two historical files from github to fill all of the time from 2013 until diana was launched ('hist1' and hist2').
#  Diana data ('FCRweir.csv' from git)
#  Local timezone
#  Timezone of input files (just to be similar to other FLARE fucntions)

#Units:
#  flow = cms
#  wtr_temp = degC
#  Timestep = daily
#Missing data is assigned the prior days flow

##These lines may be useful for getting this into FLARE
#
#     data_location <- paste0("your working directory")
#     diana_location <- paste0(data_location, "/", "diana_data")

#     if(!file.exists(diana_location)){
#       setwd(data_location)
#       system("git clone -b diana-data --single-branch https://github.com/CareyLabVT/SCCData.git diana-data/")
#     }
#
#
#     setwd(diana_location)
#     system(paste0("git pull"))
#####
inflow_qaqc <- function(realtime_file,
                        qaqc_file,
                        nutrients_file,
                        cleaned_inflow_file,
                        local_tzone,
                        input_file_tz){

  ##Step 1: Pull required data from GitHub ##
  diana_location <- realtime_file

  ##Step 2: Read in historical flow data, clean, and aggregate to daily mean##

  flow <- readr::read_csv(qaqc_file, guess_max = 1000000, col_types = readr::cols()) %>%
    dplyr::rename("timestamp" = DateTime) %>%
    dplyr::select(timestamp, WVWA_Flow_cms, WVWA_Temp_C, VT_Flow_cms, VT_Temp_C) %>%
    dplyr::mutate(day = day(timestamp),
           year = year(timestamp),
           month = month(timestamp)) %>%
    dplyr::group_by(day, year, month) %>%
    dplyr:: summarize(WVWA_Flow_cms = mean(WVWA_Flow_cms, na.rm = TRUE),
              WVWA_Temp_C = mean(WVWA_Temp_C, na.rm = TRUE),
              VT_Flow_cms = mean(VT_Flow_cms, na.rm = TRUE),
              VT_Temp_C = mean(VT_Temp_C, na.rm = TRUE), .groups = "drop") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(day = as.numeric(day)) %>%
    dplyr::mutate(day = ifelse(as.numeric(day) < 10, paste0("0",day),day)) %>%
    dplyr::mutate(time = as_date(paste0(year,"-",month,"-",day))) %>%
    dplyr::select(time,WVWA_Flow_cms,WVWA_Temp_C,VT_Flow_cms,VT_Temp_C) %>%
    dplyr::mutate(VT_Flow_cms = ifelse(is.nan(VT_Flow_cms), NA, VT_Flow_cms),
           VT_Temp_C = ifelse(is.nan(VT_Temp_C), NA, VT_Temp_C),
           WVWA_Flow_cms = ifelse(is.nan(WVWA_Flow_cms), NA, WVWA_Flow_cms),
           WVWA_Temp_C = ifelse(is.nan(WVWA_Temp_C), NA, WVWA_Temp_C)) %>%
    dplyr::arrange(time)

  inflow_temp_flow <- tibble(time = seq(first(flow$time), last(flow$time), by = "1 day")) %>%
    left_join(flow, by = "time") %>%
    mutate(TEMP = ifelse(is.na(VT_Temp_C), WVWA_Temp_C, VT_Temp_C),
           FLOW = ifelse(time > as_date("2019-06-07"), VT_Flow_cms, WVWA_Flow_cms),
           SALT = 0) %>%
    mutate(TEMP = imputeTS::na_interpolation(TEMP),
           FLOW = imputeTS::na_interpolation(FLOW)) %>%
    select(time, FLOW, TEMP, SALT)

  ##Step 3: Read in diana data, convert flow from PSI to CSM, calculations to
  #account for building new weir in June 2019 (FCR Specific), and
  #aggregate to daily mean.##

  inflow_realtime <- read_csv(diana_location, skip=4, col_names = F, col_types = readr::cols())
  inflow_realtime_headers <- read.csv(diana_location, skip=1, header = F, nrows= 1, as.is=T)
  colnames(inflow_realtime) <- inflow_realtime_headers
  inflow_realtime <- inflow_realtime %>%
    select(TIMESTAMP, Lvl_psi, wtr_weir) %>%
    rename("psi_corr" = Lvl_psi,
           "time" = TIMESTAMP,
           "TEMP" = wtr_weir) %>%
    mutate(time = force_tz(time, tzone = input_file_tz),
           time = with_tz(time, tzone = local_tzone)) %>%
    filter(time > last(inflow_temp_flow$time)) %>%
    mutate(head = ((65.822 * psi_corr) - 4.3804) / 100,
           FLOW = 2.391 * (head^2.5)) %>%
    mutate(day = day(time),
           year = year(time),
           month = month(time)) %>%
    group_by(day, year, month) %>%
    summarize(FLOW = mean(FLOW, na.rm = TRUE),
              TEMP = mean(TEMP, na.rm = TRUE), .groups = "drop") %>%
    ungroup() %>%
    mutate(day = as.numeric(day)) %>%
    mutate(day = ifelse(as.numeric(day) < 10, paste0("0",day),day)) %>%
    mutate(time = as_date(paste0(year,"-",month,"-",day))) %>%
    select(time, FLOW, TEMP) %>%
    mutate(FLOW = 0.003122 + 0.662914*FLOW, #Convert Diana to WVWA
           SALT = 0.0)

  inflow_combined <- full_join(inflow_temp_flow, inflow_realtime, by = "time") %>%
    mutate(FLOW = ifelse(is.na(FLOW.x), FLOW.y, FLOW.x),
           TEMP = ifelse(is.na(TEMP.x), TEMP.y, TEMP.x),
           SALT = ifelse(is.na(SALT.x), SALT.y, SALT.x)) %>%
    select(time, FLOW, TEMP, SALT)

  #### BRING IN THE NUTRIENTS

  if(!is.na(nutrients_file)){

    nutrients <- read_csv(nutrients_file, guess_max = 100000, col_types = readr::cols()) %>%
      filter(Reservoir == "FCR" & Site == "100") %>%
      rename("time" = DateTime)  %>%
      mutate(time = as_date(time)) %>%
      mutate(NIT_amm = NH4_ugL*1000*0.001*(1/18.04),
             NIT_nit =  NO3NO2_ugL*1000*0.001*(1/62.00),
             PHS_frp = SRP_ugL*1000*0.001*(1/94.9714),
             #PHS_frp_ads = PHS_frp,
             OGM_doc = DOC_mgL*1000*(1/12.01)* 0.10,
             OGM_docr = DOC_mgL*1000*(1/12.01)* 0.90,
             OGM_poc = 0.1*(OGM_doc+OGM_docr),
             TN = TN_ugL*1000*0.001*(1/14),
             TP = TP_ugL*1000*0.001*(1/30.97),
             OGM_don = (5/6)*(TN-(NIT_amm+NIT_nit)) * 0.1,
             OGM_donr = (5/6)*(TN-(NIT_amm+NIT_nit)) * 0.9,
             OGM_dop = 0.3*(TP - PHS_frp) *0.1,
             OGM_dopr = 0.3*(TP - PHS_frp) * 0.9,
             OGM_pon = (1/6)*(TN -(NIT_amm+NIT_nit)),
             OGM_pop = 0.7*(TP - PHS_frp),
             #CAR_dic = DIC_mgL*1000*(1/52.515),
             #CAR_ch4 = 0.0,
             SIL_rsi = 126.3866) %>%
      select(time, NIT_amm, NIT_nit, PHS_frp, OGM_doc, OGM_docr, OGM_poc, OGM_don,OGM_donr, OGM_dop, OGM_dopr, OGM_pop, OGM_pon,SIL_rsi)

    inflow_combined_with_na <- left_join(inflow_combined, nutrients, by = "time") %>%
      mutate(OXY_oxy = rMR::Eq.Ox.conc(TEMP, elevation.m = 506,
                                  bar.press = NULL, bar.units = NULL,
                                  out.DO.meas = "mg/L",
                                  salinity = 0, salinity.units = "pp.thou")*1000*(1/32)) %>%
      mutate(NIT_amm = ifelse(time <= last(nutrients$time) & time >= first(nutrients$time), imputeTS::na_interpolation(NIT_amm), NA),
             NIT_nit = ifelse(time <= last(nutrients$time) & time >= first(nutrients$time), imputeTS::na_interpolation(NIT_nit), NA),
             PHS_frp = ifelse(time <= last(nutrients$time) & time >= first(nutrients$time), imputeTS::na_interpolation(PHS_frp), NA),
             OGM_doc = ifelse(time <= last(nutrients$time) & time >= first(nutrients$time), imputeTS::na_interpolation(OGM_doc), NA),
             OGM_docr = ifelse(time <= last(nutrients$time) & time >= first(nutrients$time), imputeTS::na_interpolation(OGM_docr), NA),
             OGM_poc = ifelse(time <= last(nutrients$time) & time >= first(nutrients$time), imputeTS::na_interpolation(OGM_poc), NA),
             OGM_don = ifelse(time <= last(nutrients$time) & time >= first(nutrients$time), imputeTS::na_interpolation(OGM_don), NA),
             OGM_donr = ifelse(time <= last(nutrients$time) & time >= first(nutrients$time), imputeTS::na_interpolation(OGM_donr), NA),
             OGM_dop = ifelse(time <= last(nutrients$time) & time >= first(nutrients$time), imputeTS::na_interpolation(OGM_dop), NA),
             OGM_dopr = ifelse(time <= last(nutrients$time) & time >= first(nutrients$time), imputeTS::na_interpolation(OGM_dopr), NA),
             OGM_pop = ifelse(time <= last(nutrients$time) & time >= first(nutrients$time), imputeTS::na_interpolation(OGM_pop), NA),
             OGM_pon = ifelse(time <= last(nutrients$time) & time >= first(nutrients$time), imputeTS::na_interpolation(OGM_pon), NA),
             #PHS_frp_ads = ifelse(time <= last(nutrients$time) & time >= first(nutrients$time), na_interpolation(PHS_frp_ads), NA),
             #CAR_dic = ifelse(time <= last(nutrients$time) & time >= first(nutrients$time), na_interpolation(CAR_dic), NA),
             #CAR_ch4 = ifelse(time <= last(nutrients$time) & time >= first(nutrients$time), na_interpolation(CAR_ch4), NA),
             SIL_rsi = ifelse(time <= last(nutrients$time) & time >= first(nutrients$time), imputeTS::na_interpolation(SIL_rsi), NA)
      ) %>%
      mutate(OGM_dop = ifelse(time > as_date("2013-09-01") & time < as_date("2015-01-01"), NA, OGM_dop),
             OGM_dopr = ifelse(time > as_date("2013-09-01") & time < as_date("2015-01-01"), NA, OGM_dopr),
             OGM_pop = ifelse(time > as_date("2013-09-01") & time < as_date("2015-01-01"), NA, OGM_pop))

    nutrients_monthly <- nutrients %>%
      mutate(month = month(time)) %>%
      group_by(month) %>%
      summarise_at(vars(NIT_amm:SIL_rsi), mean, na.rm = TRUE, .groups = "drop")

    inflow_clean <- inflow_combined_with_na %>%
      mutate(month = month(time)) %>%
      left_join(nutrients_monthly, by = "month") %>%
      mutate(NIT_amm = ifelse(is.na(NIT_amm.x), NIT_amm.y,NIT_amm.x),
             NIT_nit = ifelse(is.na(NIT_nit.x), NIT_nit.y,NIT_nit.x),
             PHS_frp = ifelse(is.na(PHS_frp.x), PHS_frp.y,PHS_frp.x),
             OGM_doc = ifelse(is.na(OGM_doc.x), OGM_doc.y,OGM_doc.x),
             OGM_docr = ifelse(is.na(OGM_docr.x), OGM_docr.y,OGM_docr.x),
             OGM_poc = ifelse(is.na(OGM_poc.x), OGM_poc.y,OGM_poc.x),
             OGM_don = ifelse(is.na(OGM_don.x), OGM_don.y,OGM_don.x),
             OGM_donr = ifelse(is.na(OGM_don.x), OGM_don.y,OGM_don.x),
             OGM_dop = ifelse(is.na(OGM_dop.x), OGM_dop.y,OGM_dop.x),
             OGM_dopr = ifelse(is.na(OGM_dop.x), OGM_dop.y,OGM_dop.x),
             OGM_pop = ifelse(is.na(OGM_pop.x), OGM_pop.y,OGM_pop.x),
             OGM_pon = ifelse(is.na(OGM_pon.x), OGM_pon.y,OGM_pon.x),
             #PHS_frp_ads = ifelse(is.na(PHS_frp_ads.x), PHS_frp_ads.y,PHS_frp_ads.x),
             #CAR_dic = ifelse(is.na(CAR_dic.x), CAR_dic.y,CAR_dic.x)) %>%
      #CAR_ch4 = ifelse(is.na(CAR_ch4.x), CAR_ch4.y,CAR_ch4.x),
             SIL_rsi = ifelse(is.na(SIL_rsi.x), SIL_rsi.y,SIL_rsi.x))  %>%
      select(time, FLOW,TEMP,SALT,OXY_oxy, SIL_rsi, NIT_amm,NIT_nit,PHS_frp,OGM_doc,OGM_docr,OGM_poc,OGM_don,OGM_donr,OGM_pon,OGM_dop,OGM_dopr,OGM_pop)

  }else{
    inflow_clean <- inflow_combined
  }

  #inflow_clean %>%
  #   pivot_longer(cols = -time, names_to = "Nutrient", values_to = "values") %>%
  #   ggplot(aes(x = time, y = values)) +
  #   geom_point() +
  #   geom_line() +
  #   facet_wrap(~Nutrient, scales = "free")


  #inflow_clean <- inflow_clean %>%
  #  filter(time < as_datetime("2018-01-01 00:00:00"))


  readr::write_csv(inflow_clean, cleaned_inflow_file)

}


