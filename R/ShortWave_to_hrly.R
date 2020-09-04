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


ShortWave_to_hrly <- function(debiased, time0, lat, lon, local_tzone){
  ## downscale shortwave to hourly

  # grouping = "NOAA.member"
  # if("fday.group" %in% colnames(debiased)){
  #   grouping = append(grouping, "fday.group")
  #
  #   ShortWave.hours <- debiased %>%
  #     dplyr::mutate(fday = fday.group) %>%
  #     unique() %>%
  #     group_by_at(grouping) %>%
  #     expand(fday = seq((fday.group - 23/24), fday.group, by = 1/24)) %>%# the days since start of forecast
  #     ungroup() %>%
  #     filter(fday > -1/24) %>% # (rounding error causes the first "fday" to be -1e-16 instead of 0)
  #     dplyr::mutate(timestamp = as_datetime(time0 + fday*24*60*60, tz = local_tzone)) %>%
  #     # filter(timestamp >= time0 + 6*60*60) %>%
  #     dplyr::mutate(date = as_date(timestamp),
  #                   hour = hour(timestamp))
  #
  # }else{
  #   grouping = append(grouping, "date")
  #   ShortWave.hours <- debiased %>%
  #     dplyr::group_by_at(grouping) %>%
  #     tidyr::expand(hour = 0:23)
  # }
  #
  # d5 <- debiased %>%
  #   select(ShortWave, grouping) %>%
  #   full_join(ShortWave.hours, by = grouping) %>%
  #   dplyr::mutate(timestamp = as_datetime(paste(date, " ", hour, ":","00:00", sep = ""), tz = local_tzone) - 1*60*60)
  #
  # d4 <- debiased %>%
  #   select(ShortWave, grouping) %>%
  #   full_join(ShortWave.hours, by = grouping) %>%
  #   dplyr::mutate(timestamp = as_datetime(paste(date, " ", hour, ":","00:00", sep = ""), tz = "GMT") - 1*60*60) %>% # subtract one hour to convert times from representing pervious hour to representing the next hour
  #   dplyr::mutate(doy = yday(timestamp) + hour/24) %>%
  #   dplyr::mutate(rpot = solar_geom(doy, lon, lat)) %>%
  #   dplyr::mutate(timestamp = with_tz(timestamp, tz = local_tzone)) %>%
  #   select(timestamp, NOAA.member, rpot)
  #
  # d6 <- inner_join(d5, d4, by = c("timestamp", "NOAA.member")) %>% # hourly sw flux calculated using solar geometry
  #   dplyr::group_by_at(grouping) %>%
  #   dplyr::mutate(avg.rpot = mean(rpot, na.rm = TRUE)) %>% # daily sw mean from solar geometry
  #   ungroup() %>%
  #   dplyr::mutate(ShortWave = ifelse(avg.rpot > 0, ShortWave * (rpot/avg.rpot),0)) %>%
  #   select(timestamp, NOAA.member, ShortWave)

  timestamp <- seq(time0, time0 + lubridate::days(max(debiased$fday.group)), by = "1 hour")
  timestamp <- timestamp[1:(length(timestamp)-1)]
  timestamp_UTC <- lubridate::with_tz(timestamp, tzone = "UTC")
  hr <- lubridate::hour(timestamp_UTC)
  doy <- lubridate::yday(timestamp_UTC) + hr/24
  rpot <- flare::solar_geom(doy, lon, lat)

  fday <- NULL

  for(k in 1:max(debiased$fday.group)){
    fday <- c(fday, rep(k, 24))
  }


  d <- tibble::tibble(timestamp,timestamp_UTC,hr, doy, rpot, fday)

  davg <- d %>%
    dplyr::group_by(fday) %>%
    dplyr::summarize(mean_solar = mean(rpot), .groups = "drop")

  d2 <- dplyr::full_join(d, davg, by = "fday") %>%
    dplyr::mutate(SolorProp = rpot/mean_solar) %>%
    dplyr::rename(fday.group = fday)

  d3 <- dplyr::full_join(d2, debiased, by = "fday.group")

  d4 <- debiased %>%
    dplyr::select(fday.group, NOAA.member, ShortWave, dscale.member) %>%
    dplyr::left_join(d2, by = "fday.group") %>%
    dplyr::mutate(ShortWave = ShortWave * SolorProp) %>%
    dplyr::select(timestamp, NOAA.member, ShortWave, dscale.member)

}
