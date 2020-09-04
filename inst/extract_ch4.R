extract_ch4 <- function(fname,
                        input_file_tz,
                        local_tzone,
                        focal_depths){

  d <- read_csv(fname, guess_max = 1000000,
                col_types = readr::cols()) %>%
    mutate(DateTime = as_datetime(DateTime),
           DateTime = force_tz(DateTime, tzone = input_file_tz),
           DateTime = DateTime + hours(12))%>%
    filter(Reservoir == "FCR" & Depth_m < 10) %>%
    mutate(CH4 = ch4_umolL * 1000 * 0.001) %>%
    select(DateTime, Depth_m,CH4,Rep) %>%
    group_by(DateTime,Depth_m) %>%
    summarise(CH4 = mean(CH4, na.rm = TRUE)) %>%
    ungroup() %>%
    rename("timestamp" = DateTime,
           "depth" = Depth_m) %>%
    pivot_longer(cols = -c(timestamp, depth), names_to = "variable", values_to = "value") %>%
    mutate(method = "grab_sample") %>%
    filter(!is.na(value)) %>%
    select(timestamp , depth, value, variable, method)

  if(!is.na(focal_depths)){
    d <- d %>% filter(depth %in% focal_depths)
  }

  return(d)
}
