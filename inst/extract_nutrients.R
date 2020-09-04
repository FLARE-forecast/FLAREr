extract_nutrients <- function(fname,
                        input_file_tz,
                        local_tzone,
                        focal_depths){

  d <- read_csv(fname, guess_max = 1000000,
                col_types = readr::cols()) %>%
    mutate(DateTime = force_tz(DateTime, tzone = input_file_tz),
           DateTime = with_tz(DateTime, tzone = local_tzone)) %>%
    filter(Reservoir == "FCR" & Site == "50") %>%
    mutate(TN = TN_ugL * 1000 * 0.001 * (1/14),
           TP = TP_ugL * 1000 * 0.001 * (1/30.97),
           NH4 = NH4_ugL * 1000 * 0.001 * (1 / 18.04),
           NO3NO2 = NO3NO2_ugL * 1000 * 0.001 * (1/62.00),
           SRP = SRP_ugL * 1000 * 0.001 * (1/95),
           DOC = DOC_mgL* 1000 * (1/12.01),
           DIC = DIC_mgL*1000*(1/52.515)) %>%
    select(DateTime, Depth_m, TN, TP, NH4, NO3NO2, SRP, DOC, DIC) %>%
    rename("timestamp" = DateTime,
           "depth" = Depth_m,
           "fdom" = DOC) %>%
    pivot_longer(cols = -c(timestamp, depth), names_to = "variable", values_to = "value") %>%
    mutate(method = "grab_sample") %>%
    filter(!is.na(value)) %>%
    select(timestamp , depth, value, variable, method)

  if(!is.na(focal_depths)){
    d <- d %>% filter(depth %in% focal_depths)
  }

  return(d)
}
