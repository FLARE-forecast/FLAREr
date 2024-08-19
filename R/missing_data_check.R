#' Check for NA, NaN, or Inf values within the forecast met files
#'
#' @param met_df met dataframe to check
#'
#' @return numeric; number of NA, NaN, or Inf values in the file
#' @noRd

missing_data_check <- function(met_df){

  #identify any Inf values
  check_flag <- 0
  for (i in seq.int(1:ncol(met_df))){
    cname <- names(met_df)[i]
    TF_check_test <- is.infinite(met_df %>% pull(cname))
    if (TRUE %in% TF_check_test){
      stop(paste0('met file has Inf values in column: ',cname))
    }
  }

  # check for missing data
  na_check <- sum(is.na(met_df))

  if (na_check != 0){
    stop('NA or NaN value found in met forecast file')
  }
}
