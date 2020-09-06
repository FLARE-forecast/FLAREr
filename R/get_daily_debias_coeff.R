#' @title Download and Downscale NOAA GEFS for a single site
#' @return None
#'
#' @param site_index, index of site_list, lat_list, lon_list to be downloaded
#' @param lat_list, vector of latitudes that correspond to site codes
#' @param lon_list, vector of longitudes that correspond to site codes
#' @param site_list, vector of site codes, used in directory and file name generation
#' @param downscale, logical specifying whether to downscale from 6-hr to 1-hr
#' @param overwrite, logical stating to overwrite any existing output_file
#' @param model_name, directory name for the 6-hr forecast, this will be used in directory and file name generation
#' @param model_name_ds, directory name for the 1-hr forecast, this will be used in directory and file name generation
#' @param output_directory, directory where the model output will be save
#' @noRd
#'
#' @author Quinn Thomas
#'

get_daily_debias_coeff <- function(joined.data,
                                   VarInfo,
                                   PLOT,
                                   working_directory){
  # --------------------------------------
  # purpose: save coefficients for linear debiasing (slope, intercept, standard deviation of residuals, r2 of linear regression), for comparison of total values over time (precip), and the covariance matrix between variables
  # Creator: Laura Puckett, December 14 2018
  # --------------------------------------

  get_coeff <- function(col.obs, col.for, method, PLOT, VarName, working_directory){
    if(method == "lm"){
      model <- lm(unlist(col.obs) ~ unlist(col.for))
      intercept <- model$coefficients[1]
      slope <- model$coefficients[2]
      res <- residuals(model)
      r2 <- summary(model)$r.squared
    }else if(method == "compare_totals"){
      # option where comparing sum rather than fitting a line
      slope <- sum(col.obs)/sum(col.for)
      intercept <- 0
      res <- col.for*slope - col.obs
      r2 <- NA
    }else if(method == "none"){
      # option where comparing sum rather than fitting a line
      slope <- 1
      intercept <- 0
      res <- NA
      r2 <- NA
    }

    tibble::tibble(Forecast = unlist(col.for),
           Observed = unlist(col.obs)) %>%
      readr::write_csv(paste0(working_directory, "/", VarName,"_daily_downscale_data.csv"))

    if(PLOT){
      lims <- range(c(unlist(col.for),unlist(col.obs)))
      p <- tibble::tibble(Forecast = unlist(col.for),
                  Observed = unlist(col.obs)) %>%
        ggplot2::ggplot() +
        ggplot2::geom_point(aes(Forecast,Observed)) +
        ggplot2::geom_abline(aes(intercept = intercept, slope = slope), color = "red") +
        ggplot2::geom_abline(aes(intercept = 0, slope = 1), color = "black") +
        ggplot2::labs(title = VarName) +
        ggplot2::ylim(lims) +
        ggplot2::xlim(lims) +
        ggplot2::theme_bw()
      ggplot2::ggsave(filename = paste0(working_directory, "/", VarName,"_daily_downscale.pdf"), device = "pdf", plot = p, width = 5, height = 5, units = "in")
    }

    res.sd <- sd(unlist(res))

    return(list(intercept, slope, res.sd, r2, res))
  }

  n_vars <- nrow(VarInfo)
  VarNames <- VarInfo$VarNames
  df <- data.frame(matrix(NA, ncol = n_vars, nrow = 6))
  colnames(df) <- VarInfo$VarNames

  for (rowNum in 1:4){
    for(colNum in 1:n_vars){
      VarName <- VarNames[colNum]
      method <- VarInfo$debias_method[colNum]
      df[rowNum, VarName] <- get_coeff(col.obs = joined.data[,paste0(VarName,".obs")],
                                      col.for = joined.data[,paste0(VarName,".for")],
                                      method = method,
                                      PLOT,
                                      VarName,
                                      working_directory)[[rowNum]]
    }
  }

  df <- as.data.frame(df)
  row.names(df) <- c("intercept", "slope", "sd.res.daily", "r2.daily", "ds.res.hourly", "r2.hourly")

  ## covariance matrix
  df2 = NULL

  for(colNum in 1:n_vars){
    VarName <- VarNames[colNum]
    method <- VarInfo$debias_method[colNum]
    tmp <- as.numeric(unlist(get_coeff(col.obs = joined.data[,paste0(VarName,".obs")],
                                       col.for = joined.data[,paste0(VarName,".for")],
                                       method = method,
                                       PLOT = FALSE,
                                       VarName,
                                       working_directory)[5]))
    df2 <- cbind(df2, tmp)
  }

  noCovVarNames <- VarInfo %>% dplyr::filter(use_covariance == FALSE)
  noCovVarNames <- noCovVarNames$VarNames

  cov <- cov(df2)
  colnames(cov) <- VarNames
  rownames(cov) <- VarNames

  for(i in length(noCovVarNames)){

    cov[noCovVarNames[i],] <- 0
    cov[,noCovVarNames[i]] <- 0
  }

  return(list(df, cov))
}
