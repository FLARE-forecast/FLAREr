##' @title Generate diagnostic plot of FLARE output with observations
##' @details Function combines the netcdf output with the long-format observation file to produce a set of plots for each state variable, calibrated parameter, and diagnostic variable
##' @param file_name string; full path to file containing plotting output
##' @param targets_df dataframe; data frame containing all observations (targets)
##' @param forecast_df dataframe; data frame containing the forecast predictions
##' @return None
##' @export
##' @import dplyr
##' @import ggplot2
##' @importFrom lubridate as_datetime
##' @author Austin Delany
##' @examples

plotting_general <- function(forecast_df,
                               targets_df,
                               file_name){

  pdf_file_name <- paste0(tools::file_path_sans_ext(file_name),".pdf")

  focal_depths_plotting <- unique(forecast_df$depth)

  if(length(focal_depths_plotting) < 4){
    plot_height <- 3
  }else{
    plot_height <- 8
  }

  pdf(pdf_file_name,width = 11, height = plot_height)

  state_depth_variables <- forecast_df |>
    filter(variable_type == 'state',
           !(variable %in% c('secchi', 'depth', "ice_thickness"))) |>
    distinct(variable) |>
    pull(variable)

  ## BUILD DEPTH-SPECIFIC STATE VARIABLE PLOTS
  for (var in state_depth_variables){
    message(var)

    var_target_depths <- targets_df |>
      filter(variable == var,
             !is.na(depth)) |>
      distinct(depth) |>
      pull(depth)

    if (length(var_target_depths) == 0){
      state_plot <- forecast_df |>
        filter(variable == var) |>
        ggplot(aes(x = datetime)) +
        geom_line(aes(y = prediction, group = parameter)) +
        geom_vline(aes(xintercept = reference_datetime)) +
        theme_bw() +
        labs(y = "forecast") +
        ggtitle(var) +
        theme(plot.title = element_text(hjust = 0.5))

    }else{
      state_plot <- forecast_df |>
        filter(variable == var,
               depth %in% var_target_depths) |>
        left_join(targets_df) |>
        ggplot(aes(x = datetime)) +
        geom_line(aes(y = prediction, group = parameter)) +
        geom_point(aes(y = observation), color = "red") +
        geom_vline(aes(xintercept = reference_datetime)) +
        theme_bw() +
        facet_wrap(~depth, scales = "free") +
        labs(y = "forecast") +
        ggtitle(var) +
        theme(plot.title = element_text(hjust = 0.5))

    }

    plot(state_plot)
  }

  ## BUILD NON-DEPTH-SPECIFIC STATE VARIABLE PLOTS
  state_non_depth_variables <- forecast_df |>
    filter(variable_type == 'state',
           variable %in% c('secchi', 'depth', 'ice_thickness')) |>
    distinct(variable) |>
    pull(variable)

  state_non_depth_plot <- forecast_df |>
    filter(variable == state_non_depth_variables) |>
    left_join(targets_df) |>
    #dplyr::filter(variable %in% c("temperature","secchi", "chla", "lw_factor") & (depth == 1.5 | is.na(depth))) |>
    ggplot(aes(x = datetime)) +
    geom_line(aes(y = prediction, group = parameter)) +
    geom_point(aes(y = observation), color = "red") +
    geom_vline(aes(xintercept = reference_datetime)) +
    theme_bw() +
    facet_wrap(~variable, scales = "free") +
    labs(y = "forecast")

  plot(state_non_depth_plot)


  ## CREATE PARAMETER PLOTS
  parameter_variables <- forecast_df |>
    filter(variable_type == 'parameter') |>
    distinct(variable) |>
    pull(variable)

  parameter_plot <- forecast_df |>
    filter(variable == parameter_variables) |>
    #left_join(targets_df) |>
    #dplyr::filter(variable %in% c("temperature","secchi", "chla", "lw_factor") & (depth == 1.5 | is.na(depth))) |>
    ggplot(aes(x = datetime)) +
    geom_line(aes(y = prediction, group = parameter)) +
    #geom_point(aes(y = observation), color = "red") +
    geom_vline(aes(xintercept = reference_datetime)) +
    theme_bw() +
    facet_wrap(~variable, scales = "free") +
    labs(y = "forecast")

  plot(parameter_plot)


  ## CREATE DIAGNOSTIC PLOTS
  diagnostic_variables <- forecast_df |>
    filter(variable_type == 'diagnostic') |>
    distinct(variable) |>
    pull(variable)

  for (var in diagnostic_variables){
    message(var)

    diagnostic_target_depths <- targets_df |>
      filter(variable %in% state_depth_variables,
             !is.na(depth)) |>
      distinct(depth) |>
      pull(depth)

    diagnostic_plot <- forecast_df |>
      filter(variable == var,
             depth %in% diagnostic_target_depths) |>
      ggplot(aes(x = datetime)) +
      geom_line(aes(y = prediction, group = parameter)) +
      geom_vline(aes(xintercept = reference_datetime)) +
      theme_bw() +
      facet_wrap(~depth, scales = "free") +
      labs(y = "forecast") +
      ggtitle(var) +
      theme(plot.title = element_text(hjust = 0.5))

    plot(diagnostic_plot)
  }

  dev.off()

  invisible(pdf_file_name)

}
