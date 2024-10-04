##' @title Generate diagnostic plot of FLARE output with observations
##' @details Function combines the netcdf output with the long-format observation file to produce a set of plots for each state variable, calibrated parameter, and diagnostic variable
##' @param targets_df dataframe; data frame containing all observations (targets)
##' @param forecast_df dataframe; data frame containing the forecast predictions
##' @param file_name string; name of pdf file that will be saved
##' @param plots_directory full path of directory where plot will be saved
##' @return None
##' @import dplyr
##' @import ggplot2
##' @importFrom lubridate as_datetime
##' @author Austin Delany
##' @keywords internal

plotting_general <- function(forecast_df,
                              targets_df,
                              file_name,
                              plots_directory){


  pdf_file_name <- file.path(plots_directory, file_name)

  combined_df <- left_join(forecast_df, targets_df, by = join_by(datetime, site_id, depth, variable))

  focal_depths_plotting <- unique(combined_df$depth)
  max_ensembles <- max(combined_df$parameter)

  focal_ensemebles <- 1:min(c(10, max_ensembles))

  if(length(focal_depths_plotting) < 4){
    plot_height <- 3
  }else{
    plot_height <- 8
  }

  pdf(pdf_file_name,width = 11, height = plot_height)

  state_depth_variables <- combined_df |>
    filter(variable_type == 'state',
           !(variable %in% c('secchi', 'depth', "ice_thickness"))) |>
    distinct(variable) |>
    pull(variable)

  ## BUILD DEPTH-SPECIFIC STATE VARIABLE PLOTS
  for (var in state_depth_variables){

    var_target_depths <- combined_df |>
      filter(variable == var,
             !is.na(depth)) |>
      distinct(depth) |>
      pull(depth)

    single_ensemble <- combined_df |>
      filter(variable == var,
             depth %in% var_target_depths,
             parameter %in% focal_ensemebles)

    if (length(var_target_depths) == 0){

      state_plot <- combined_df |>
        filter(variable == var,
               depth %in% var_target_depths) |>
        ggplot(aes(x = datetime)) +
        geom_line(aes(y = prediction, group = parameter), color = "gray") +
        geom_line(data = single_ensemble, aes(x = datetime, y = prediction, group = parameter)) +
        geom_vline(aes(xintercept = reference_datetime)) +
        theme_bw() +
        ggtitle(var) +
        theme(plot.title = element_text(hjust = 0.5))


    }else{

      obs <- combined_df |>
        filter(variable == var,
               depth %in% var_target_depths) |>
        distinct(datetime, site_id, depth, variable, observation)

      state_plot <- combined_df |>
        filter(variable == var,
               depth %in% var_target_depths) |>
        ggplot(aes(x = datetime)) +
        geom_line(aes(y = prediction, group = parameter), color = "gray") +
        geom_line(data = single_ensemble, aes(x = datetime, y = prediction, group = parameter)) +
        geom_point(data = obs, aes(x = datetime, y = observation), color = "red") +
        geom_vline(aes(xintercept = reference_datetime)) +
        theme_bw() +
        facet_wrap(~depth) +
        ggtitle(var) +
        theme(plot.title = element_text(hjust = 0.5))

    }

    plot(state_plot)
  }

  ## BUILD NON-DEPTH-SPECIFIC STATE VARIABLE PLOTS
  state_non_depth_variables <- combined_df |>
    filter(variable_type == 'state',
           variable %in% c('secchi', 'depth', 'ice_thickness')) |>
    distinct(variable) |>
    pull(variable)

  single_ensemble <- combined_df |>
    filter(variable %in% state_non_depth_variables,
           parameter %in% focal_ensemebles)


  obs <- combined_df |>
    filter(variable %in% state_non_depth_variables) |>
    distinct(datetime, site_id, depth, variable, observation)

  state_non_depth_plot <- combined_df |>
    filter(variable %in% state_non_depth_variables) |>
    #dplyr::filter(variable %in% c("temperature","secchi", "chla", "lw_factor") & (depth == 1.5 | is.na(depth))) |>
    ggplot(aes(x = datetime)) +
    geom_line(aes(y = prediction, group = parameter), color = "gray") +
    geom_line(data = single_ensemble, aes(x = datetime, y = prediction, group = parameter)) +
    geom_point(data = obs, aes(x = datetime, y = observation), color = "red") +
    geom_vline(aes(xintercept = reference_datetime)) +
    theme_bw() +
    facet_wrap(~variable, scales = "free_y")

  plot(state_non_depth_plot)


  ## CREATE PARAMETER PLOTS
  parameter_variables <- combined_df |>
    filter(variable_type == 'parameter') |>
    distinct(variable) |>
    pull(variable)

  single_ensemble <- combined_df |>
    filter(variable %in% parameter_variables,
           parameter %in% focal_ensemebles)

  parameter_plot <- combined_df |>
    filter(variable %in% parameter_variables) |>
    #left_join(targets_df) |>
    #dplyr::filter(variable %in% c("temperature","secchi", "chla", "lw_factor") & (depth == 1.5 | is.na(depth))) |>
    ggplot(aes(x = datetime)) +
    geom_line(aes(y = prediction, group = parameter), color = "gray") +
    geom_line(data = single_ensemble, aes(x = datetime, y = prediction, group = parameter)) +
    geom_vline(aes(xintercept = reference_datetime)) +
    theme_bw() +
    facet_wrap(~variable, scales = "free_y") +
    labs(y = "value")

  plot(parameter_plot)


  ## CREATE DIAGNOSTIC PLOTS
  diagnostic_variables <- combined_df |>
    filter(variable_type == 'diagnostic') |>
    distinct(variable) |>
    pull(variable)



  for (var in diagnostic_variables){

    diagnostic_target_depths <- combined_df |>
      filter(variable %in% diagnostic_variables) |>
      distinct(depth) |>
      pull(depth)

    single_ensemble <- combined_df |>
      filter(variable == var,
             depth %in% diagnostic_target_depths,
             parameter %in% focal_ensemebles)

    diagnostic_plot <- combined_df |>
      filter(variable == var,
             depth %in% diagnostic_target_depths) |>
      ggplot(aes(x = datetime)) +
      geom_line(aes(y = prediction, group = parameter), color = "gray") +
      geom_line(data = single_ensemble, aes(x = datetime, y = prediction, group = parameter)) +
      geom_vline(aes(xintercept = reference_datetime)) +
      theme_bw() +
      facet_wrap(~depth) +
      ggtitle(var) +
      theme(plot.title = element_text(hjust = 0.5))

    plot(diagnostic_plot)
  }

  dev.off()

  invisible(pdf_file_name)

}
