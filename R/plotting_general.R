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

  focal_ensembles <- 1:min(c(10, max_ensembles))

  # Extract once; passing via aes() inherits group=parameter and triggers the
  # "each group consists of only one observation" geom_line warning.
  ref_datetimes <- unique(combined_df$reference_datetime)

  if(length(focal_depths_plotting) < 4){
    plot_height <- 3
  }else{
    plot_height <- 8
  }

  pdf(pdf_file_name,width = 11, height = plot_height)

  # Pre-split by variable_type to avoid repeated full-table scans in each section
  state_df      <- combined_df |> filter(variable_type == 'state')
  parameter_df  <- combined_df |> filter(variable_type == 'parameter')
  diagnostic_df <- combined_df |> filter(variable_type == 'diagnostic')

  # Variables with at least one non-NA depth are depth-specific; those with
  # only NA depths (secchi, lake depth, ice thickness, etc.) are non-depth.
  vars_with_depth <- state_df |>
    filter(!is.na(depth)) |>
    distinct(variable) |>
    pull(variable)

  state_depth_variables <- state_df |>
    filter(variable %in% vars_with_depth) |>
    distinct(variable) |>
    pull(variable)

  ## BUILD DEPTH-SPECIFIC STATE VARIABLE PLOTS
  for (var in state_depth_variables){

    var_df <- state_df |> filter(variable == var)

    var_target_depths <- var_df |>
      filter(!is.na(depth)) |>
      distinct(depth) |>
      pull(depth)

    var_depth_df    <- var_df |> filter(depth %in% var_target_depths)
    single_ensemble <- var_depth_df |> filter(parameter %in% focal_ensembles)

    if (length(var_target_depths) == 0){

      state_plot <- var_depth_df |>
        ggplot(aes(x = datetime)) +
        geom_line(aes(y = prediction, group = parameter), color = "gray") +
        geom_line(data = single_ensemble, aes(x = datetime, y = prediction, group = parameter)) +
        geom_vline(xintercept = ref_datetimes) +
        theme_bw() +
        ggtitle(var) +
        theme(plot.title = element_text(hjust = 0.5))

    }else{

      obs <- var_depth_df |>
        distinct(datetime, site_id, depth, variable, observation, up95, low95)

      state_plot <- var_depth_df |>
        ggplot(aes(x = datetime)) +
        geom_line(aes(y = prediction, group = parameter), color = "gray") +
        geom_line(data = single_ensemble, aes(x = datetime, y = prediction, group = parameter)) +
        geom_point(data = obs, aes(x = datetime, y = observation), color = "red") +
        geom_errorbar(data = obs, aes(x = datetime, ymin = low95, ymax = up95), color = "red") +
        geom_vline(xintercept = ref_datetimes) +
        theme_bw() +
        facet_wrap(~depth) +
        ggtitle(var) +
        theme(plot.title = element_text(hjust = 0.5))

    }

    suppressWarnings(plot(state_plot))
  }

  ## BUILD NON-DEPTH-SPECIFIC STATE VARIABLE PLOTS
  state_non_depth_variables <- state_df |>
    filter(!(variable %in% vars_with_depth)) |>
    distinct(variable) |>
    pull(variable)

  if (length(state_non_depth_variables) > 0) {

    non_depth_df    <- state_df |> filter(variable %in% state_non_depth_variables)
    single_ensemble <- non_depth_df |> filter(parameter %in% focal_ensembles)

    obs <- non_depth_df |>
      distinct(datetime, site_id, depth, variable, observation, up95, low95)

    state_non_depth_plot <- non_depth_df |>
      ggplot(aes(x = datetime)) +
      geom_line(aes(y = prediction, group = parameter), color = "gray") +
      geom_line(data = single_ensemble, aes(x = datetime, y = prediction, group = parameter)) +
      geom_point(data = obs, aes(x = datetime, y = observation), color = "red") +
      geom_errorbar(data = obs, aes(x = datetime, ymin = low95, ymax = up95), color = "red") +
      geom_vline(xintercept = ref_datetimes) +
      theme_bw() +
      facet_wrap(~variable, scales = "free_y")

    suppressWarnings(plot(state_non_depth_plot))

  }

  ## CREATE PARAMETER PLOTS
  parameter_variables <- parameter_df |>
    distinct(variable) |>
    pull(variable)

  if(length(parameter_variables) > 0){

    single_ensemble <- parameter_df |> filter(parameter %in% focal_ensembles)

    parameter_plot <- parameter_df |>
      ggplot(aes(x = datetime)) +
      geom_line(aes(y = prediction, group = parameter), color = "gray") +
      geom_line(data = single_ensemble, aes(x = datetime, y = prediction, group = parameter)) +
      geom_vline(xintercept = ref_datetimes) +
      theme_bw() +
      facet_wrap(~variable, scales = "free_y") +
      labs(y = "value")

    suppressWarnings(plot(parameter_plot))

  }

  ## CREATE DIAGNOSTIC PLOTS
  diagnostic_variables <- diagnostic_df |>
    distinct(variable) |>
    pull(variable)

  if (length(diagnostic_variables) > 0) {

    # Compute depths once outside the loop rather than once per variable
    diagnostic_target_depths <- diagnostic_df |>
      distinct(depth) |>
      pull(depth)

    for (var in diagnostic_variables){

      var_diag_df     <- diagnostic_df |> filter(variable == var, depth %in% diagnostic_target_depths)
      single_ensemble <- var_diag_df |> filter(parameter %in% focal_ensembles)

      diagnostic_plot <- var_diag_df |>
        ggplot(aes(x = datetime)) +
        geom_line(aes(y = prediction, group = parameter), color = "gray") +
        geom_line(data = single_ensemble, aes(x = datetime, y = prediction, group = parameter)) +
        geom_vline(xintercept = ref_datetimes) +
        theme_bw() +
        facet_wrap(~depth) +
        ggtitle(var) +
        theme(plot.title = element_text(hjust = 0.5))

      suppressWarnings(plot(diagnostic_plot))
    }

  }

  dev.off()

  invisible(pdf_file_name)

}
