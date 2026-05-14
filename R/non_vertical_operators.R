#' Registry of forward and inverse operators for non-vertical observations
#'
#' forward_fn: model-space value (e.g. Kd) -> obs space (e.g. secchi depth).
#'   This is what gets appended to the augmented state vector.
#'
#' inverse_fn: DA-updated augmented-state value -> value written back to model
#'   internals (state or diagnostic).  NULL means the observation is a pure
#'   diagnostic: the DA update propagates through ensemble covariance only and
#'   nothing needs to be written back (the model recomputes it next step).
#'
#' Variables not listed here get the default: identity forward, NULL inverse.
#'
#' @noRd
NON_VERTICAL_OPERATORS <- list(

  depth = list(
    forward_fn = identity,
    inverse_fn = identity   # write-back handled specially in apply_da_updates
  ),

  secchi = list(
    # Poole-Atkins: Zsd = 1.7 / Kd
    forward_fn = function(kd)  1.7 / kd,
    # updated secchi -> Kd written back to the extc_coeff diagnostic
    inverse_fn = function(val) 1.7 / pmax(val, 1e-6)
  )
)


#' Retrieve forward/inverse operators for a non-vertical observation variable
#'
#' @param var_name character; the state_names_obs value (e.g. "secchi")
#' @return list with elements forward_fn and inverse_fn
#' @noRd
get_non_vertical_operator <- function(var_name) {
  if (var_name %in% names(NON_VERTICAL_OPERATORS)) {
    NON_VERTICAL_OPERATORS[[var_name]]
  } else {
    list(forward_fn = identity, inverse_fn = NULL)
  }
}


#' Convert model_depth_m config value to a layer index
#'
#' @param model_depth_m value from observations_config: NA, "bottom", or numeric metres
#' @param config FLARE configuration list (needs model_settings$modeled_depths)
#' @return integer layer index
#' @noRd
resolve_depth_index <- function(model_depth_m, config) {
  if (is.na(model_depth_m)) {
    return(1L)
  }
  if (identical(model_depth_m, "bottom") || model_depth_m == "bottom") {
    return(length(config$model_settings$modeled_depths))
  }
  which.min(abs(config$model_settings$modeled_depths - as.numeric(model_depth_m)))
}


#' Extract the ensemble vector for one non-vertical variable from model state/diagnostics
#'
#' @param var_name character; state_names_obs identifier
#' @param meta list; one element of obs_non_vertical (has model_source, model_variable, model_depth_m)
#' @param states_depth array [nstates, ndepths, nmembers] of current model states
#' @param diagnostics array [ndiag, ntime, ndepths, nmembers] of model diagnostics
#' @param lake_depth numeric vector [nmembers] of current lake depth
#' @param states_config states configuration data frame
#' @param config FLARE configuration list
#' @param time_index integer current time step index
#' @return numeric vector [nmembers], or NULL if value cannot yet be extracted
#' @noRd
extract_modeled_non_vertical <- function(var_name, meta, states_depth, diagnostics,
                                         lake_depth, states_config, config, time_index) {
  if (meta$model_source == "state") {
    if (var_name == "depth") {
      return(lake_depth)
    }
    state_idx <- which(states_config$state_names == meta$model_variable)
    depth_idx <- resolve_depth_index(meta$model_depth_m, config)
    return(states_depth[state_idx, depth_idx, ])
  }

  if (meta$model_source == "diagnostic") {
    diag_names <- config$output_settings$diagnostics_names
    diag_idx   <- which(diag_names == meta$model_variable)
    if (length(diag_idx) == 0) return(NULL)
    depth_idx  <- resolve_depth_index(meta$model_depth_m, config)
    return(diagnostics[diag_idx, time_index, depth_idx, ])
  }

  NULL
}


#' Validate that all diagnostic variables in non_vertical_noise_config exist in
#' the appropriate output_settings list in configure_flare.yml
#'
#' model_source = "diagnostic"       -> must be in output_settings$diagnostics_names
#' model_source = "diagnostic_daily" -> must be in output_settings$diagnostics_daily$names
#'
#' @param non_vertical_noise_config data frame from non_vertical_noise_config.csv; NULL is allowed
#' @param config FLARE configuration list
#' @return invisible NULL; stops with an informative message if validation fails
#' @noRd
validate_non_vertical_noise_config <- function(non_vertical_noise_config, config) {
  if (is.null(non_vertical_noise_config) || nrow(non_vertical_noise_config) == 0) {
    return(invisible(NULL))
  }

  diag_rows <- non_vertical_noise_config[non_vertical_noise_config$model_source == "diagnostic", ]
  if (nrow(diag_rows) > 0) {
    known   <- config$output_settings$diagnostics_names
    missing <- diag_rows$model_variable[!diag_rows$model_variable %in% known]
    if (length(missing) > 0) {
      stop(
        "non_vertical_noise_config.csv references diagnostic variable(s) not in ",
        "output_settings$diagnostics_names in configure_flare.yml: ",
        paste(missing, collapse = ", "),
        ". Add them to diagnostics_names or use model_source = 'diagnostic_daily' ",
        "if they come from daily GLM output files."
      )
    }
  }

  daily_rows <- non_vertical_noise_config[non_vertical_noise_config$model_source == "diagnostic_daily", ]
  if (nrow(daily_rows) > 0) {
    known_daily   <- config$output_settings$diagnostics_daily$names
    missing_daily <- daily_rows$model_variable[!daily_rows$model_variable %in% known_daily]
    if (length(missing_daily) > 0) {
      stop(
        "non_vertical_noise_config.csv references diagnostic_daily variable(s) not in ",
        "output_settings$diagnostics_daily$names in configure_flare.yml: ",
        paste(missing_daily, collapse = ", "),
        ". Add them to diagnostics_daily$names or check the model_source value."
      )
    }
  }

  invisible(NULL)
}


#' Apply per-member process noise to non-vertical model variables
#'
#' Runs inside the per-ensemble-member loop after the model output has been
#' stored.  State-source variables (e.g. lake_depth) are perturbed in place;
#' diagnostic-source variables are perturbed directly in the diagnostics array
#' so the noise is reflected in the augmented state vector at the same time
#' step rather than lagging one step.
#'
#' @param non_vertical_noise_config data frame from non_vertical_noise_config.csv;
#'   columns: model_variable, model_source, model_depth_m, process_noise_sd.
#'   Supported model_source values: "state", "diagnostic", "diagnostic_daily".
#'   NULL or zero-row data frame means no noise is applied.
#' @param lake_depth_m scalar; lake depth for this member at this time step
#' @param diagnostics_slice matrix [ndiag, ndepths] for this member and time step
#' @param diagnostics_daily_slice numeric vector [ndiag_daily] for this member
#'   and time step; NULL if no diagnostics_daily are configured
#' @param config FLARE configuration list
#' @return named list with elements \code{lake_depth_m}, \code{diagnostics_slice},
#'   and \code{diagnostics_daily_slice}, all possibly updated
#' @noRd
apply_non_vertical_process_noise <- function(non_vertical_noise_config,
                                              lake_depth_m,
                                              diagnostics_slice,
                                              diagnostics_daily_slice = NULL,
                                              config) {
  if (is.null(non_vertical_noise_config) || nrow(non_vertical_noise_config) == 0) {
    return(list(lake_depth_m         = lake_depth_m,
                diagnostics_slice     = diagnostics_slice,
                diagnostics_daily_slice = diagnostics_daily_slice))
  }

  # Ensure diagnostics_slice is always [ndiag, ndepths]; when ndiag == 1 R
  # drops the first dimension during array indexing, yielding a plain vector.
  ndiag <- length(config$output_settings$diagnostics_names)
  if (!is.null(diagnostics_slice) && is.null(dim(diagnostics_slice))) {
    diagnostics_slice <- matrix(diagnostics_slice, nrow = ndiag)
  }

  for (k in seq_len(nrow(non_vertical_noise_config))) {
    nvc      <- non_vertical_noise_config[k, ]
    noise_sd <- nvc$process_noise_sd
    if (is.na(noise_sd)) next

    if (nvc$model_source == "state" && nvc$model_variable == "lake_depth") {
      lake_depth_m <- rnorm(1, lake_depth_m, noise_sd)

    } else if (nvc$model_source == "diagnostic" && !is.null(diagnostics_slice)) {
      diag_idx  <- which(config$output_settings$diagnostics_names == nvc$model_variable)
      depth_idx <- resolve_depth_index(nvc$model_depth_m, config)
      if (length(diag_idx) > 0) {
        diagnostics_slice[diag_idx, depth_idx] <- rnorm(
          1, diagnostics_slice[diag_idx, depth_idx], noise_sd
        )
      }

    } else if (nvc$model_source == "diagnostic_daily" && !is.null(diagnostics_daily_slice)) {
      diag_idx <- which(config$output_settings$diagnostics_daily$names == nvc$model_variable)
      if (length(diag_idx) > 0) {
        diagnostics_daily_slice[diag_idx] <- rnorm(1, diagnostics_daily_slice[diag_idx], noise_sd)
      }
    }
  }

  list(lake_depth_m          = lake_depth_m,
       diagnostics_slice      = diagnostics_slice,
       diagnostics_daily_slice = diagnostics_daily_slice)
}
