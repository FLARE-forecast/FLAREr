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
    if (time_index <= 1) return(NULL)   # diagnostics array not yet populated
    diag_names <- config$output_settings$diagnostics_names
    diag_idx   <- which(diag_names == meta$model_variable)
    if (length(diag_idx) == 0) return(NULL)
    depth_idx  <- resolve_depth_index(meta$model_depth_m, config)
    return(diagnostics[diag_idx, time_index, depth_idx, ])
  }

  NULL
}
