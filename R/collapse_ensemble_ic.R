#' Copy ensemble member 1's GLM restart NetCDF to every other member
#'
#' Used when `config$uncertainty$initial_condition` is FALSE so that all members
#' begin the forecast from an identical GLM internal state (mixer state, water
#' quality variables not carried in the FLARE state vector, etc.).  Silently
#' does nothing when member 1 has no restart file yet.
#'
#' @param working_directory directory holding the per-member `<m>/` directories
#' @param nmembers number of ensemble members
#' @noRd
collapse_glm_restart_to_member1 <- function(working_directory, nmembers) {

  rst_name <- function(member_idx) {
    file.path(working_directory, member_idx,
              paste0("glm_restart_", member_idx, ".nc"))
  }

  src <- rst_name(1)
  if (!file.exists(src)) return(invisible(NULL))

  # Source and destinations are distinct paths, so no tempfile staging is
  # needed (unlike the particle-filter resample, where members swap files).
  for (m in seq_len(nmembers)[-1]) {
    file.copy(src, rst_name(m), overwrite = TRUE)
  }

  invisible(NULL)
}

#' Collapse the model state ensemble onto ensemble member 1 at one time step
#'
#' Overwrites the member dimension at time index `idx` with member 1's slice for
#' every state array that is propagated to the next time step, plus the output
#' diagnostics so the written output is internally consistent.  Parameters are
#' deliberately *not* collapsed: parameter spread is governed by
#' `config$uncertainty$parameter`.
#'
#' `diagnostics` / `diagnostics_daily` are `NA` (not arrays) when no diagnostics
#' are configured; those are passed through untouched.
#'
#' @param arrays named list of state arrays (see body for expected layouts)
#' @param idx time index to collapse
#' @noRd
#' @return `arrays` with the member dimension collapsed at `idx`
collapse_states_to_member1 <- function(arrays, idx) {

  # [time, state, height, ens]
  arrays$states_height[idx, , , ] <- arrays$states_height[idx, , , 1]
  # [time, state, depth, ens]
  arrays$states_depth[idx, , , ] <- arrays$states_depth[idx, , , 1]
  # [time, height, ens]
  arrays$model_internal_heights[idx, , ] <- arrays$model_internal_heights[idx, , 1]
  # [time, ens]
  arrays$lake_depth[idx, ] <- arrays$lake_depth[idx, 1]
  # [3, time, ens]
  arrays$snow_ice_thickness[, idx, ] <- arrays$snow_ice_thickness[, idx, 1]

  # [diag, time, depth, ens]
  if (is.array(arrays$diagnostics)) {
    arrays$diagnostics[, idx, , ] <- arrays$diagnostics[, idx, , 1]
  }
  # [diag, time, ens]
  if (is.array(arrays$diagnostics_daily)) {
    arrays$diagnostics_daily[, idx, ] <- arrays$diagnostics_daily[, idx, 1]
  }

  # Every particle is now identical, so weights must be uniform.
  if (!is.null(arrays$log_particle_weights)) {
    arrays$log_particle_weights[idx, ] <- log(1.0)
  }

  arrays
}
