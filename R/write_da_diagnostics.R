#' @title Write DA diagnostic tables to parquet files
#'
#' @description Combines the per-step diagnostic tibbles accumulated during
#'   \code{run_da_forecast} and writes five parquet files:
#'   \itemize{
#'     \item \code{da_filter_health.parquet} — NIS, DFS, inflation per step
#'     \item \code{da_innovations.parquet}   — per-obs innovation, NIS, influence
#'     \item \code{da_ensemble_spread.parquet} — prior/posterior mean & SD per state × depth
#'     \item \code{da_parameters.parquet}   — parameter evolution and bounds (if estimated)
#'     \item \code{da_ranks.parquet}         — observation rank within prior ensemble
#'   }
#'
#' @param da_forecast_output  list returned by \code{run_da_forecast}; must
#'   contain a \code{da_diagnostics} element (list of per-step tibble lists).
#' @param local_directory  path to write parquet files (created if needed)
#'
#' @return invisibly, the path to the output directory
#' @export
write_da_diagnostics <- function(da_forecast_output, local_directory) {

  steps <- da_forecast_output$da_diagnostics
  if (is.null(steps) || length(steps) == 0L) {
    message("write_da_diagnostics: no diagnostic steps to write")
    return(invisible(local_directory))
  }

  config    <- da_forecast_output$config
  site_id   <- config$location$site_id
  model_id  <- config$run_config$sim_name
  ref_date  <- lubridate::as_date(da_forecast_output$forecast_start_datetime)

  dir.create(local_directory, showWarnings = FALSE, recursive = TRUE)

  .bind_table <- function(key) {
    tbls <- Filter(Negate(is.null), lapply(steps, `[[`, key))
    if (length(tbls) == 0L) return(NULL)
    dplyr::bind_rows(tbls) |>
      dplyr::mutate(site_id  = site_id,
                    model_id = model_id,
                    reference_date = ref_date)
  }

  filter_health   <- .bind_table("filter_health")
  innovations     <- .bind_table("innovations")
  ensemble_spread <- .bind_table("ensemble_spread")
  ranks           <- .bind_table("ranks")
  parameters      <- .bind_table("parameters")

  if (!is.null(filter_health))
    arrow::write_parquet(filter_health,
                         file.path(local_directory, "da_filter_health.parquet"))

  if (!is.null(innovations))
    arrow::write_parquet(innovations,
                         file.path(local_directory, "da_innovations.parquet"))

  if (!is.null(ensemble_spread))
    arrow::write_parquet(ensemble_spread,
                         file.path(local_directory, "da_ensemble_spread.parquet"))

  if (!is.null(ranks))
    arrow::write_parquet(ranks,
                         file.path(local_directory, "da_ranks.parquet"))

  if (!is.null(parameters))
    arrow::write_parquet(parameters,
                         file.path(local_directory, "da_parameters.parquet"))

  cross_state <- .bind_table("cross_state")
  cross_depth <- .bind_table("cross_depth")

  if (!is.null(cross_state))
    arrow::write_parquet(cross_state,
                         file.path(local_directory, "da_cross_state.parquet"))

  if (!is.null(cross_depth))
    arrow::write_parquet(cross_depth,
                         file.path(local_directory, "da_cross_depth.parquet"))

  message(sprintf("DA diagnostics written to: %s", local_directory))
  invisible(local_directory)
}
