#' @title Render the DA diagnostics report
#'
#' @description Copies the built-in Quarto template to \code{lake_directory},
#'   renders it against the parquet files written by
#'   \code{\link{write_da_diagnostics}}, and returns the path to the HTML file.
#'   The temporary copy of the \code{.qmd} is removed after rendering.
#'
#' @param lake_directory  full path to the lake repository directory (same value
#'   passed to \code{\link{run_flare}})
#' @param diagnostics_dir directory containing the parquet files, relative to
#'   \code{lake_directory} (default: \code{"da_diagnostics"})
#' @param site_id   optional character; filter report to one site
#' @param model_id  optional character; filter report to one model / sim_name
#' @param output_file basename of the HTML output file
#'   (default: \code{"da_diagnostics_report.html"})
#' @param quiet logical; suppress quarto console output (default \code{TRUE})
#'
#' @return invisibly, the full path to the rendered HTML file
#' @export
render_da_diagnostics <- function(lake_directory,
                                  diagnostics_dir = "da_diagnostics",
                                  site_id         = NULL,
                                  model_id        = NULL,
                                  output_file     = "da_diagnostics_report.html",
                                  quiet           = TRUE) {

  if (!requireNamespace("quarto", quietly = TRUE)) {
    stop("Package 'quarto' is required. Install with: install.packages('quarto')")
  }

  template <- system.file("diagnostics", "da_diagnostics_report.qmd",
                           package = "FLAREr")
  if (!nzchar(template) || !file.exists(template)) {
    stop("Could not find da_diagnostics_report.qmd inside the FLAREr package.")
  }

  # Quarto renders output next to the input .qmd, so copy it to lake_directory.
  tmp_qmd <- file.path(lake_directory, "da_diagnostics_report.qmd")
  file.copy(template, tmp_qmd, overwrite = TRUE)
  on.exit(unlink(tmp_qmd), add = TRUE)

  quarto::quarto_render(
    input          = tmp_qmd,
    output_file    = output_file,
    execute_params = list(
      diagnostics_dir = diagnostics_dir,
      site_id         = site_id,
      model_id        = model_id
    ),
    quiet = quiet
  )

  out_path <- file.path(lake_directory, output_file)
  message("DA diagnostics report: ", out_path)
  invisible(out_path)
}
