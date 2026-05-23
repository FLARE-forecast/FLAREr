`%||%` <- function(x, y) if (is.null(x)) y else x

#' Resolve the I/O backend for a FLARE run from `config$run_config`.
#'
#' Returns one of `"faasr"`, `"s3"`, `"local"`. Errors if
#' `use_faasr=TRUE` while `use_s3=FALSE`; FaaSr mode is always
#' cloud-backed. Falls back to `"s3"` with a warning when
#' `use_faasr=TRUE` but the FaaSr RPC stubs are not loaded into
#' `globalenv()`, i.e. when not running inside a FaaSr container.
#'
#' @param config FLAREr config list (must contain `run_config$use_s3`
#'   and optionally `run_config$use_faasr`).
#' @return Character scalar: `"faasr"`, `"s3"`, or `"local"`.
#' @keywords internal
flare_io_mode <- function(config) {
  use_s3    <- isTRUE(config$run_config$use_s3)
  use_faasr <- isTRUE(config$run_config$use_faasr)
  if (use_faasr && !use_s3) {
    stop("use_faasr=TRUE requires use_s3=TRUE. ",
         "Set use_s3: true in configure_run.yml.")
  }
  if (use_faasr) {
    if (!exists("faasr_get_file", envir = globalenv(), mode = "function")) {
      warning("use_faasr=TRUE but FaaSr RPC stubs are not loaded; ",
              "falling back to mode='s3'. ",
              "This usually means the code is running outside a FaaSr container.")
      return("s3")
    }
    return("faasr")
  }
  if (use_s3) "s3" else "local"
}

# Resolve a faasr_* RPC stub from globalenv at call time. The stubs are
# source()'d into globalenv by the FaaSr executor on action startup, so
# they don't exist at package-load time.
.flare_faasr <- function(name) {
  get(name, envir = globalenv(), mode = "function")
}

.flare_split_bucket   <- function(s) stringr::str_split_fixed(s, "/", n = 2)
.flare_split_endpoint <- function(s) stringr::str_split_fixed(s, "\\.", n = 2)

# Filesystem root used as the object store in mode="local". Honor an
# explicit lake_directory if set; otherwise derive from the
# qaqc_data_directory layout (<lake_directory>/targets/<site_id>).
.flare_local_root <- function(config) {
  lake_dir <- config$file_path$lake_directory
  if (!is.null(lake_dir)) return(lake_dir)
  qaqc <- config$file_path$qaqc_data_directory
  if (!is.null(qaqc)) return(dirname(dirname(qaqc)))
  getwd()
}

.flare_require_server <- function(server_name, config) {
  s3 <- config$s3[[server_name]]
  if (is.null(s3)) {
    stop(sprintf("config$s3$%s is not defined", server_name))
  }
  s3
}

#' Download a single object to a local path.
#'
#' Uniform wrapper that dispatches to FaaSr RPC, direct S3, or local
#' filesystem based on `flare_io_mode(config)`. Argument names and
#' defaults match the FaaSr R stub `faasr_get_file()`.
#'
#' @param local_file Local destination filename.
#' @param remote_file Remote object filename.
#' @param server_name Name of the DataStore (key under `config$s3`).
#' @param local_folder Local folder for `local_file`.
#' @param remote_folder Remote folder/prefix for `remote_file`.
#' @param config FLAREr config list.
#' @return `TRUE` on success (invisibly), or whatever the underlying
#'   stub returns under `mode="faasr"`.
#' @export
flare_get_file <- function(local_file, remote_file,
                           server_name   = "",
                           local_folder  = ".",
                           remote_folder = ".",
                           config) {
  mode <- flare_io_mode(config)

  if (mode == "faasr") {
    dst <- file.path(local_folder, local_file)
    dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
    res <- tryCatch(
      .flare_faasr("faasr_get_file")(
        local_file    = local_file,
        remote_file   = remote_file,
        server_name   = server_name,
        local_folder  = local_folder,
        remote_folder = remote_folder
      ),
      error = function(e) e
    )
    if (inherits(res, "error") || !file.exists(dst)) {
      stop(sprintf("Not Found (404): faasr_get_file failed for s3://%s/%s/%s",
                   server_name, remote_folder, remote_file))
    }
    return(invisible(res))
  }

  if (mode == "s3") {
    s3 <- .flare_require_server(server_name, config)
    dst <- file.path(local_folder, local_file)
    dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
    if (isTRUE(s3$anonymous)) {
      bp <- .flare_split_bucket(s3$bucket)
      endpoint <- s3$endpoint
      if (!grepl("^https?://", endpoint)) endpoint <- paste0("https://", endpoint)
      url <- file.path(endpoint, bp[1], remote_folder, remote_file)
      utils::download.file(url, destfile = dst, mode = "wb", quiet = TRUE)
      return(invisible(TRUE))
    }
    bp <- .flare_split_bucket(s3$bucket)
    ep <- .flare_split_endpoint(s3$endpoint)
    aws.s3::save_object(
      object    = file.path(remote_folder, remote_file),
      bucket    = bp[1],
      file      = dst,
      region    = ep[1],
      base_url  = ep[2],
      use_https = as.logical(Sys.getenv("USE_HTTPS"))
    )
    return(invisible(TRUE))
  }

  # mode == "local": files are already at their working-directory
  # location, so the remote-fetch step is a no-op.
  invisible(TRUE)
}

#' Upload a single local file as an object.
#'
#' @inheritParams flare_get_file
#' @export
flare_put_file <- function(local_file, remote_file,
                           server_name   = "",
                           local_folder  = ".",
                           remote_folder = ".",
                           config) {
  mode <- flare_io_mode(config)

  if (mode == "faasr") {
    return(.flare_faasr("faasr_put_file")(
      local_file    = local_file,
      remote_file   = remote_file,
      server_name   = server_name,
      local_folder  = local_folder,
      remote_folder = remote_folder
    ))
  }

  if (mode == "s3") {
    s3 <- .flare_require_server(server_name, config)
    bp <- .flare_split_bucket(s3$bucket)
    ep <- .flare_split_endpoint(s3$endpoint)
    aws.s3::put_object(
      file      = file.path(local_folder, local_file),
      object    = file.path(remote_folder, remote_file),
      bucket    = bp[1],
      region    = ep[1],
      base_url  = ep[2],
      use_https = as.logical(Sys.getenv("USE_HTTPS"))
    )
    return(invisible(TRUE))
  }

  # mode == "local": data is already at its working-directory
  # location; no upload needed.
  invisible(TRUE)
}

#' Delete a single remote object.
#'
#' @param remote_file Remote object filename.
#' @param server_name Name of the DataStore (key under `config$s3`).
#' @param remote_folder Remote folder/prefix.
#' @param config FLAREr config list.
#' @return `TRUE` on success (invisibly).
#' @export
flare_delete_file <- function(remote_file,
                              server_name   = "",
                              remote_folder = "",
                              config) {
  mode <- flare_io_mode(config)

  if (mode == "faasr") {
    return(.flare_faasr("faasr_delete_file")(
      remote_file   = remote_file,
      server_name   = server_name,
      remote_folder = remote_folder
    ))
  }

  if (mode == "s3") {
    s3 <- .flare_require_server(server_name, config)
    bp <- .flare_split_bucket(s3$bucket)
    ep <- .flare_split_endpoint(s3$endpoint)
    aws.s3::delete_object(
      object    = file.path(remote_folder, remote_file),
      bucket    = bp[1],
      region    = ep[1],
      base_url  = ep[2],
      use_https = as.logical(Sys.getenv("USE_HTTPS"))
    )
    return(invisible(TRUE))
  }

  # mode == "local": no remote object exists to delete.
  invisible(TRUE)
}

#' List object keys under a prefix.
#'
#' @param server_name Name of the DataStore (key under `config$s3`).
#' @param prefix Prefix to list under.
#' @param local_path Optional local filesystem directory to enumerate
#'   when `mode="local"`. When supplied AND mode resolves to "local",
#'   returns `list.files(local_path, recursive=TRUE, full.names=FALSE)`.
#'   When NULL (default) AND mode="local", returns `character(0)`, so
#'   remote-list callers iterate zero times in pure-local mode. Callers
#'   that need an existence check that works under BOTH local and remote
#'   modes (e.g. `get_run_config()`) should pass `local_path` so the
#'   local enumeration is meaningful.
#' @param config FLAREr config list.
#' @return Character vector of object keys / relative file paths.
#' @export
flare_get_folder_list <- function(server_name = "",
                                  prefix      = "",
                                  local_path  = NULL,
                                  config) {
  mode <- flare_io_mode(config)

  if (mode == "faasr") {
    return(.flare_faasr("faasr_get_folder_list")(
      server_name = server_name,
      prefix      = prefix
    ))
  }

  if (mode == "s3") {
    s3 <- .flare_require_server(server_name, config)
    bp <- .flare_split_bucket(s3$bucket)
    ep <- .flare_split_endpoint(s3$endpoint)
    files <- aws.s3::get_bucket(
      bucket    = bp[1],
      prefix    = prefix,
      region    = ep[1],
      base_url  = ep[2],
      use_https = as.logical(Sys.getenv("USE_HTTPS"))
    )
    keys <- vapply(files, `[[`, "", "Key", USE.NAMES = FALSE)
    return(keys[!grepl("/$", keys)])
  }

  # mode == "local": enumerate the supplied directory if given;
  # otherwise return empty so remote-list callers iterate zero times.
  if (!is.null(local_path) && dir.exists(local_path)) {
    return(list.files(local_path, recursive = TRUE, full.names = FALSE))
  }
  character(0)
}

#' Return an `arrow::s3_bucket()` (or local) handle for a partitioned
#' dataset under a server/prefix.
#'
#' Used for `arrow::open_dataset()` / `arrow::write_dataset()` calls.
#' Note that under any mode, `arrow` operations on the returned handle
#' transit directly between R and the storage backend — they do not
#' funnel through the FaaSr RPC server. FaaSr (when in `mode="faasr"`)
#' provides the credentials, but not the traffic path.
#'
#' @param server_name Name of the DataStore (key under `config$s3`).
#' @param faasr_prefix Optional sub-prefix appended to the bucket path.
#' @param local_path Filesystem path used in `mode="local"`. Caller is
#'   responsible for constructing this from any per-driver YAML
#'   conventions (e.g. `config$met$future_met_model`). Ignored in
#'   `mode="s3"`/`"faasr"`. If NULL in `mode="local"`, falls back to
#'   `<.flare_local_root(config)>/<server_name>/<faasr_prefix>`.
#' @param mode_override Optional explicit mode (`"local"`, `"s3"`, or
#'   `"faasr"`) that bypasses `flare_io_mode(config)`. Used when a
#'   per-driver YAML toggle (e.g. `config$met$future_met_use_s3`,
#'   `config$flows$use_flows_s3`) makes a specific driver local while
#'   global outputs still go to S3. NULL (default) uses config-driven
#'   dispatch.
#' @param config FLAREr config list.
#' @return An `arrow::s3_bucket()` handle (modes `s3`/`faasr`) or
#'   `arrow::SubTreeFileSystem` (mode `local`).
#' @export
flare_arrow_s3_bucket <- function(server_name   = "",
                                  faasr_prefix  = "",
                                  local_path    = NULL,
                                  mode_override = NULL,
                                  config) {
  mode <- if (!is.null(mode_override)) mode_override else flare_io_mode(config)

  if (mode == "faasr") {
    return(.flare_faasr("faasr_arrow_s3_bucket")(
      server_name  = server_name,
      faasr_prefix = faasr_prefix
    ))
  }

  if (mode == "s3") {
    s3 <- .flare_require_server(server_name, config)
    bp <- .flare_split_bucket(s3$bucket)
    bucket <- if (nzchar(faasr_prefix)) paste0(bp[1], "/", faasr_prefix) else bp[1]
    if (isTRUE(s3$anonymous)) {
      return(arrow::s3_bucket(
        bucket            = bucket,
        endpoint_override = s3$endpoint,
        anonymous         = TRUE
      ))
    }
    return(arrow::s3_bucket(
      bucket            = bucket,
      endpoint_override = s3$endpoint,
      access_key        = Sys.getenv("AWS_ACCESS_KEY_ID"),
      secret_key        = Sys.getenv("AWS_SECRET_ACCESS_KEY")
    ))
  }

  # mode == "local": honor an explicit local_path so caller-specific
  # path conventions (e.g. config$met$future_met_model) carry through;
  # otherwise fall back to the generic <root>/<server>/<prefix> layout.
  if (!is.null(local_path)) {
    return(arrow::SubTreeFileSystem$create(local_path))
  }
  arrow::SubTreeFileSystem$create(
    file.path(.flare_local_root(config), server_name, faasr_prefix)
  )
}
