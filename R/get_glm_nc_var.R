get_glm_nc_var <- function(nc, working_dir, z_out, vars_depth, vars_no_depth,
                           diagnostic_vars, diagnostics_daily_config)
{
  glm_nc <- nc

  # Read the full layer-count vector (1-D, one value per timestep) to locate
  # the final step and surface layer index.
  tallest_layer_all <- ncdf4::ncvar_get(glm_nc, "NS")
  final_time_step   <- length(tallest_layer_all)
  tallest_layer     <- tallest_layer_all[final_time_step]

  # Read only the final-timestep slice of a variable, regardless of how many
  # dimensions it has.  GLM always stores time as the last (unlimited) dimension
  # in the NetCDF file, which ncdf4 exposes as the last R index.  For variables
  # with extra leading dimensions (e.g. AED multi-group tracers stored as
  # [group, z, time]) the slice is flattened and the first `n` elements are
  # returned, matching the behaviour of the original
  #   matrix(ncvar_get(...), ncol = final_time_step)[1:n, final_time_step]
  # pattern.
  #
  # Some ncdf4 builds (and coordinate variables) do not populate $var for every
  # variable in the file, so `glm_nc$var[[name]]` can return NULL.  In that case
  # we fall back to the original full-array read to avoid a NULL-arithmetic error
  # inside rep().
  .last_ts <- function(var_name, n = NULL) {
    v     <- glm_nc$var[[var_name]]
    ndims <- if (!is.null(v)) length(v$dim) else 0L

    if (ndims >= 1L) {
      dim_sizes <- vapply(v$dim, function(d) d$len, integer(1L))
      start     <- c(rep(1L, ndims - 1L), final_time_step)
      count     <- c(dim_sizes[seq_len(ndims - 1L)], 1L)
      slice     <- as.vector(ncdf4::ncvar_get(glm_nc, var_name,
                                              start = start, count = count))
    } else {
      raw   <- ncdf4::ncvar_get(glm_nc, var_name)
      slice <- as.vector(matrix(as.vector(raw), ncol = final_time_step)[, final_time_step])
    }

    if (is.null(n)) slice else slice[seq_len(n)]
  }

  heights      <- .last_ts("z", tallest_layer)
  heights_surf <- heights[tallest_layer]

  snow      <- .last_ts("snow_thickness")
  ice_white <- .last_ts("white_ice_thickness")
  ice_blue  <- .last_ts("blue_ice_thickness")

  output <- array(NA, dim = c(tallest_layer, length(vars_depth)))
  for (v in seq_along(vars_depth)) {
    output[, v] <- .last_ts(vars_depth[v], tallest_layer)
  }
  output_no_depth <- NA

  if (length(diagnostic_vars) > 0) {
    diagnostics_output <- array(NA, dim = c(tallest_layer, length(diagnostic_vars)))
    for (v in seq_along(diagnostic_vars)) {
      diagnostics_output[, v] <- .last_ts(diagnostic_vars[v], tallest_layer)
    }
  } else {
    diagnostics_output <- NA
  }

  salt <- .last_ts("salt", tallest_layer)

  if (length(heights) == 1) {
    output             <- rbind(output, output)
    heights            <- c(heights / 2, heights)
    diagnostics_output <- rbind(diagnostics_output, diagnostics_output)
    salt               <- c(salt, salt)
  }

  if (length(diagnostics_daily_config$names) > 0) {
    # Full z array is only needed for depth-interpolated diagnostics_daily vars.
    # Defer the read until we confirm at least one such variable is requested.
    heights_all <- NULL

    diagnostics_daily_output <- array(NA, dim = c(length(diagnostics_daily_config$names)))
    for (v in seq_along(diagnostics_daily_config$names)) {
      if (tools::file_ext(diagnostics_daily_config$file[v]) == "csv") {
        diagnostics_daily_output[v] <- readr::read_csv(
          file.path(working_dir, diagnostics_daily_config$file[v]),
          show_col_types = FALSE
        ) |> dplyr::pull(diagnostics_daily_config$names[v])
      } else if (tools::file_ext(diagnostics_daily_config$file[v]) == "nc") {
        if (!is.na(diagnostics_daily_config$depth[v])) {
          # Depth interpolation requires heights at every timestep — read the
          # full z array once and reuse it for any subsequent vars that need it.
          if (is.null(heights_all)) {
            heights_all <- ncdf4::ncvar_get(glm_nc, "z")
          }
          time <- ncdf4::ncvar_get(glm_nc, "time")
          var  <- ncdf4::ncvar_get(glm_nc, diagnostics_daily_config$names[v])
          if (length(time) == 0) {
            stop("You requested a diagnostics_daily_config from the output.nc but
                 the nsave value in the base glm.nml only allows one output value
                 per day (thus can't calculate a daily statistic). Decrease the nsave value")
          }
          var2 <- rep(NA, length(time))
          for (t in seq_along(time)) {
            max_height <- max(heights_all[1:tallest_layer_all[t], t])
            depths     <- max_height - heights_all[1:tallest_layer_all[t], t]
            var2[t]    <- approx(depths, var[1:tallest_layer_all[t], t],
                                 diagnostics_daily_config$depth[v])$y
          }
        } else {
          var2 <- ncdf4::ncvar_get(glm_nc, diagnostics_daily_config$names[v])
        }

        if (stringr::str_detect(diagnostics_daily_config$save_names[v], "mean")) {
          diagnostics_daily_output[v] <- mean(var2, na.rm = TRUE)
        } else if (stringr::str_detect(diagnostics_daily_config$save_names[v], "max")) {
          diagnostics_daily_output[v] <- max(var2, na.rm = TRUE)
        } else if (stringr::str_detect(diagnostics_daily_config$save_names[v], "min")) {
          diagnostics_daily_output[v] <- min(var2, na.rm = TRUE)
        }
      }
    }
  } else {
    diagnostics_daily_output <- NA
  }

  return(list(
    output                   = output,
    output_no_depth          = output_no_depth,
    lake_depth               = heights_surf,
    heights                  = heights,
    snow_wice_bice           = c(snow, ice_white, ice_blue),
    salt                     = salt,
    diagnostics_output       = diagnostics_output,
    diagnostics_daily_output = diagnostics_daily_output
  ))
}
