#' Read AED2 module initial values from an aed2.nml file.
#'
#' Looks up the `*_initial` field in each AED module section for the standard
#' FLARE WQ variable names, and reads phytoplankton initial values from the
#' CSV database referenced by `aed_phytoplankton$dbase`.
#'
#' @param aed2_nml_file Path to an aed2.nml file.
#' @return Named list mapping FLARE state name -> scalar initial value.
#' @importFrom utils read.csv
#' @importFrom stats setNames
#' @keywords internal
read_aed_initial_values <- function(aed2_nml_file) {

  aed <- read_nml(aed2_nml_file)

  # Map FLARE variable name -> (aed2.nml section, field name)
  map <- list(
    OXY_oxy  = list(s = "aed_oxygen",        f = "oxy_initial"),
    CAR_dic  = list(s = "aed_carbon",         f = "dic_initial"),
    CAR_ch4  = list(s = "aed_carbon",         f = "ch4_initial"),
    SIL_rsi  = list(s = "aed_silica",         f = "rsi_initial"),
    NIT_amm  = list(s = "aed_nitrogen",       f = "amm_initial"),
    NIT_nit  = list(s = "aed_nitrogen",       f = "nit_initial"),
    PHS_frp  = list(s = "aed_phosphorus",     f = "frp_initial"),
    OGM_doc  = list(s = "aed_organic_matter", f = "doc_initial"),
    OGM_docr = list(s = "aed_organic_matter", f = "docr_initial"),
    OGM_poc  = list(s = "aed_organic_matter", f = "poc_initial"),
    OGM_don  = list(s = "aed_organic_matter", f = "don_initial"),
    OGM_donr = list(s = "aed_organic_matter", f = "donr_initial"),
    OGM_pon  = list(s = "aed_organic_matter", f = "pon_initial"),
    OGM_dop  = list(s = "aed_organic_matter", f = "dop_initial"),
    OGM_dopr = list(s = "aed_organic_matter", f = "dopr_initial"),
    OGM_pop  = list(s = "aed_organic_matter", f = "pop_initial")
  )

  out <- list()
  for (nm in names(map)) {
    entry <- map[[nm]]
    val   <- tryCatch(aed[[entry$s]][[entry$f]], error = function(e) NULL)
    if (!is.null(val)) out[[nm]] <- val
  }

  # Noncohesive sediment: NCS_ss1, NCS_ss2, ... (one per size class)
  noncoh <- aed$aed_noncohesive
  if (!is.null(noncoh$num_ss) && !is.null(noncoh$ss_initial)) {
    for (i in seq_len(noncoh$num_ss)) {
      out[[paste0("NCS_ss", i)]] <- noncoh$ss_initial[i]
    }
  }

  # Phytoplankton: initial values live in the CSV referenced by
  # aed_phytoplankton$dbase. Row 'p_name' lists group names (cyano,
  # green, diatom, ...), row 'p_initial' lists initial biomass values.
  phyto_dbase <- aed$aed_phytoplankton$dbase
  if (!is.null(phyto_dbase)) {
    phyto_dbase <- trimws(gsub("'", "", phyto_dbase))
    phyto_csv   <- file.path(dirname(aed2_nml_file), phyto_dbase)
    if (file.exists(phyto_csv)) {
      tbl      <- read.csv(phyto_csv, header = FALSE, stringsAsFactors = FALSE)
      row_keys <- trimws(gsub("'", "", tbl[, 1]))
      name_row <- which(row_keys == "p_name")
      init_row <- which(row_keys == "p_initial")
      if (length(name_row) == 1 && length(init_row) == 1) {
        p_names <- trimws(gsub("'", "", as.character(tbl[name_row, -1])))
        p_inits <- suppressWarnings(as.numeric(tbl[init_row, -1]))
        for (j in seq_along(p_names)) {
          if (!is.na(p_inits[j])) {
            out[[paste0("PHY_", p_names[j])]] <- p_inits[j]
          }
        }
      }
    }
  }

  out
}


#' Update states_to_obs_mapping_1 for PHY_ variables in a states_config data frame.
#'
#' Reads Xcc (carbon-to-chlorophyll ratio) from the phytoplankton parameter
#' database referenced by aed_phytoplankton$dbase in the supplied aed2.nml, then
#' sets states_to_obs_mapping_1 = 12.0 / Xcc for each PHY_ row.  This matches
#' the AED2 conversion chla = (carbon_mmolC_m3 / Xcc) * 12.0, so the multiplier
#' from carbon state to chla observation is 12/Xcc.
#'
#' @param states_config  Data frame with a state_names column and a
#'   states_to_obs_mapping_1 column (as returned by read.csv on states_config.csv).
#' @param nml_path  Path to an aed2.nml file.
#' @return The modified states_config data frame.
#' @keywords internal
update_phy_states_obs_mapping <- function(states_config, nml_path) {

  aed        <- read_nml(nml_path)
  phyto_blk  <- aed$aed_phytoplankton
  if (is.null(phyto_blk)) stop("No aed_phytoplankton block found in ", nml_path)

  # Locate and read the phytoplankton parameter database CSV.
  dbase_file <- trimws(gsub("'", "", phyto_blk$dbase))
  dbase_path <- file.path(dirname(nml_path), dbase_file)
  if (!file.exists(dbase_path)) stop("Phytoplankton database not found: ", dbase_path)

  tbl      <- read.csv(dbase_path, header = FALSE, stringsAsFactors = FALSE)
  row_keys <- trimws(gsub("'", "", tbl[, 1]))

  name_row <- which(row_keys == "p_name")
  xcc_row  <- which(row_keys == "Xcc")
  if (length(name_row) != 1) stop("Expected exactly one 'p_name' row in ", dbase_path)
  if (length(xcc_row)  != 1) stop("Expected exactly one 'Xcc' row in ", dbase_path)

  db_names <- trimws(gsub("'", "", as.character(tbl[name_row, -1])))
  db_xcc   <- suppressWarnings(as.numeric(tbl[xcc_row, -1]))

  # the_phytos selects which database columns (1-indexed) are active.
  the_phytos <- phyto_blk$the_phytos
  if (is.null(the_phytos)) the_phytos <- seq_along(db_names)

  xcc_map <- setNames(db_xcc[the_phytos], db_names[the_phytos])

  phy_rows <- grep("^PHY_", states_config$state_names)
  for (i in phy_rows) {
    suffix <- sub("^PHY_", "", states_config$state_names[i])
    if (suffix %in% names(xcc_map)) {
      states_config$states_to_obs_mapping_1[i] <- 12.0 / xcc_map[[suffix]]
    } else {
      warning(sprintf("No Xcc entry for '%s' (looked for '%s' in %s)",
                       states_config$state_names[i], suffix, dbase_file))
    }
  }

  states_config
}


#' Read GLM nml initial profile fields and return a list in the format of
#' generate_initial_conditions().
#'
#' The GLM nml stores the_heights, the_temps, the_sals, and wq_init_vals in
#' bottom-to-top (ascending height) order.  wq_init_vals is a flat vector
#' where the first num_heights values belong to `wq_names[1]`, the next
#' num_heights values to `wq_names[2]`, etc. (same ordering written by
#' run_model.R).  This function reverses that encoding back into the depth-
#' major, surface-first layout used by the FLARE states array.
#'
#' For any state in states_config that is not found in the glm nml (e.g. a
#' WQ variable absent from wq_names), the function optionally falls back to the
#' scalar initial value from aed2_nml_file, replicated uniformly across all
#' modeled depths.
#'
#' @param nml_file       Path to a GLM glm3.nml file.
#' @param states_config  Data frame with at least a `state_names` column,
#'   matching the FLARE states_config.csv convention (temp, salt, then WQ vars).
#' @param modeled_depths Numeric vector of depths from the surface (m,
#'   positive downward) at which FLARE tracks states.
#' @param max_model_layers Integer. Defaults to length(modeled_depths).
#' @param nmembers       Number of ensemble members. Defaults to 1; the same
#'   interpolated values are replicated across all members.
#' @param aed2_nml_file  Optional path to an aed2.nml file.  When supplied,
#'   any state absent from the glm nml is filled with the corresponding
#'   `*_initial` value from the AED2 module configuration.
#' @return A list with elements `states`, `pars`, and `aux_states_init`
#'   matching the structure returned by generate_initial_conditions().
#' @keywords internal
nml_to_initial_conditions <- function(nml_file,
                                      states_config,
                                      modeled_depths,
                                      max_model_layers = NULL,
                                      nmembers = 1L,
                                      aed2_nml_file = NULL) {

  nml <- read_nml(nml_file)
  ip  <- nml$init_profiles

  the_heights  <- ip$the_heights          # ascending from bottom
  the_temps    <- ip$the_temps
  the_sals     <- ip$the_sals
  lake_depth   <- ip$lake_depth
  wq_names     <- ip$wq_names             # character vector or NULL
  wq_init_vals <- ip$wq_init_vals         # flat numeric vector or NULL

  # read_nml may return wq_names as a single comma-separated string
  # (e.g. "'OXY_oxy','CAR_dic',...") instead of a character vector.
  if (length(wq_names) == 1 && grepl(",", wq_names)) {
    wq_names <- trimws(gsub("'", "", strsplit(wq_names, ",")[[1]]))
  }

  # AED2 fallback scalar initial values (used when a state is
  # absent from the nml).
  aed2_init <- if (!is.null(aed2_nml_file)) {
    read_aed_initial_values(aed2_nml_file)
  } else {
    list()
  }

  num_heights     <- length(the_heights)
  ndepths_modeled <- length(modeled_depths)
  if (is.null(max_model_layers)) max_model_layers <- ndepths_modeled
  nstates <- nrow(states_config)

  # Convert GLM heights (from bottom, ascending) to depths from the surface,
  # then sort ascending so approx() receives a monotone x.
  the_depths_desc <- lake_depth - the_heights
  ord             <- order(the_depths_desc)
  x_asc           <- the_depths_desc[ord]

  # Build a named lookup: state name -> list(x, y) or plain vector
  nml_vals <- list(
    temp = the_temps[ord],
    salt = the_sals[ord]
  )

  n_wq <- length(wq_names)
  if (n_wq > 0 && !is.null(wq_init_vals) && length(wq_init_vals) > 0) {
    n_h_wq <- length(wq_init_vals) / n_wq
    if (abs(n_h_wq - round(n_h_wq)) > 1e-9) {
      warning(sprintf(
        paste0("length(wq_init_vals) [%d] is not evenly divisible ",
               "by length(wq_names) [%d]; WQ values may be misaligned."),
        length(wq_init_vals), n_wq
      ))
    }
    n_h_wq <- round(n_h_wq)
    wq_mat <- matrix(wq_init_vals, nrow = n_h_wq, ncol = n_wq)

    x_wq_desc <- lake_depth - the_heights[seq_len(n_h_wq)]
    ord_wq    <- order(x_wq_desc)
    x_wq_asc  <- x_wq_desc[ord_wq]

    for (wq in seq_len(n_wq)) {
      nml_vals[[wq_names[wq]]] <- list(x = x_wq_asc, y = wq_mat[ord_wq, wq])
    }
  }

  # Interpolate every state to modeled_depths; fall back to aed2
  # scalar when missing.
  states <- array(NA_real_, dim = c(nstates, max_model_layers, nmembers))

  for (i in seq_len(nstates)) {
    sname <- states_config$state_names[i]
    entry <- nml_vals[[sname]]

    if (!is.null(entry)) {
      if (is.list(entry)) {
        interp <- approx(
          x = entry$x, y = entry$y, xout = modeled_depths, rule = 2
        )$y
      } else {
        interp <- approx(
          x = x_asc, y = entry, xout = modeled_depths, rule = 2
        )$y
      }
    } else if (sname %in% names(aed2_init)) {
      interp <- rep(aed2_init[[sname]], ndepths_modeled)
    } else {
      next
    }

    states[i, seq_len(ndepths_modeled), ] <- interp
  }

  # model_internal_heights: height above bottom for each modeled depth
  heights_from_bottom <- lake_depth - modeled_depths
  heights_from_bottom[heights_from_bottom < 0] <- NA_real_

  model_internal_heights <- array(NA_real_, dim = c(max_model_layers, nmembers))
  model_internal_heights[seq_len(ndepths_modeled), ] <- heights_from_bottom

  snow_ice_thickness <- array(NA_real_, dim = c(3L, nmembers))
  snow_ice_thickness[1L, ] <-
    if (!is.null(ip$snow_thickness))      ip$snow_thickness      else 0
  snow_ice_thickness[2L, ] <-
    if (!is.null(ip$white_ice_thickness)) ip$white_ice_thickness else 0
  snow_ice_thickness[3L, ] <-
    if (!is.null(ip$blue_ice_thickness))  ip$blue_ice_thickness  else 0

  # salt interpolated to modeled_depths
  salt <- array(NA_real_, dim = c(ndepths_modeled, nmembers))
  salt[] <- approx(
    x = x_asc, y = the_sals[ord], xout = modeled_depths, rule = 2
  )$y

  aux_states_init <- list(
    snow_ice_thickness     = snow_ice_thickness,
    the_sals_init          = the_sals,
    model_internal_heights = model_internal_heights,
    lake_depth             = array(lake_depth, dim = c(nmembers)),
    salt                   = salt,
    log_particle_weights   = array(log(1.0), dim = c(nmembers)),
    inflation              = NA_real_
  )

  list(
    states          = states,
    pars            = NULL,
    aux_states_init = aux_states_init
  )
}
