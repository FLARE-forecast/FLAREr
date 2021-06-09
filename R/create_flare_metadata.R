#' @param file_name
#'
#' @param enkf_output
#'
#' @title Generate metadata (EML) using the Ecological Forecasting Initiatve Standards
#' @return None
#'
#' @export
#'
#' @author Quinn Thomas
#'
#'

create_flare_metadata <- function(file_name,
                             enkf_output){

  emld::eml_version("eml-2.2.0")

  nc <- ncdf4::nc_open(file_name)
  t <- ncdf4::ncvar_get(nc,'time')
  local_tzone <- ncdf4::ncatt_get(nc, 0)$time_zone_of_simulation
  full_time <- as.POSIXct(t,
                          origin = '1970-01-01 00:00.00 UTC',
                          tz = "UTC")

  data_assimilation <- ncdf4::ncvar_get(nc, "data_assimilation")

  forecast_issue_time <- ncdf4::ncatt_get(nc, varid = 0)$forecast_issue_time

  #split_forecast_issue_time <- unlist(stringr::str_split(forecast_issue_time, " "))
  forecast_issue_time <- lubridate::as_date(forecast_issue_time)
  #forecast_issue_time <- paste0(split_forecast_issue_time[1],"T",split_forecast_issue_time[2])
  forecast_iteration_id <- ncdf4::ncatt_get(nc, varid = 0)$forecast_iteration_id
  forecast_project_id <- ncdf4::ncatt_get(nc, varid = 0)$forecast_project_id

  if(!is.null(enkf_output$pars_config)){
    npars <- nrow(enkf_output$pars_config)
  }else{
    npars <- 0
  }
  nstates <- dim(enkf_output$x)[3] - npars

  nmembers <- length(ncdf4::ncvar_get(nc, "ensemble"))

  var_names <- names(nc$var)

  attributeName <- rep(NA, length(var_names))
  attributeDefinition <- rep(NA, length(var_names))
  unit <- rep(NA, length(var_names))
  formatString <- rep(NA, length(var_names))
  numberType <- rep(NA, length(var_names))
  definition <- rep(NA, length(var_names))
  col_classes <- rep("numeric", length(var_names))
  for(i in 1:length(var_names)){
    curr_var <- nc$var[[i]]
    attributeName[i] <- curr_var$name
    attributeDefinition[i] <- curr_var$longname
    unit[i] <- "dimensionless"
    formatString[i] <- NA
    numberType[i] <- typeof(ncdf4::ncvar_get(nc, var_names[i]))
    if(numberType[i] == "double"){numberType[i] = "real"}
  }

  ncdf4::nc_close(nc)

  attributes <- tibble::tibble(
    attributeName = attributeName,
    attributeDefinition = attributeDefinition,
    unit = unit,
    formatString = formatString,
    numberType = numberType,
    definition = definition

  )


  attrList <- EML::set_attributes(attributes,
                            col_classes = col_classes)

  ## sets metadata about the file itself (name, file type, size, MD5, etc)
  physical <- EML::set_physical(basename(file_name))
  ## set metadata for the file as a whole
  dataTable <- EML::eml$otherEntity(
    entityName = "forecast",  ## this is a standard name to allow us to distinguish this entity from
    entityDescription = "Forecast of water physics, chemistry, and biology",
    physical = physical,
    attributeList = attrList)

  EML::set_unitList(data.frame(id = 'netcdf', unitType="dimensionless", "parentSI"="dimensionless", "multiplierToSI" = 1, "description"="units are defined in the netcdf file"))

  coverage <- EML::set_coverage(begin = lubridate::as_date(dplyr::first(full_time)),
                 end = lubridate::as_date(dplyr::last(full_time)),
                 #sci_names = "NA",
                 geographicDescription = enkf_output$config$location$name,
                 west = enkf_output$config$location$longitude, east = enkf_output$config$location$longitude,
                 north = enkf_output$config$location$latitude, south = enkf_output$config$location$latitude)


  keywordSet <- list(
    list(
      keywordThesaurus = "EFI controlled vocabulary",
      keyword = list("forecast",
                     "water quality",
                     "timeseries")
    ))

  abstract <- enkf_output$config$metadata$abstract #system.file("extdata", "abstract.md", package="EFIstandards", mustWork = TRUE)

  dataset = EML::eml$dataset(
    title = enkf_output$config$metadata$forecast_title,
    creator = enkf_output$config$metadata$me,
    contact = list(references = enkf_output$config$metadata$me$id),
    pubDate = lubridate::as_date(lubridate::as_datetime(forecast_issue_time)),
    intellectualRights = enkf_output$config$metadata$intellectualRights,
    abstract =  abstract,
    dataTable = dataTable,
    keywordSet = keywordSet,
    coverage = coverage
  )

  if(enkf_output$config$uncertainty$initial_condition){
    initial_conditions = list(
      # Possible values: no, contains, data_driven, propagates, assimilates
      status = "assimilates",
      # Number of parameters / dimensionality
      complexity = nstates,
      propagation = list(
        type = "ensemble", # ensemble vs analytic
        size = nmembers          # required if ensemble
      ),
      assimilation = list(
        type = "EnKF",
        reference = "https://www.biorxiv.org/content/10.1101/2020.01.22.915538v2.abstract",
        complexity = nstates
      )
    )
  }else{
    initial_conditions = list(
      # Possible values: no, contains, data_driven, propagates, assimilates
      status = "data_driven",
      # Number of parameters / dimensionality
      complexity = nstates
    )
  }

  if(enkf_output$config$uncertainty$parameter & npars > 0){
    parameters = list(
      status = "assimilates",
      complexity = npars,
      propagation = list(
        type = "ensemble", # ensemble vs analytic
        size = nmembers          # required if ensemble
      ),
      assimilation = list(
        type = "EnKF",
        reference = "https://www.biorxiv.org/content/10.1101/2020.01.22.915538v2.abstract",
        complexity = npars
      )
    )
  }else{
    parameters = list(
      status =  "present",
      complexity = 0
    )
  }

  if(enkf_output$config$uncertainty$process){
    process_error = list(
      status = "propagates",
      propagation = list(
        type = "ensemble", # ensemble vs analytic
        size = nmembers          # required if ensemble
      ),
      complexity = nstates,
      covariance = TRUE,
      localization = "Distance extinction of covariance"
    )

  }else{
    process_error = list(
      status = "absent"
    )
  }

  if(enkf_output$config$uncertainty$weather | enkf_output$config$uncertainty$met_downscale){
    drivers = list(
      status = "propagates",
      complexity = length(unique(enkf_output$met_file_names)),
      propagation = list(
        type = "ensemble", # ensemble vs analytic
        size = length(unique(enkf_output$met_file_names)))
    )

  }else{
    drivers = list(
      uncertainty = "contains"
    )
  }

  forecast_horizon <- length(which(data_assimilation == 0))

  if(forecast_horizon == 0){
    forecast_horizon = 1
  }

  additionalMetadata <- EML::eml$additionalMetadata(
    #  describes="forecast",  ## not sure how to find the correct ID for this to be valid
    metadata = list(
      forecast = list(
        ## Basic elements
        timestep = "1 day", ## should be udunits parsable; already in coverage -> temporalCoverage?
        forecast_horizon = forecast_horizon,
        forecast_issue_time = forecast_issue_time,
        forecast_iteration_id = forecast_iteration_id,
        forecast_project_id = forecast_project_id,
        metadata_standard_version = "0.2",
        model_description = enkf_output$config$metadata$model_description,
        ## UNCERTAINTY CLASSES
        initial_conditions = initial_conditions,
        parameters = parameters,
        random_effects = list(
          status = "absent"
        ),
        obs_error = list(
          status = "data_driven"
        ),
        process_error = process_error,
        drivers = drivers
        # assimilation_method ## required if any uncertainty = assimilates
      ) # forecast
    ) # metadata
  ) # eml$additionalMetadata

  my_eml <- EML::eml$eml(dataset = dataset,
                    additionalMetadata = additionalMetadata,
                    packageId = forecast_project_id,
                    system = "uuid"  ## system used to generate packageId
  )

  EML::eml_validate(my_eml)

  EFIstandards::forecast_validator(my_eml)

  eml_file_name <- file.path(enkf_output$config$file_path$forecast_output_directory, paste0(tools::file_path_sans_ext(basename(file_name)),"-eml.xml"))

  EML::write_eml(my_eml, eml_file_name)
  invisible(eml_file_name)
}
