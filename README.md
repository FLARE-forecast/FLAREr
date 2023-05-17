<!-- badges: start -->
  [![R-CMD-check](https://github.com/FLARE-forecast/FLAREr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/FLARE-forecast/FLAREr/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/FLARE-forecast/FLAREr/branch/master/graph/badge.svg)](https://codecov.io/gh/FLARE-forecast/FLAREr?branch=master)
<!-- badges: end -->

# FLAREr

This document serves as a users guide and a tutorial for the FLARE (Forecasting Lake and Reservoir Ecosystems) system ([Thomas et al. 2020](https://agupubs.onlinelibrary.wiley.com/doi/abs/10.1029/2019WR026138)). FLARE generates forecasts and forecast uncertainty of water temperature and water quality for 1 to 35-day ahead time horizon at multiple depths of a lake or reservoir. It uses data assimilation to update the initial starting point for a forecast and the model parameters based a real-time statistical comparisons to observations.  It has been developed, tested, and evaluated for Falling  Creek Reservoir in Vinton,VA ([Thomas et al. 2020](https://agupubs.onlinelibrary.wiley.com/doi/abs/10.1029/2019WR026138)) and National Ecological Observatory Network lakes ([Thomas et al. 2023](https://doi.org/10.1002/fee.2623)

FLAREr is a set of R scripts that

* Generating the inputs and configuration files required by the General Lake Model (GLM)
* Applying data assimilation to GLM
* Processing and archiving forecast output
* Visualizing forecast output

FLARE uses the 1-D General Lake Model ([Hipsey et al. 2019](https://www.geosci-model-dev.net/12/473/2019/)) as the mechanistic process model that predicts hydrodynamics of the lake or reservoir. For forecasts of water quality, it uses GLM with the Aquatic Ecosystem Dynamics library. The binaries for GLM and GLM-AED are included in the FLARE code that is available on GitHub. FLARE requires GLM version 3.3 or higher.

More information about the GLM can be found here:

* [GLM 3.0.0 manuscript](https://www.geosci-model-dev.net/12/473/2019/) 
* [GLM on GitHub](https://github.com/AquaticEcoDynamics/glm-aed)
* [GLM users guide](https://aquaticecodynamics.github.io/glm-workbook/) 

FLARE development has been supported by grants from the U.S. National Science Foundation (CNS-1737424, DBI-1933016, DBI-1933102)

## Installation

You will need to download the necessary packages prior to running.

```
remotes::install_github("FLARE-forecast/GLM3r")
remotes::install_github("FLARE-forecast/FLAREr")

```
## Use

FLAREr is a set of functions that address key steps in the forecasting workflow. 

### Requires

User generated *insitu* observations, meteorology, and inflow/outflow in a specified format.  See FLARE example vignette for format specification.

### Quick Run

The code below will produce a single forecast for Falling Creek Reservoir using configuration files included with the package.

```
tmp <- tempdir()
file.copy(system.file("example", package = "FLAREr"), tmp, recursive = TRUE)
lake_directory <- file.path(tmp, "example")
FLAREr::run_flare(lake_directory = lake_directory,configure_run_file = "configure_run.yml", config_set_name = "default")

arrow::open_dataset(file.path(lake_directory,"forecasts/parquet")) |> 
  filter(variable == "temperature",
         depth == 1) |> 
  collect() |> 
  ggplot(aes(x = datetime, y = prediction, group = parameter)) +
  geom_line() +
  geom_vline(aes(xintercept = as_datetime(reference_datetime))) +
  labs(title = "1 m water temperature forecast")
```



 
