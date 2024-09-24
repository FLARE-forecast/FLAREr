<!-- badges: start -->
[![R-CMD-check](https://github.com/FLARE-forecast/FLAREr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/FLARE-forecast/FLAREr/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/FLARE-forecast/FLAREr/branch/single-parameter/graph/badge.svg)](https://codecov.io/gh/FLARE-forecast/FLAREr?branch=single-parameter)
<!-- badges: end -->

# FLAREr

This document serves as a user guide and a tutorial for the FLARE (Forecasting Lake and Reservoir Ecosystems) system ([Thomas et al. 2020](https://agupubs.onlinelibrary.wiley.com/doi/abs/10.1029/2019WR026138)). FLARE generates forecasts with uncertainty of water temperature and water quality for 1- to 35-day-ahead time horizon at multiple depths of a lake or reservoir. It uses data assimilation to update the initial starting point for a forecast and the model parameters based a real-time statistical comparisons to observations.  It has been developed, tested, and evaluated for Falling Creek Reservoir in Virginia ([Thomas et al. 2020](https://agupubs.onlinelibrary.wiley.com/doi/abs/10.1029/2019WR026138)), Beaverdam Reservoir in Virginia ([Wander et al 2024](https://doi.org/10.1002/ecs2.4752)), Lake Sunapee in New Hampshire ([Woelmer et al. 2024](https://doi.org/10.1016/j.ecoinf.2024.102825)), National Ecological Observatory Network lakes across the United States ([Thomas et al. 2023](https://doi.org/10.1002/fee.2623) and [Olsson et al. 2024](https://doi.org/10.22541/essoar.171458144.44104603/v1)), and Lough Feeagh in Ireland ([Paíz et al. 2024](http://dx.doi.org/10.2139/ssrn.4888712)).

FLAREr is a set of R scripts that

* Generating the inputs and configuration files required by the General Lake Model (GLM)
* Applying data assimilation to GLM
* Processing and archiving forecast output
* Visualizing forecast output

FLARE uses the 1-D General Lake Model ([Hipsey et al. 2019](https://www.geosci-model-dev.net/12/473/2019/)) as the mechanistic process model that predicts hydrodynamics of the lake or reservoir. For forecasts of water quality, it uses GLM with the Aquatic Ecosystem Dynamics library. FLARE requires GLM version 3.3 or higher.

More information about the GLM can be found here:

* [GLM 3.0.0 manuscript](https://www.geosci-model-dev.net/12/473/2019/) 
* [GLM on GitHub](https://github.com/AquaticEcoDynamics/glm-aed)
* [GLM users guide](https://aquaticecodynamics.github.io/glm-workbook/) 

FLARE development has been supported by grants from the U.S. National Science Foundation (CNS-1737424, DBI-1933016, DBI-1933102)

## Installation

You will need to download the necessary packages before running.

```
remotes::install_github("FLARE-forecast/FLAREr")
```

Next, you need the GLM model.  You can get in using multiple pathways

The easiest way is to install the `GLM3r` package from Github using

```
remotes::install_github("rqthomas/GLM3r")
```

or you can download it from the AquaticEcoDynamics GitHub organization.  This code assumes you are in the directory with the FLARE configurations and workflow subdirectories

```
download.file("https://github.com/rqthomas/glm-aed/archive/refs/heads/main.zip", "glm_aed.zip")
unzip("glm_aed.zip")
```

if you are running on Mac you will need to run:

```
system2("chmod","u+x glm-aed-main/binaries/macos/Sonoma/glm_latest/glm")
system2("./glm-aed-main/binaries/macos/Sonoma/glm_latest/glm")
```

if you are running on Linux you will need to run:

```
system2("chmod","u+x glm-aed-main/binaries/ubuntu/22.04/glm_latest/glm")
system2("./glm-aed-main/binaries/ubuntu/22.04/glm_latest/glm")
```

Working on windows

## Use

FLAREr is a set of functions that address key steps in the forecasting workflow. 

### Requires

User-generated *insitu* observations, meteorology, and inflow/outflow in a specified format.  See the FLARE example vignette for format specification.
You are required to set a directory structure as follows:



### Quick Run

The code below will produce a single forecast for Falling Creek Reservoir using configuration files included with the package.

```
library(arrow)
library(tidyverse)
library(FLAREr)

remotes::install_github("rqthomas/GLM3r")
Sys.setenv('GLM_PATH'='GLM3r')


dir.create(tempdir(),showWarnings = FALSE)
lake_directory <- file.path(tempdir(), "extdata")
file.copy(system.file("extdata", package = "FLAREr"), tempdir(), recursive = TRUE)
run_flare(lake_directory = lake_directory,configure_run_file = "configure_run.yml", config_set_name = "default")

open_dataset(file.path(lake_directory,"forecasts/parquet")) |> 
  filter(variable == "temperature",
         depth == 1) |> 
  collect() |> 
  ggplot(aes(x = datetime, y = prediction, group = parameter)) +
  geom_line() +
  geom_vline(aes(xintercept = as_datetime(reference_datetime))) +
  labs(title = "1 m water temperature forecast")
```



 
