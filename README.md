<!-- badges: start -->
[![R-CMD-check](https://github.com/FLARE-forecast/FLAREr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/FLARE-forecast/FLAREr/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/FLARE-forecast/FLAREr/branch/main/graph/badge.svg)](https://codecov.io/gh/FLARE-forecast/FLAREr?branch=main)
<!-- badges: end -->

# FLAREr

This document serves as a user guide and a tutorial for the FLARE (Forecasting Lake and Reservoir Ecosystems) system ([Thomas et al. 2020](https://agupubs.onlinelibrary.wiley.com/doi/abs/10.1029/2019WR026138)). FLARE generates forecasts with uncertainty of water temperature and water quality for 1- to 35-day-ahead time horizon at multiple depths of a lake or reservoir. It uses data assimilation to update the initial starting point for a forecast and the model parameters based a real-time statistical comparisons to observations.  It has been developed, tested, and evaluated for Falling Creek Reservoir in Virginia ([Thomas et al. 2020](https://doi.org/10.1029/2019WR026138)), Beaverdam Reservoir in Virginia ([Wander et al. 2024](https://doi.org/10.1002/ecs2.4752)), Lake Sunapee in New Hampshire ([Woelmer et al. 2024](https://doi.org/10.1016/j.ecoinf.2024.102825)), National Ecological Observatory Network lakes across the United States ([Thomas et al. 2023](https://doi.org/10.1002/fee.2623) and [Olsson et al. 2024](https://doi.org/10.1029/2023WR035901)), and Lough Feeagh in Ireland ([Páiz et al. 2025](https://doi.org/10.1002/ecs2.70335)). FLARE was among the top-performing forecast models in a year-long water quality forecasting challenge across seven lakes ([Olsson et al. 2025](https://doi.org/10.1002/eap.70004)).

FLAREr is a set of R scripts that

* Generating the inputs and configuration files required by the General Lake Model (GLM)
* Applying data assimilation to GLM
* Processing and archiving forecast output
* Visualizing forecast output

FLARE uses the 1-D General Lake Model ([Hipsey et al. 2019](https://www.geosci-model-dev.net/12/473/2019/)) as the mechanistic process model that predicts hydrodynamics of the lake or reservoir. For forecasts of water quality, it uses GLM with the Aquatic Ecosystem Dynamics library. FLARE v4.0 requires GLM-AED version 4, which adds the NetCDF restart capacity that FLARE's restart workflow depends on. GLM-AED 4 is currently available on the `v4alpha` branch of GLM-AED and is provided by the `GLMAEDr` package (see [Installation](#installation) below).

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

The recommended way is to install the `GLMAEDr` package from Github, which downloads and manages the GLM binary for you. It provides GLM-AED version 4 (the `v4alpha` build), which FLARE v4.0 requires for its NetCDF restart capacity:

```
remotes::install_github("flare-forecast/GLMAEDr")
GLMAEDr::glm_install()
Sys.setenv('GLM_PATH'=GLMAEDr::glm_path())
```

> **Windows is not currently supported.** GLM-AED version 4 binaries are not yet built for Windows, so `GLMAEDr` cannot provide a working GLM binary on that platform. Use macOS or Linux (for example, via WSL2 on Windows) until v4 Windows binaries are available.

The alternative pathways below predate GLM-AED 4 and may not include the NetCDF restart capacity. Use them only if the binary they provide is a GLM-AED 4 (`v4alpha`) build; otherwise the FLARE restart workflow will not work.

Alternatively, you can install the `GLM3r` package from Github using

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

## Use

FLAREr is a set of functions that address key steps in the forecasting workflow. 

### Requires

User-generated *insitu* observations, meteorology, and inflow/outflow in a specified format.  See the FLARE example vignette for format specification.

The expected top-level directory structure (referred to as `lake_directory` in the code) is:

```
lake_directory/
├── configuration/
│   └── <config_set_name>/   # e.g. "default" — holds FLARE yml files and GLM nml
├── drivers/
│   ├── met/                 # meteorological driver files
│   ├── iflow/               # inflow driver files
│   └── oflow/               # outflow driver files
└── targets/
    └── <site_id>/           # observed in-situ data
```

The `configuration/` sub-directory must contain a `configure_run.yml` file and the GLM namelist (`glm3.nml`). Additional configuration files (states, parameters, observations) live alongside these. See the [FLAREr example vignette](articles/flare-example-vignette.html) for full format specifications.

### Quick Run

The code below will produce a single forecast for Falling Creek Reservoir using configuration files included with the package.

```
library(arrow)
library(tidyverse)
library(FLAREr)

remotes::install_github("flare-forecast/GLMAEDr")
GLMAEDr::glm_install()
Sys.setenv('GLM_PATH'=GLMAEDr::glm_path())


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

## Storage backends (local, S3, and FaaSr)

FLAREr reads its drivers, targets, and configuration — and writes its
forecast output, scores, and restart files — through a single I/O layer
that can target three different backends. The backend is selected per run
from two flags in `configure_run.yml`:

| `use_s3` | `use_faasr` | Mode    | Where data lives |
|----------|-------------|---------|------------------|
| `FALSE`  | `FALSE`     | `local` | the local filesystem under `lake_directory` |
| `TRUE`   | `FALSE`     | `s3`    | an S3-compatible bucket, accessed directly with `aws.s3` / `arrow` |
| `TRUE`   | `TRUE`      | `faasr` | a FaaSr DataStore, accessed through the FaaSr runtime |

When neither flag is set both default to `FALSE` (`local` mode), so
existing local configurations keep working unchanged. `use_faasr: TRUE`
requires `use_s3: TRUE` — FaaSr mode is always cloud-backed and FLAREr
errors at startup if `use_faasr` is set without `use_s3`.

Bucket names and endpoints for `s3` and `faasr` modes are defined in the
`s3:` block of `configure_flare.yml`, with one entry per DataStore (e.g.
`drivers`, `targets`, `forecasts_parquet`, `restart`, `scores`). Set
`anonymous: true` on a store for public read-only buckets; non-anonymous
access reads `AWS_ACCESS_KEY_ID` and `AWS_SECRET_ACCESS_KEY` from the
environment.

### FaaSr (serverless cloud execution)

[FaaSr](https://faasr.io) runs R functions as serverless actions on
platforms such as GitHub Actions, AWS Lambda, and OpenWhisk, using
S3-compatible *DataStores* for persistent storage. When FLAREr runs as a
FaaSr action, set `use_faasr: TRUE` so that all object I/O is routed
through the FaaSr runtime's helpers (`faasr_get_file()`,
`faasr_put_file()`, `faasr_arrow_s3_bucket()`, …) instead of calling
`aws.s3` directly. FaaSr supplies the DataStore credentials, so they do
not need to be baked into the FLAREr configuration.

FaaSr is **not** a package dependency of FLAREr: its helper functions are
injected into the global environment by the FaaSr executor when an action
starts. If `use_faasr: TRUE` but those helpers are not present — for
example, when the same configuration is run outside a FaaSr container —
FLAREr prints a warning and falls back to `s3` mode.

Inside a FaaSr action function, call `initialize_faasr(config)` once
before `run_flare()` to validate the configuration: it errors on the
inconsistent `use_faasr: TRUE` / `use_s3: FALSE` combination and warns
early if AWS credentials are missing. See `?initialize_faasr` for
details.



 
