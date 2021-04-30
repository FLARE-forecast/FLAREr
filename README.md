<!-- badges: start -->
  [![R-CMD-check](https://github.com/FLARE-forecast/flare/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/FLARE-forecast/flare/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/FLARE-forecast/flare/branch/master/graph/badge.svg)](https://codecov.io/gh/FLARE-forecast/flare?branch=master)
<!-- badges: end -->

# FLAREr

The [FLARE project](https://flare-forecast.org/) creates open-source software for flexible, scalable, robust, and near-real time iterative ecological forecasts in lakes and reservoirs.

## Installation

FLARE uses the [General Lake Model (GLM)](https://aed.see.uwa.edu.au/research/models/GLM/) for forecasting.
You will need to download the necessary packages prior to running.
```
remotes::install_github("FLARE-forecast/GLM3r")
remotes::install_github("FLARE-forecast/FLAREr")

```
