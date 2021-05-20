<!-- badges: start -->
  [![R-CMD-check](https://github.com/FLARE-forecast/FLAREr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/FLARE-forecast/FLAREr/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/FLARE-forecast/FLAREr/branch/master/graph/badge.svg)](https://codecov.io/gh/FLARE-forecast/FLAREr?branch=master)
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

## Use

FLAREr is a set of functions that address key steps in the forecasting workflow. 

### Set up

`create_glm_inflow_outflow_files()`:   
`create_obs_matrix()`:    
`generate_glm_met_files.R()`:   
`generate_initial_conditions()` and `generate_restart_initial_conditions()`:    
`initiate_model_error()`:    

### Run

`run_da_forecast()`: runs data assimilation and forecasting. 

### Processing

`write_forecast_ncdf()`: write output in Ecological Forecasting Initative standards.     
`create_flare_eml()`: write metadata in Ecological Forecasting Initative standards.     
`plotting_general()`: generates a PDF with default visualizations. 
 

 
