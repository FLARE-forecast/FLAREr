<!-- badges: start -->
  [![R-CMD-check](https://github.com/FLARE-forecast/FLAREr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/FLARE-forecast/FLAREr/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/FLARE-forecast/FLAREr/branch/master/graph/badge.svg)](https://codecov.io/gh/FLARE-forecast/FLAREr?branch=master)
<!-- badges: end -->

# FLAREr

The [FLARE project](Forecasting Lake and Reservoir Ecosystems)(https://flare-forecast.org/) creates open-source software for flexible, scalable, robust, and near-real time iterative ecological forecasts in lakes and reservoirs.  It uses data assimilation to update the initial starting point for a forecast and the model parameters based a real-time statistical comparisons to observations.  It has been developed, tested, and evaluated for Falling  Creek Reservoir in Vinton,VA ([Thomas et al. 2020](https://agupubs.onlinelibrary.wiley.com/doi/abs/10.1029/2019WR026138)).

FLAREr is a set of R scripts that

* Generating the inputs and configuration files required by the General Lake Model (GLM)
* Applying data assimilation to GLM
* Processing and archiving forecast output
* Visualizing forecast output

FLARE uses the 1-D General Lake Model ([Hipsey et al. 2019](https://www.geosci-model-dev.net/12/473/2019/)) as the mechanistic process model that predicts hydrodynamics of the lake or reservoir.  For forecasts of water quality, it uses GLM with the Aquatic Ecosystem Dynamics library.   The binaries for GLM and GLM-AED are included in the FLARE code that is available on GitHub. FLARE requires GLM version 3.1 or higher.

More information about the GLM can be found here:

* [GLM users guide](https://aed.see.uwa.edu.au/research/models/GLM/index.html) 
* [GLM 3.0.0 manuscript](https://www.geosci-model-dev.net/12/473/2019/) 
* [GLM on GitHub](https://github.com/AquaticEcoDynamics/GLM)
* [AED on GitHub](https://github.com/AquaticEcoDynamics/libaed2)

FLARE development has been supported by grants from National Science Foundation (CNS-1737424, DEB-1753639, EF-1702506, DBI-1933016, DEB-1926050)

## Installation

You will need to download the necessary packages prior to running.
```
remotes::install_github("FLARE-forecast/GLM3r")
remotes::install_github("FLARE-forecast/FLAREr")

```

## Use

FLAREr is a set of functions that address key steps in the forecasting workflow. 

### Requires

User generated *in situ* observations, meteorology, and inflow/outflow in a specified format.  See FLARE example vignette for format specification.

### Set up

`create_glm_inflow_outflow_files()`:   
`create_obs_matrix()`:    
`generate_glm_met_files()`:   
`generate_initial_conditions()`:    
`initiate_model_error()`:    

### Run

`run_da_forecast()`: runs data assimilation and forecasting. 

### Processing

`write_forecast_netcdf()`: write output in Ecological Forecasting Initiative standards.     
`create_flare_metadata()`: write metadata in Ecological Forecasting Initiative standards.     
`plotting_general()`: generates a PDF with default visualizations. 
 

 
