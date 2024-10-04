# FLAREr 3.0.0

* New particle filter data assimilation method
* Able to one parameter from a set of parameters that share the same name (e.g., the temperature in one of three sediment zones)
* Able to assimilate depth and secchi depth
* Separate inflow and outflow models
* Openmeteo API as an option for meteorological inputs
* Updates to restart capacity that now requires GLM-AED version 3.4 or higher
* Access to daily summaries (e.g., lake.csv) from GLM-AED in FLAREr output
* Flexible paths for directory and partitioning of driver files.  Uses `glue::glue` to create path from internal variables.
* Capacity to save subdaily statistics (like max temperature at a depth or mean co2 flux)

# FLAREr 3.0.1

* Fix bug in `check_noaa_ready()` that assumed a specific value for `future_met_model`
* Fixes to testing suite

# FLAREr 3.0.2

* Fixed bug associated with the daily diagnostics where the depths defaulted to a logical if all depth values were NA
* Fixed bug associated with plotting daily diagnostics
