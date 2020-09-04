source(system.file("plotting_general.R", package="flare"))
library(tidyverse)

qaqc_location <- "/Users/quinn/Dropbox/Research/SSC_forecasting/run_flare_package/qaqc_data/"
file_name <- "/Users/quinn/Dropbox (VTFRS)/Research/SSC_forecasting/run_flare_package/test_H_2018_07_12_2018_07_15_F_0_20200904T100834.nc"

file_name <- saved_file
plotting_general(file_name,qaqc_location)

