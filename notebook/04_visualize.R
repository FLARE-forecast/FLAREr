#source(system.file("plotting_general.R", package="flare"))

qaqc_location <- "/Users/quinn/Dropbox/Research/SSC_forecasting/run_flare_package/qaqc_data/"
#file_name <- "/Users/quinn/Dropbox (VTFRS)/Research/SSC_forecasting/run_flare_package/test_H_2018_07_12_2018_07_13_F_2_20200904T144257.nc"

#file_name <- saved_file
flare::plotting_general(file_name = saved_file,
                        qaqc_location)

