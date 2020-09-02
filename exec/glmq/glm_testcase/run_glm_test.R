library(ncdf4)

test_directory <- "/Users/quinn/Dropbox/Research/SSC_forecasting/glm_testcase/"
setwd(test_directory)
baseline_output <- paste0(test_directory,"/output_baseline.nc")
testing_output <- paste0(test_directory,"/output.nc")


system("./glm")

#Read in baseline output
baseline_nc <- nc_open(baseline_output)

temp <- ncvar_get(baseline_nc, "temp")
NS <- ncvar_get(baseline_nc, "NS")
time_stamp <- ncvar_get(baseline_nc, "time")

n_time_steps <- dim(temp)[2]
surface_temp_baseline <- rep(NA, n_time_steps)
for(i in 1:n_time_steps){
surface_temp_baseline[i] <- temp[NS[i],i]
}

nc_close(baseline_nc)

#Read in testing output
baseline_nc <- nc_open(baseline_output)

temp <- ncvar_get(baseline_nc, "temp")
NS <- ncvar_get(baseline_nc, "NS")
time_stamp <- ncvar_get(baseline_nc, "time")

n_time_steps <- dim(temp)[2]
surface_temp_testing<- rep(NA, n_time_steps)
for(i in 1:n_time_steps){
  surface_temp_testing[i] <- temp[NS[i],i]
}

nc_close(baseline_nc)



plot(time_stamp, surface_temp_baseline, type = "l", col = "black")
points(time_stamp, surface_temp_testing, type = "l", col =  "red")
legend("topright", legend = c("baseline","tested"), lty = c(1,1), col = c("black","red"))



