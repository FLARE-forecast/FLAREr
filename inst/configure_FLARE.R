##########################
# Lake information
###########################

lake_name <<- "fcre"
lake_latitude <<- 37.307   #Degrees North
lake_longitude <<- 79.837  #Degrees West

#Time zone that GLM is run in
#Currently needed to be GMT so that it interfaces with the NOAA forecast
#reference_tzone <<- "GMT"
#Local time zone of the lake
local_tzone <<- "EST"

##########################
# Management Options
###########################
simulate_SSS <<- TRUE
#Include SSS in data assimilation
forecast_SSS <<- FALSE
#Run forecasts without SSS turned on
use_specified_sss <<- TRUE
# Use SSS from file in forecast
forecast_SSS_flow <<- 1000
#m3/day rate of flow if SSS turned on in forecast
forecast_SSS_Oxy <<- 500
#umol/m3  of oxygen if SSS turned on in forecast
sss_fname <<- paste0(data_location,"/manual-data/FCR_SSS_inflow_2013_2020.csv")

sss_inflow_factor <<- 1.0
sss_depth <<- 8.0

#####################
# Weather forcing options
######################
use_future_met <<- FALSE
#TRUE = use NOAA forecast for "Future"
#FALSE = use observed weather for "Future"; only works if "forecasting" past dates

DOWNSCALE_MET <<- FALSE
#Downscale the coarse resolutoin NOAA data to the local
#site using the meterology station at the lake

noaa_location <<- paste0(data_location, "/",lake_name,"/")

downscaling_coeff <<- paste0(data_location, "/manual-data/debiased.coefficients.2018_07_12_2019_07_11.RData")

#file name of previous downscaling coefficients
#use NA if not using an existing file

met_ds_obs_start <<- as.Date("2018-07-12")
met_ds_obs_end <<- as.Date("2019-07-11")
#Dates to use to developing the downscaling coefficient

missing_met_data_threshold <<- 100

use_future_inflow <<- FALSE
future_inflow_flow_coeff <<- c(0.0010803, 0.9478724, 0.3478991)
future_inflow_flow_error <<- 0.00965
future_inflow_temp_coeff <<- c(0.20291, 0.94214, 0.04278)
future_inflow_temp_error <<- 0.943

doc_scalar <<- 3.0

############################
# Run information
#############################

model_name <- "glm_aed" #other is "null"

GLMversion <<- "GLM 3.1.0a"
FLAREversion <<- "v1.1"
#GLM and FLARE version; the code adds these to the output files

base_GLM_nml <- paste0(forecast_location,"/glm3.nml" )
base_AED_nml <<- paste0(forecast_location,"/aed2_20200701_2DOCpools.nml")
base_AED_phyto_pars_nml  <<- paste0(forecast_location,"/aed2_phyto_pars_30June2020.nml")
base_AED_zoop_pars_nml  <<- paste0(forecast_location,"/aed2_zoop_pars.nml")

#################################
### Uncertainty simulated
################################

uncert_mode <<- 1
#Choose the types of uncertainty include in foreast
#1 = all types
#2 = no uncertainty
#3 = only process uncertainty
#4 = only NOAA weather forecast uncertainty
#5 = only initial condition uncertainty
#6 = only initial condition uncertainty and no state updating  with EnKF
#7 = only parameter uncertainty
#8 = only meteorology downscaling uncertainty
#9 = no sources of uncertainty and no state updating with EnKF

single_run <<- FALSE
#Removes uncertainty and only simulates 3 ensemble members

#########################
### Depth information
#########################
#Depths used in the EnKF
#This are the depths that are saved between days
#Init depth of lake
lake_depth_init <<- 9.4  #not a modeled state

modeled_depths <<- round(c(0.1, seq(0.33334, 9.33, 0.333334)), 2)

default_temp_init <<- c(25.667, 24.9101, 23.067, 21.8815, 19.6658, 16.5739, 12.9292, 12.8456, 12.8127, 12.8079, 12.778)
default_temp_init_depths <<-  c(0.127, 1.004, 2.005, 3.021, 4.002, 5.004, 6.004, 7.01, 8.001, 9.015, 9.518)
the_sals_init <<- 0.0

default_snow_thickness_init <<- 0.0
default_white_ice_thickness_init <<- 0.0
default_blue_ice_thickness_init <<- 0.0

##############################
##  EnKF setup
##############################
ensemble_size <<- 21*10
n_ds_members <<- 10
n_inflow_outflow_members <<- 21*10
localization_distance <<- NA #distance in meters were covariances in the model error are used
vert_decorr_length <- 4.0
no_negative_states <- TRUE

#################################
# Parameter calibration information
#################################

par_file <- paste0(forecast_location,"/parameter_calibration_config.csv")

#####################################
###  Observation information
######################################

obs_config_file <- paste0(forecast_location,"/observations_config_ch4_dic.csv")

realtime_insitu_location <- paste0(data_location,"/mia-data")
realtime_met_station_location <- paste0(data_location,"/carina-data") 
manual_data_location <- paste0(data_location, "/manual-data") 
realtime_inflow_data_location <- paste0(data_location, "/diana-data")

combined_obs_file <- paste0(manual_data_location,"/observations_postQAQC_long.csv")
insitu_obs_fname <<- c(paste0(realtime_insitu_location,"/Catwalk.csv"),
                       paste0(manual_data_location,"/Catwalk_cleanedEDI.csv"))
secchi_fname <- paste0(manual_data_location,"/Secchi_depth_2013-2019.csv")
ctd_fname <<- paste0(manual_data_location,"/CTD_final_2013_2019.csv")
ch4_fname <<- paste0(manual_data_location,"/DATASET_for_EDI_LOL_MS_10May20.csv")
nutrients_fname <<- paste0(manual_data_location,"/chemistry.csv")
variable_obsevation_depths <<- FALSE
exo_sensor_2_ctd_chla <- c(0, 1)  #c(-2.0430, 2.5314) #c(1.8795, 0.6662)
exo_sensor_2_ctd_do <- c(0, 1) #c(8.3670, 0.7152)
do_sensor_2_ctd_do_5 <- c(0, 1) #c(19.6254, 0.8636)
do_sensor_2_ctd_do_9 <- c(0, 1) #c(11.0971, 0.9156)
ctd_2_exo_sensor_chla <<- c(0, 1)  #c(-2.0430, 2.5314) #c(-1.582, 1.335)
ctd_2_exo_sensor_do <<- c(0, 1) #c(-10.770, 1.061)
exo_sensor_2_grab_sample_fdom <<- c(-38.95, 22.47)
focal_depths <<- NA

specified_inflow1 <- paste0(manual_data_location, "/FCR_weir_inflow_2013_2019_20200624_allfractions_2poolsDOC.csv")
specified_inflow2 <- paste0(manual_data_location, "/FCR_wetland_inflow_2013_2019_20200713_allfractions_2DOCpools.csv")
specified_sss_inflow_file <- paste0(manual_data_location, "/FCR_SSS_inflow_2013_2019_20200701_allfractions_2DOCpools.csv")
specified_sss_outflow_file <- NA
specified_outflow1 <- paste0(manual_data_location, "/FCR_spillway_outflow_SUMMED_WeirWetland_2013_2019_20200615.csv")
specified_metfile <- NA #paste0(manual_data_location, "/met_full_postQAQC.csv")


met_file <- paste0(manual_data_location,"/met_full_postQAQC.csv")
met_raw_obs_fname <<- c(paste0(realtime_met_station_location,"/FCRmet.csv"),
                        paste0(manual_data_location,"/Met_final_2015_2019.csv"))

inflow1_file <<- paste0(manual_data_location,"/inflow_postQAQC.csv")
inflow_raw_file1 <<- c(paste0(realtime_inflow_data_location,"/FCRweir.csv"),
                       paste0(manual_data_location,"/inflow_for_EDI_2013_06Mar2020.csv"))

outflow_file1 <<- paste0(manual_data_location,"/FCR_spillway_outflow_newEDI_SUMMED_WeirWetland_2013_2018_20190912.csv")

inflow_file2 <<- NA


#########################################
###  Water quality state information
#########################################

states_config_file <- paste0(forecast_location,"/states_config_ch4_dic.csv")

#carbon to chlorophyll ratio (mg C/mg chla)
#12 g/ mole of C vs. X g/ mole of chla
#Initial concentration of phytoplankton (mmol C/m3)
# biomass_to_chla <<- c((160/12),(60/12), (60/12))

########################################
# Dignostics
#######################################

diagnostics_names <- c("extc_coef",
                       "PHY_cyano_fI",
                       "PHY_cyano_fNit",
                       "PHY_cyano_fPho",
                       "PHY_cyano_fT",
                       "PHY_green_fI",
                       "PHY_green_fNit",
                       "PHY_green_fPho",
                       "PHY_green_fT",
                       "PHY_diatom_fI",
                       "PHY_diatom_fNit",
                       "PHY_diatom_fPho",
                       "PHY_diatom_fT",
                       "rad")

#########################################
# Archiving options
#######################################

#Pull data from github?
#Push results to github?
pull_from_git <<- TRUE
push_to_git <<- FALSE

#########################################
# Plotting related options
#######################################



# Options for printing function
# Depths (meters) that the water quality variables are plotted
focal_depths_plotting <<- modeled_depths
#Depths that are plotted for the manager plot
focal_depths_manager <<- c(1,5,8) #c(2, 4, 9) #c(4,16,25) #c(4,16,25)
#Indexes for the depths that are compared to calculate turnover
turnover_index_1 <<- 1 #1 #4
turnover_index_2 <<- 8 #8 #25