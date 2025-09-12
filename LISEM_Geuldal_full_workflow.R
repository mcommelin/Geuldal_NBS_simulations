# full workflow for lisem simulations in the Geul catchment.

# Initialization ---------------------------------------------------------------
library(yaml)
library(hydroGOF)
library(rosettaPTF)
library(gdalUtilities)
library(terra)
library(raster)
library(cowplot)
library(sf)
library(conflicted)
library(tidyverse)
library(sensobol)

# load configuration
config <- yaml.load_file("config.yaml")

# make global choices for conflicting functions
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

# load pcraster functions
source("sources/r_scripts/pcrasteR.R")
set_pcraster(env = config$conda_env, miniconda = config$miniconda_path)

#set digits to 10 for detail in coordinates
options(digits = 10)

# load helper functions coded for this project
source("sources/r_scripts/aux_functions.R")

# 1. Data preparation --------------
# Where possible automatize GIS data management to create base data layers
# for the whole Geul catchment

# !! All datetime data in the project is in GMT+1 !!

## 1.1 make base maps ----------------------------------------------------------
# run the code from 'create_base_maps_lisem.R' this now includes a lot of 
# manual work in QGIS - but it will result in tif files for all basemaps.

#Notes on manual adjustments!



## 1.2 cathment delineation ----------------------------------------------------
# catchment delineation based on DEM and the local drain direction on 20
# meter resolution. 
# currently done manually with steps:
# 1. copy coordinates of outlet close to Juliana channel from QGIS to text file
# 2. with PCRaster col2map make map of the outlet (mask = dem_region.map)
# 3. run delineate_catchment.mod script to find the catchment area.
# 4. resample based on the 5 and 20m mask.map to the correct extent
# 5. run the pcr_script 'base_ldd.mod' to make the ldd.map

# Result: based on this dem.map and catchment.map and ldd.map are made for 5 and 20 m.

## 1.3 preparation of precipitation and discharge data -------------------------
# with the script 'prepare_rainfall_discharge_lisem.R' the radar precipitation
# is processed to input for OpenLISEM, including a ID.map with 1km2 grid.
# this script calls 'KNMI_precipitation.R' which download 5 minute radar data
# from the KNMI data portal. It downloads the days of the selected events.

## 1.4 make SWATRE soil tables -------------------------------------------------

# for the simulations of infiltration we use the SWATRE mobel inside OpenLISEM
# this requires the van Genuchten parameters for differents soil layers for
# each identified soil and landuse combination. 
# To estimate these parameters from variables we know a modelling / equation
# workflow is used.
# Based on soil texture, organic matter and management we calculate the input
# for SWATRE by first applying Saxton&Rawls 2006 equations and than the Rosetta
# (v3) model.
source("sources/r_scripts/swatre_input.R")
soil_landuse_to_swatre(file = "LISEM_data/swatre/UBC_texture.csv",
                       swatre_out = "LISEM_data/calibration/base_swatre_params.csv")
# and make the tables based on the base params:
make_swatre_tables(cal_file = "base_swatre_params.csv")

## 1.5 convert to PCRaster maps ------------------------------------------------
# convert base maps to PCraster LISEM input on 5 and 20 meter resolution.

# load the list of base maps.
base_maps <- readLines("sources/base_maps.txt")

# convert the required base maps
source("sources/r_scripts/source_to_base_maps.R") #function to transform tif to .map

chanmaps <- c("channels_bool.tif", "channels_depth.tif", "channels_width.tif",
            "channels_type.tif", "build_up_area_5m.tif", "channels_baseflow.tif")
outmaps <- c("chanmask", "chandepth", "chanwidth", "chantype", "bua", "baseflow")

for (i in seq_along(chanmaps)) {
  source_to_base_maps(
    map_in = paste0("data/processed_data/GIS_data/base_rasters/", chanmaps[i]),
    map_out = outmaps[i],
    resample_method = "max"
  )
}

# 2. Subcatchment initial testing ----------------------------------------------
# we use subcatchments to test the model setup and perform some pre-calibration
# the used subcatchments are (ID number points table):
# Watervalderbeek (10), Eyserbeek (14)
# Kelmis (18), Gulp (4), Lemiers (12)

#! Always load the following data - adjust if needed for custom settings
points_id <- config$subcatchments #, 18, 4, 12, 90)
reso <- config$resolution

# load subcatchment points csv file
points <- read_csv("LISEM_data/setup/outpoints_description.csv")

# Additional preparation of baseflow
#1. If not done run the function from 2.2 for the full Geul_catchment
#create_lisem_run(20, 1)

# copy baseflow.map to full run
#file.copy(from = "LISEM_data/Geul_5m/maps/baseflow.map", to = "LISEM_runs/Geul_5m/maps", overwrite = T)
#file.copy(from = "LISEM_data/Geul_20m/maps/baseflow.map", to = "LISEM_runs/Geul_20m/maps", overwrite = T)

# run the pcrscript to prepare the baseflow maps 'sources/pcr_scripts/baseflow_calculations.mod'
# update the runfile, set the option: stationary baseflow as map = 1 to 0.
# run the model until the first calculations start.
# copy the resulting baseinflow.map from the results folder to 'LISEM_data/Geul_xm/maps'

# After these steps rebuild the desired subcatchments and LISEM runs

## 2.1 prepare subcatchments ----------------------------------------------------

# load the function for subcatchment preparation
source("sources/r_scripts/create_subcatch_db.R")

for (i in seq_along(points_id)) {
  for (j in seq_along(reso)) {
    base_maps_subcatchment(
      cell_size = reso[j],
      sub_catch_number = points_id[i],
      calc_ldd = F
    )
  }
}

# this databases can be used to create a LISEM run. Choices in settings or
# calibration values can be set in this stage.

## 2.2 make initial lisem runs ------------------------------------------------

# TODO adjust runfile template -> make sure it is up to date with latest settings!
# TODO redefine begin and end times for subcatch events based on P and Q observed
source("sources/r_scripts/create_lisem_run.R")

for (i in seq_along(points_id)) {
  for (j in seq_along(reso)) {
    create_lisem_run(
      resolution = reso[j], 
      catch_num = points_id[i]
    )
  }
}

# first we explore the available precipitation and discharge data 
# to select events for calibration
# in the script 'discharge_rain_exploration.R' a first exploration and selection
# of events for the whole Geul catchment is done. This is also described in the
# document: 'selectie_regenbuien.docx'

# Based on this we further:
# 1: check the quality of the 5 minute resolution rainfall radar
source("sources/r_scripts/create_graphs_observations_simulations.R")

## 2.3 Explore precipitation and discharge --------------------------------------
# explore the available discharge and precipitation data
events <- read_csv("sources/selected_events.csv") %>%
  mutate(ts_start = ymd_hms(event_start),
         ts_end = ymd_hms(event_end)) %>%
  filter(use == "cal")

# check the quality of the 5 minute rainfall data
# for 2 subcatchments:
points_id <- c(10, 14, 4, 18) # add 4 and 18 later
# load subcatchment points csv file
points <- read_csv("LISEM_data/setup/outpoints_description.csv")

# Prepare all combinations of points, events, and temporal resolutions
Tres <- c("hour", "min")
combos <- expand.grid(point = points_id, event = events$ts_start,
  tres = Tres, stringsAsFactors = FALSE)

rain_list <- vector("list", nrow(combos))
for (x in seq_len(nrow(combos))) {
  point_id <- combos$point[x]
  event_idx <- combos$event[x]
  tres_val <- combos$tres[x]
  
  # select subcatchment
  subcatch <- points %>%
    filter(point == point_id) %>%
    filter(cell_size == 5)
  subcatch_name <- subcatch$subcatch_name
  wdir <- paste0("LISEM_runs/", subcatch_name, "_5m/maps/")
  evdate <- date(combos$event[x])
  # run the function to compare precipitation sources
  rain_list[[x]] <- subcatch_rain_compare(
    wdir = wdir,
    ev_date = evdate,
    tres = tres_val
  )
}

# combine the plots
# Extract total values from rain_list (assuming total P is in the 2nd list)
combos$total <- sapply(rain_list, function(x) x[[2]])
x = nrow(combos) / 2
# with cowplot combine the figures and save
for (i in 1:x) {
  plot_grid(rain_list[[i]][[1]], rain_list[[i + x]][[1]], nrow = 2, align = "hv")
  ggsave(
    paste0("images/rain_compare_", date(combos$event[i]), "_", combos$point[i],".png")
  )
}

# Based on this analysis we first will work with the event on 2023-06-22

# points of subcatchments
p_id <- c(10, 14, 4, 18, 12)
# event_dates
ev_dates <- c("2023-06-22")
# mkae figures with combined rain and discharge
graph_subcatch_qp(points_id = p_id, event_dates = ev_dates)

## 2.4 Simulation and figure ---------------------------------------------------
# select a subcatchment and event from the LISEM_runs folder structure
# manually execute the LISEM run
# when finished this function below will make a graph of the discharge including
# a Goodness-of-fit calculation
# point_id = number of the subcatchment point
# resolution = resolution of the simulation (5 or 20 m)
# clean_up = do you want to empty the results directory after the figure
#             is made - advise is TRUE!
# the resulting figure is stored in ./images/simulations/
source("sources/r_scripts/create_graphs_observations_simulations.R")
# WARNING; this function only works on a clean res folder, so empty it before a new lisem simulation!!!!
graph_lisem_simulation(point_id = 4, resolution = 20, clean_up = F)


# 3. Calibration ---------------------------------------------------------------

## 3.1 manual calibration ------------------------------------------------------

# adjust infiltration parameters for calibration. 
# copy the file "LISEM_data/calibration/base_params_swatre.csv 
# give a new name and adjust thevalues for specific ubc codes.
# initial parameters of interest are npar, alpha and ksat.
# when save run the following function to update the input
# WARNING this input effects all lisem runs of the Geulcatchment!!
# you can always go back the base settings by loading the base file in the function.
make_swatre_tables(cal_file = "base_swatre_params.csv")

# run your simulation and make evaluation figures:
# WARNING; this function only works on a clean res folder, so empty it before a new lisem simulation!!!!
graph_lisem_simulation(point_id = 10, resolution = 20, clean_up = T)



# load functions to run lisem many times for calibration etc.
source("sources/r_scripts/lisem_auto_functions.R")

## 3.2 First explorative runs --------------------------------------------------

# process the selected calibration parameters to prepare for random sampling
input_parameters_OL()

# sample parameters for QRN run
set.seed(4571)  #random seed, to be able to reproduce sampling

# to draw the sample, we use the 'sensobol' package. For initial exploration 
# runs we only need the 'A' matrix and we will draw 64 parameter sets.
sample_QRN_sim(file = "LISEM_data/setup/vars_openlisem.csv", n = 128, 
               matrix = "A", 
               out_file = "LISEM_data/params/sim_runoff_params.csv")

# further steps of adjustment if needed.

# prepare runs based on selected parameters and create script to run on hpc
# this is all done in th the script auto_runs_input_lisem.r


# 4. Baseline simulations ------------------------------------------------------

# 5. Scenario simulations ------------------------------------------------------
