# full workflow for lisem simulations in the Geul catchment.

# Initialization ---------------------------------------------------------------

library(tidyverse) # always load as last library!

# load pcraster functions
source("sources/r_scripts/pcrasteR.R")
set_pcraster(env = "qgis", miniconda = "~/ProgramFiles/miniconda3")

#set digits to 10 for detail in coordinates
options(digits = 10)

# load the functions coded for this project

# 1. Data preparation --------------
# Where possible automatize GIS data management to create base data layers
# for the whole Geul catchment

#1.1 run the code from 'create_base_maps_lisem.R' this now includes a lot of 
# manual work in QGIS - but it will result in tif files for all basemaps.

#1.2 catchment delineation based on DEM and the local drain direction on 20
# meter resolution. 
# currently done manually with steps:
# 1. copy coordinates of outlet close to Juliana channel from QGIS to text file
# 2. with PCRaster col2map make map of the outlet (mask = dem_region.map)
# 3. run delineate_catchment.mod script to find the catchment area.
# 4. resample based on the 5 and 20m mask.map to the correct extent

# Result: based on this dem.map and catchment.map are made for 5 and 20 m.

#1.3 convert base maps to PCraster LISEM input on 5 and 20 meter resolution.

# load the list of base maps.
base_maps <- readLines("sources/base_maps.txt")

# 2. Subcatchment initial testing ----------------------------------------------
# we use subcatchments to test the model setup and perform some pre-calibration

# 3. Calibration ---------------------------------------------------------------

# 4. Baseline simulations ------------------------------------------------------

# 5. Scenario simulations ------------------------------------------------------
