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
# 5. run the pcr_script 'base_maps.mod' to make the ldd.map

# Result: based on this dem.map and catchment.map are made for 5 and 20 m.

#1.3 preparation of precipitation data
# with the script 'create_rainfall_discharge_lisem.R' the radar precipitation
# is processed to input for OpenLISEM, including a ID.map with 1km2 grid.

#1.4 convert base maps to PCraster LISEM input on 5 and 20 meter resolution.

# load the list of base maps.
base_maps <- readLines("sources/base_maps.txt")

# convert the channel related maps
source("sources/r_scripts/source_to_base_maps.R") #function to transform tif to .map

chanmaps <- c("channels_bool.tif", "channels_depth.tif", "channels_width.tif",
            "culverts_bool.tif")
outmaps <- c("chanmask", "chandepth", "chanwidth", "culvertmask")

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
# Watervalderbeek (10), Eyserbeek (14), Kelmis (18)

# prepare the databases for the subcatchments
points_id <- c(10, 14, 18)
reso <- c(5, 20)

# load the function for subcatchment preparation
source("sources/r_scripts/make_subcatch_db.R")

for (i in seq_along(points_id)) {
  for (j in seq_along(reso)) {
    base_maps_subcatchment(
      cell_size = reso[j],
      sub_catch_number = points_id[i]
    )
  }
}

# this databases can be used to create a LISEM run. Choices in settings or
# calibration values can be set in this stage.


# 3. Calibration ---------------------------------------------------------------

# 4. Baseline simulations ------------------------------------------------------

# 5. Scenario simulations ------------------------------------------------------
