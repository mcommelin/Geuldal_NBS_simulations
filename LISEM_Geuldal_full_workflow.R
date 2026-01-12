# full workflow for lisem simulations in the Geul catchment.
# select lines and execute these with Ctrl + Enter

# Initialization ---------------------------------------------------------------

# load and set configured settings from config.yaml
source("sources/r_scripts/configuration.R")

# 1. Data preparation --------------
# Where possible automatize GIS data management to create base data layers
# for the whole Geul catchment

# !! All datetime data in the project is in GMT+1 !!

source("sources/r_scripts/source_to_base_maps.R")
## 1.1 make base maps ----------------------------------------------------------
# based on manual work, and preparation code in 'create_base_maps_lisem.R' 
# base raster and vector layers are made

# the resulting maps are stored online in the folder 'Geuldal_spatial_data'
# from here a full automated workflow is possible.
# if the base maps are changed the workflow has to be rerun to rebuild all 
# influenced maps

## 1.2 catchment delineation ----------------------------------------------------
#First download all maps from Sharepoint - Geuldal spatial data to ./spatial_data

# catchment delineation based on DEM and the local drain direction on 20
# meter resolution

catch_maps_res()
# Result: based on this dem.map and catchment.map are made for 5, 10 and 20 m.

## 1.3 preparation of precipitation and discharge data -------------------------
# with the script 'prepare_rainfall_discharge_lisem.R' the radar precipitation
# is processed to input for OpenLISEM, including a ID.map with 1km2 grid.
# this script calls 'KNMI_precipitation.R' which download 5 minute radar data
# from the KNMI data portal. It downloads the days of the selected events.

#NOTE1: this data is already available from ./spatial_data/ext_data/
#NOTE2: the input rainfall files can be found at ./prepared/rain
  # place this folder in ./LISEM_run/rain 

## 1.4 prepare base dataset  ------------------------------------------------

# the function below makes PCRaster maps for all resolutions from the data
# in ./spatial_data/
spatial_data_to_pcr()

# in the function below the local drain direction maps are made and based
# on the csv file describing all outpoints, subcatchments are made.
# NOTE: ldd calculations for the whole Geul catchement take a lot of time
# these maps are also provided in ./spatial_data/prepared/
# manually adding these to the correct folders will speed up time.
# set force_ldd = TRUE to recalculate the ldd
ldd_subcatch(force_ldd = FALSE)

## 1.5 prepare lookup table landuse and soil -----------------------------------

# note field OM for forests is too high, now a correction done in the csv file
#TODO move OM adjustment to code?
pars_lu <- read_csv("sources/setup/tables/fieldwork_to_classes.csv", show_col_types = FALSE) %>%
  mutate(nbs_type = if_else(nbs_type == "extensieve begrazing", NA, nbs_type)) %>%
  # remove 1 nbs label to include in natural grassland group
  filter(is.na(nbs_type)) %>%
  group_by(lu_nr) %>%
  summarise(rr = round(mean(rr), digits = 2),
         n_res = round(mean(n_res), digits = 2),
         n_veg = round(mean(n_veg), digits = 2),
         om = round(mean(om_corr), digits = 2))

# load lu table
lu_tbl <- read_csv("sources/setup/tables/lu_tbl.csv", show_col_types = FALSE)
lu_add <- lu_tbl %>%
  filter(rr != -9) %>%
  select(-description, -smax_eq, - notes) 
s_eq <- lu_tbl %>% select(lu_nr, smax_eq)

# the O horizon has the high OM values measured in the fieldcampaign
# a value between 1 and < 30 cm can be chosen for each landuse
# a value of 30 or more will give errors in the current code!
# lu types: 1 = akker, 2 = loofbos, 3 = productie gras, 4 = natuur gras,
# 5 = verhard, 6 = water, 7 = naaldbos
O_depth <- c(10, 20, 10, 10, 5, 1, 20)

# add average summer plant cover to create per.map and all derivatives
# change cover values for other seasons: maps per, lai, manning and smax 
# Alternatively we also have data from the fieldwork for per.
per <- c(0.7,0.9,0.7,0.8,0.05,0,0.9)

lu_pars <- bind_rows(pars_lu, lu_add) %>%
  left_join(s_eq, by = "lu_nr")%>%
  arrange(lu_nr) %>%
  mutate(od = O_depth, 
         cover = per)
nms <- as.character(seq(0, ncol(lu_pars) - 1))
names(lu_pars) <- nms

# save the landuse parameters as table for PCRaster
write.table(lu_pars, file = "sources/setup/calibration/lu.tbl",
            sep = " ", row.names = FALSE,
            quote = FALSE)
#note: here only cols 1,2, 3, 5 and 7 are used 1=RR; 2=n_res; 3 = n_veg; 5=SMAX, 7=cover
#the other columns are used in SWATRE creation, swatre_input.R

## 1.6 make SWATRE soil tables -------------------------------------------------

# for the simulations of infiltration we use the SWATRE mobel inside OpenLISEM
# this requires the van Genuchten parameters for differents soil layers for
# each identified soil and landuse combination. 
# To estimate these parameters from variables we know a modelling / equation
# workflow is used.
# Based on soil texture, organic matter and management we calculate the input
# for SWATRE by first applying Saxton&Rawls 2006 equations and than the Rosetta
# (v3) model.
# larger alpha and smaller n give more rapid decrease of k(h)
# Warning: equations are not tested above OM = 8%

source("sources/r_scripts/swatre_input.R")
soil_landuse_to_swatre(file = "sources/setup/swatre/UBC_texture.csv",
                       swatre_out = paste0("sources/setup/calibration/", swatre_file)
                       )

#-#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# The base maps etc for the data are now finished
# All other steps can be done by adjusting PCRaster scripts
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


# 2. Create LISEM runs for subcatchments ---------------------------------------
# we use subcatchments to test the model setup and do the calibration
# the used subcatchments are (ID number points table):
# Watervalderbeek (10), Eyserbeek (14)
# Kelmis (18), Gulp (4), Lemiers (12)
# Sippenaeken (90) -> 1km2 for testing only 

## 2.1 prepare subcatchments ----------------------------------------------------

# PCR scripts called in this function: 
# 1. delineate_catchment.mod   = make the subcatchment
# 2. ldd_subcatch.mod          = if 'calc_ldd = TRUE' - make ldd for subcatchment
# 3. base_maps_subcatch.mod    = cut other basemaps to correct size.

# the aim of this function is to only do data management and not influence 
# important settings for calibration etc, these all should be part of the next
# function.

# the catchments and resolution are by default used from the config file
# alternatively you can adjust the below:

#points_id <- c(4, 18) # use if you want to change catchment
#reso <- c(10, 20)

# load the function for subcatchment preparation
source("sources/r_scripts/create_subcatch_db.R")

# run for both resolutions and all selected subcatchments
for (i in seq_along(points_id)) {
  for (j in seq_along(reso)) {
    base_maps_subcatchment(
      cell_size = reso[j],
      sub_catch_number = points_id[i],
      do_NDVI = TRUE,  # copy NDVI related maps for dates
      calc_ldd = T  # only recalculate ldd if first time or dem is changed, takes some time!!
    )
  }
}

# you can also run for one specific subcatchment e.g.
#base_maps_subcatchment(cell_size = 10, sub_catch_number = 18, calc_ldd = T, do_NDVI = T, calc_ldd = F)

# this databases can be used to create a LISEM run. Choices in settings or
# calibration values can be set in this stage.

## 2.2 make lisem runs ------------------------------------------------

# this function mainly works for manual calibration
# to do many runs exploring a parameter space, code is developed in section 3.

# options to adjust/calibrate LISEM input:
#' 1. the landuse table including OM values.
#'      in section 1.5 this table is made 'cal_factors' can be adjuste or the
#'      resulting LISEM_data/tables/lu.tbl can be adjusted before running the 
#'      swatre code in 1.6
#' 2. the resulting swatre input parameters from section 1.6 can be adjusted 
#'      for each UBC code. A ksat, npar and alpha map are made in each run to
#'      better grasp these values.
#'      WARNING this input effects all lisem runs of the Geulcatchment!!
#'      you can always go back the base settings by loading the base file in the function.
#' 3. the channel mannings n properties: see sources/setup/tables/
#' 
#' PCRaster scripts used in the function:
#' 1. prepare_db.mod     = main script making most of the maps, channels and culverts
#' 2. storm_drains.mod    = script making storm drains in urban area 
#' 3. prepare_buffer_features.mod = script making buffers and ponds + update dem.
#' 4. baseflow_calibration.mod  = calculates baseflow for subcatchments on 20230622 
#'                - if more events/subcatch needed adjust code and csv file

# the runfile template file should be updated manually if the model has new options
# stored in : 'sources/setup/runfile_template.run'

#TODO: update buffer features to final version maps, including corrected volume

#points_id <- c(4) #c(4,10,14,18) # use if you want to update multiple subcatchments on the go
#swatre_file <- "cal_OM_test.csv" # use if you want to change the swatre params file on the go
#reso = c(10, 20) # 5, 10 or 20

source("sources/r_scripts/create_lisem_run.R")

for (i in seq_along(points_id)) {
  for (j in seq_along(reso)) {
    create_lisem_run(
      resolution = reso[j], 
      catch_num = points_id[i],
      swatre_file = swatre_file,
      do_ndvi = TRUE,  # make NDVI related maps and change run file to NDVI maps
      do_runfile = T
    )
  }
}

# you can also run for one specific subcatchment e.g.
#create_lisem_run(resolution = 20, catch_num = 4, swatre_file = swatre_file, T, T)

## 2.3 Calibrated events & subcatch --------------------------------------------

# TODO: update this section with calibrated events / subcatch
# CURRENTLY NOT USED, CODE NOT FULLY FUNCTIONAL!!!

# select a subcatchment and event from the LISEM_runs folder structure
# manually execute the LISEM run
# when finished this function below will make a graph of the discharge including
# a Goodness-of-fit calculation
# point_id = number of the subcatchment point
# resolution = resolution of the simulation (5 or 20 m)
# clean_up = do you want to empty the results directory after the figure
#             is made - advise is TRUE!
# if the runfile name has changed from the default date format set
# run_date = "20230622" # the date as character string
# if the result directory is not 'res' set
# res_dir = <res_folder_name>
# 
# # the resulting figure is stored in ./images/simulations/
# source("sources/r_scripts/create_graphs_observations_simulations.R")
# # WARNING; this function only works on a clean res folder, so empty it before a new lisem simulation!!!!
# graph_lisem_simulation(point_id = 10, resolution = 20, clean_up = T,
#                        run_date = "20230622", res_dir = "res")


# 3. NBS measure explorations --------------------------------------------------

#' in this section, code and functions will be developed to simulate small test
#' catchments with NBS solutions. 
#' Idea are:
#' 1. LandEX workshop results for Pesaken (52) en Bocholtz (54)
#' 2. sensitivity analysis of single measures, rainfall types etc etc.

## 3.1 LandEX workshop scenarios -----------------------------------------------


# 4. HPC runs full Geuldal -----------------------------------------------------

#' this section shows the workflow for the HPC simulations of the whole Geul
#' catchment on a HPC. 
#' Steps:
#' 1. from ./spatial_data to subcatchments (hpc_sub) with lateraal knopen
#' 2. make a database for each hpc_sub, reuse the functions from section 2.
#' 3. make a runfile for each hpc_sub
#' 4. run all the hpc_sub on the hpc
#' 5. collect results and make a discharge file for d-hydro

## 4.1 HPC subcatchments ------------------------------------------------------

# calculate subcatchments with pcraster
# make table with subcatch ID
# assign subcatch to lateraal ID
# make lisem catchments based on lateraal ID
  # question, can we have two side of dhydro domain in 1 lisem run?