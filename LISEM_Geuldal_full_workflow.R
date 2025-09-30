# full workflow for lisem simulations in the Geul catchment.
# select lines and execute these with Ctrl + Enter

# Initialization ---------------------------------------------------------------

# load and set configured settings from config.yaml
source("sources/r_scripts/configuration.R")

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

## 1.4 convert to PCRaster maps ------------------------------------------------
# convert base maps to PCraster LISEM input on 5 and 20 meter resolution.

# load the list of base maps.
base_maps <- readLines("sources/base_maps.txt")

# convert the required base maps
source("sources/r_scripts/source_to_base_maps.R") #function to transform tif to .map

chanmaps <- c("channels_bool.tif", "channels_depth.tif", "channels_width.tif",
              "channels_type.tif", "build_up_area_5m.tif", "channels_baseflow.tif",
              "culverts_bool.tif")
outmaps <- c("chanmask", "chandepth", "chanwidth", "chantype", "bua", "baseflow",
             "culvertmask")

for (i in seq_along(chanmaps)) {
  source_to_base_maps(
    map_in = paste0("data/processed_data/GIS_data/base_rasters/", chanmaps[i]),
    map_out = outmaps[i],
    resample_method = "max"
  )
}

## 1.5 prepare lookup table landuse and soil -----------------------------------

# load fieldwork results
pars_lu <- read_csv("data/processed_data/fieldwork_to_classes.csv") %>%
  mutate(nbs_type = if_else(nbs_type == "extensieve begrazing", NA, nbs_type)) %>%
  # remove 1 nbs label to include in natural grassland group
  filter(is.na(nbs_type)) %>%
  group_by(lu_nr) %>%
  summarise(rr = round(mean(rr), digits = 2),
         n = round(mean(n), digits = 2),
         om = round(mean(om), digits = 2),
         per = round(mean(per), digits = 2))

# load lu table
lu_tbl <- read_csv("sources/setup/tables/lu_tbl.csv")

lu_add <- lu_tbl %>%
  filter(rr != -9) %>%
  select(-description, -smax_eq, - notes)

s_eq <- lu_tbl %>% select(lu_nr, smax_eq)

#option to calibrate input parameters
# multiply OM observed:
cal_factors <- tibble(cf = c(0.7, 1, 0.5, 0.8, 1, 0, 0))

lu_pars <- bind_rows(pars_lu, lu_add) %>%
  left_join(s_eq, by = "lu_nr") %>%
  mutate(om = om * cal_factors$cf)

nms <- as.character(seq(0, ncol(lu_pars) - 1))
names(lu_pars) <- nms

write.table(lu_pars, file = "LISEM_data/tables/lu.tbl",
            sep = " ", row.names = FALSE,
            quote = FALSE)


## 1.6 make SWATRE soil tables -------------------------------------------------

# for the simulations of infiltration we use the SWATRE mobel inside OpenLISEM
# this requires the van Genuchten parameters for differents soil layers for
# each identified soil and landuse combination. 
# To estimate these parameters from variables we know a modelling / equation
# workflow is used.
# Based on soil texture, organic matter and management we calculate the input
# for SWATRE by first applying Saxton&Rawls 2006 equations and than the Rosetta
# (v3) model.
source("sources/r_scripts/swatre_input.R")
soil_landuse_to_swatre(file = "sources/setup/swatre/UBC_texture.csv",
                       swatre_out = paste0("sources/setup/calibration/", swatre_file))


# Additional preparation of baseflow

#WARNING for the calibration of 20230622 we use observed baseflow for 10, 4, 14, 12 and 18.
# still need to update this better in the code!!!!!

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

#points_id <- c(10, 12) # use if you want to update multiple subcatchments on the go

# load the function for subcatchment preparation
source("sources/r_scripts/create_subcatch_db.R")

# run for both resolutions and all selected subcatchments
for (i in seq_along(points_id)) {
  for (j in seq_along(reso)) {
    base_maps_subcatchment(
      cell_size = reso[j],
      sub_catch_number = points_id[i],
      calc_ldd = FALSE, # only recalculate ldd if first time or dem is changed, takes some time!!
      parallel = FALSE  # the map resampling can be done parallel, on windows this causes errors, then set to false.
    )
  }
}

# you can also run for one specific subcatchment e.g.

#base_maps_subcatchment(cell_size = 20, sub_catch_number = 10, calc_ldd = FALSE, parallel = FALSE)

# this databases can be used to create a LISEM run. Choices in settings or
# calibration values can be set in this stage.

## 2.2 make lisem runs ------------------------------------------------

# this function mainly works for manual calibration
# to do many runs exploring a parameter space, code is developed in section 3.

# options to adjust/calibrate LISEM input:
#' 1. the landuse table including OM values.
#'      in section 1.5 this table is made 'cal_factors' can be adjusten or the
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
#' 1.  baseflow_cal.mod  = calculates baseflow for subcatchments on 20230622 
#'                - if more events/subcatch needed adjust code and csv file
#' 2. prepare_db.mod     = main script making most of the maps, channels and culverts
#' 3. storm_drans.mod    = script making storm drains in urban area 
#'                          - contains hard coded drain size!

# the runfile template file should be updated manually if the model has new options
# stored in : 'sources/setup/runfile_template.run'

#points_id <- c(10, 12) # use if you want to update multiple subcatchments on the go
#swatre_file <- "cal_OM_test.csv" # use if you want to change the swatre params file on the go

# TODO redefine begin and end times for subcatch events based on P and Q observed
source("sources/r_scripts/create_lisem_run.R")

for (i in seq_along(points_id)) {
  for (j in seq_along(reso)) {
    create_lisem_run(
      resolution = reso[j], 
      catch_num = points_id[i],
      swatre_file = swatre_file
    )
  }
}

# you can also run for one specific subcatchment e.g.
#create_lisem_run(resolution = 20, catch_num = 10, swatre_file = "cal_OM_swatre.csv")

## 2.3 Simulation and figure ---------------------------------------------------
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

# the resulting figure is stored in ./images/simulations/
source("sources/r_scripts/create_graphs_observations_simulations.R")
# WARNING; this function only works on a clean res folder, so empty it before a new lisem simulation!!!!
graph_lisem_simulation(point_id = 10, resolution = 20, clean_up = T,
                       run_date = "20230622", res_dir = "res")



#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# From here the code is under development for different part of the WRL project.

# first we explore the available precipitation and discharge data 
# to select events for calibration
# in the script 'discharge_rain_exploration.R' a first exploration and selection
# of events for the whole Geul catchment is done. This is also described in the
# document: 'selectie_regenbuien.docx'

# Based on this we further:
# 1: check the quality of the 5 minute resolution rainfall radar
source("sources/r_scripts/create_graphs_observations_simulations.R")

## 2.4 Explore precipitation and discharge --------------------------------------
# explore the available discharge and precipitation data
ev_nums <- c(8,9,5,6)
events <- read_csv("sources/selected_events.csv") %>%
  mutate(ts_start = ymd_hms(event_start),
         ts_end = ymd_hms(event_end)) %>%
  filter(ev_num %in% ev_nums)

# check the quality of the 5 minute rainfall data
# for 2 subcatchments:
points_id <- c(12, 4) # add 4 and 18 later
# load subcatchment points csv file
points <- read_csv("sources/setup/outpoints_description.csv")


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

# 3. Calibration ---------------------------------------------------------------

## 3.1 manual calibration ------------------------------------------------------



# run your simulation and make evaluation figures:
# WARNING; this function only works on a clean res folder, so empty it before a new lisem simulation!!!!
graph_lisem_simulation(point_id = 14, resolution = 20, clean_up = T)

qobs <- read_csv("data/processed_data/obs_discharge/observed_discharge_high_res.csv")

q_run <- qobs %>%
  filter(ev_num == 9) %>%
  filter(point %in% c(14)) %>%
  mutate(qtot = Q * 300)
  
q_sum <- sum(q_run$qtot)
  
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

# prepare catchment etc.
source("sources/r_scripts/source_to_base_maps.R") #function to transform tif to .map

source_to_base_maps(map_in = "data/processed_data/GIS_data/dhydro_raster_line.tif",
                    map_out = "dhydro_line")

# Create a matrix of random 2-digit numbers (10 to 99)
set.seed(123) # Optional: for reproducibility
mat <- matrix(sample(10:99, 1500 * 27000, replace = TRUE), nrow = 1500, ncol = 27000)


# 5. Scenario simulations ------------------------------------------------------
