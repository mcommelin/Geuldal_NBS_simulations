# make rainfall and discharge dat for selected event for LISEM
# !before running this code make the subcatchment db
#' 1. create input precipitation data for OpenLISEM for each event
#' 2. make easily readable discharge/waterheigt tables for the
#'    calibration of OpenLISEM.

# Initialization --------------------------------------------------------------
library(hms)
library(gdalUtilities)
library(terra)
library(raster)
library(sf)
library(foreach)
library(doParallel)
library(tidyverse)

# load pcraster functions
source("sources/r_scripts/pcrasteR.R")
set_pcraster(env = "qgis", miniconda = "~/ProgramFiles/miniconda3")

#functions
add_suffix <- function(strings) {
  # Use grepl to check if the string contains an underscore
  sapply(strings, function(x) {
    if (!grepl("_", x)) {
      paste0(x, "_00")
    } else {
      x
    }
  })
}

n_cores <- detectCores() # number of cores
registerDoParallel(cores = n_cores - 2) # register the cluster

#1. make LISEM input precipitation maps ---------------------------------------
# 
# #make a map with numbers per cell as ID zones
# # !!!!this code only has to be run once
# # give numbers to cells
# a <- raster(paste0("data/raw_data/neerslag/KNMI_radar_1uur/NSL_20200310_00.ASC"))
# n_rows <- nrow(a)
# n_cols <- ncol(a)
# matrix_data <- matrix(1:(n_rows * n_cols), nrow = n_rows, ncol = n_cols)
# a$ID_zones <- matrix_data
# # write the ID zones to a asc map
# writeRaster(a$ID_zone, paste0("data/processed_data/ID_zones_KNMI_radar.asc"), overwrite = T)

#' We Assume that the extent of the input KNMI radar data is the same everywhere
#' checked with the current events and that is correct. When switching to 5 min
#' temporal resolution, check again.


# function that
# selects event
# selects resolution
# selects catchment
# and then makes the precipitation input

# settings on resolution, event and catchment
resolution = 20 # 5 or 20
event_num = 2 # 1, 2, or 3
catch_num = 1 # 1 = Geul 2 - 12 = subcatchments see points table


rain_input_lisem <- function(
    event_num = event_num,
  resolution = resolution,
  catch_num = catch_num
) {
  
  #load the events
events <- read_csv("sources/selected_events.csv") %>%
  mutate(ts_start = ymd_hms(event_start),
         ts_end = ymd_hms(event_end))

#read catchment information
points <- read_csv("LISEM_data/tables/outpoints_description.csv")

# set options to enough digits for accuracy extent
options(digits = 10)

# get the catchment name
catch_name <- points %>%
  filter(point == catch_num) %>%
  filter(cell_size == resolution) %>%
  select(subcatch_name)

k = event_num
  
  event_start <- events$ts_start[k]
  event_end <- events$ts_end[k]
  hours <- seq(event_start, event_end, by = "hours")
  
  map_names <- str_remove(hours, ":.*") %>%
    str_replace_all("-", "") %>%
    str_replace_all(" ", "_") %>%
    sapply(., add_suffix)
  
  #rain_maps <- map_names %>%
   # paste0("rain_", ., ".map")
  
  map_names <- map_names %>%
    paste0("NSL_", ., ".ASC")
  
  # get the correct directories
  if (catch_num == 1) {
    # Geul
    main_dir <- paste0("LISEM_data/Geul_", resolution, "m/")
  } else {
    # subcatchments
    main_dir <- paste0("LISEM_data/subcatchments/", catch_name, "_", resolution, "m/")
  }
  
  # event name
  ev_name <- as.character(event_start) %>%
    str_remove_all("-") %>%
    str_extract("^([0-9]{8})") %>%
    paste0("rain_", .)
  
  # general settings
  srs = "EPSG:28992"
  method = "near"
  
  # resample to ID.map
  
  # get the extent of the mask.map
  map2asc(
    map_in = paste0(main_dir, "maps/mask.map"),
    map_out = paste0(main_dir, "mask.asc")
  )
  
  # calculate extent from ascii header
  # read the header
  header <- readLines(paste0(main_dir, "mask.asc"), n = 5)
  # extract only digits or '.' from the strings in header
  header <- gsub("[^0-9.]", "", header)
  
  # convert to numeric
  header <- as.numeric(header, digits = 10)
  
  # get the extent
  extent <- c(
    header[3], # xllcorner
    header[4], # yllcorner
    header[3] + header[1] * header[5], # xurcorner
    header[4] + header[2] * header[5]  # yurcorner
  )
  
  #remove the mask.asc
  file.remove(paste0(main_dir, "mask.asc"))
  
  # reproject ID raster with gdal warp
  gdalwarp(
    srcfile = paste0("data/processed_data/ID_zones_KNMI_radar.asc"),
    dstfile = paste0(main_dir, "maps/ID.asc"),
    s_srs = srs,
    t_srs = srs,
    te_srs = srs,
    tr = rep(resolution, 2),
    of = "AAIgrid",
    te = extent,
    r = method,
    dryrun = F,
    overwrite = T
  )
  
  a <- raster(paste0(main_dir, "maps/ID.asc"))
  
  id_max <- max(as.matrix(a))
  id_min <- min(as.matrix(a))
  n_cols_rain <- id_max - id_min + 2
  
  # convert to pcraster format
  asc2map(clone = paste0(main_dir, "maps/mask.map"),
          map_in = paste0(main_dir, "maps/ID.asc"),
          map_out = paste0(main_dir, "maps/ID.map"),
          options = "-S")
  # remove tmp rainfall file
  file.remove(paste0(main_dir, "maps/ID.asc"))
  file.remove(paste0(main_dir, "maps/ID.prj"))
  
  # loop over rainfall maps and make rain input table
  rain_file <- paste0(main_dir, "rain/", ev_name, ".txt")
  # write the header
  writeLines(paste0("# KNMI radar, resampled with GDAL warp, method = ", 
                    method, "\n", n_cols_rain, "\ntime\nmaps"),
             rain_file)
  
  t <- tibble(ts = hours,
              maps = rain_maps) %>%
    mutate(mins = as.numeric((hours - hours[1]) / 60),
           t_str = str_pad(as.character(mins), width = 4,
                           side = "left", pad = "0"),
           t_str = paste0("001:", t_str, " ", maps))
  
  
  

  #append timeseries
  write(t$t_str, file = rain_file, append = T)
  #--------------------------------------------------------
  
    # make folder for rainfall event
    if (!dir.exists(paste0(main_dir, "rain/", ev_name))) {
      dir.create(paste0(main_dir, "rain/", ev_name), recursive = T)
    }
  
  # load the 20m catchment map
  # map2asc
  map2asc(
    map_in = "catchment.map",
    map_out = "catchment.asc",
    sub_dir = paste0(main_dir, "maps/")
  )
  
  # vectorize subcatchment
  ras <- rast(paste0(main_dir, "maps/catchment.asc"))
  pol <- as.polygons(ras)
  writeVector(pol, paste0(main_dir, "maps/catchment.shp"), overwrite = TRUE)
  
  # use polygon of subcatchment to cut the raster
  # general settings
  srs = "EPSG:28992"
  method = "near"
  
  # gdal warp with cutline
  gdalwarp(
    srcfile = paste0("data/raw_data/neerslag/KNMI_radar_1uur/", map_names[1]),
    dstfile = paste0(main_dir, "maps/ID_zone.asc"),
    s_srs = srs,
    t_srs = srs,
    tr = c(1000,1000),
    cutline = paste0(main_dir, "maps/catchment.shp"),
    crop_to_cutline = T,
    of = "AAIgrid",
    r = method,
    dryrun = F,
    overwrite = T
  )
  # give numbers to cells
  a <- raster(paste0(main_dir, "maps/ID_zone.asc"))
  n_rows <- nrow(a)
  n_cols <- ncom(a)
  matrix_data <- matrix(1:(n_rows * n_cols), nrow = n_rows, ncol = n_cols)
  a$ID_zone <- matrix_data
  plot(a)
  # write the raster to a map
  writeRaster(a, paste0(main_dir, "maps/ID_zone.asc"), overwrite = T)
  
  
  
  #--------------------------------------------------------
  
  
    
  # parallel loop to make the rainfall maps
    foreach(i = seq_along(map_names)) %dopar% {
      
     tempfile <- paste0("data/tmp_", Sys.getpid(), ".asc")
       
      # reproject raster with gdal warp
      gdalwarp(
        srcfile = paste0("data/raw_data/neerslag/KNMI_radar_1uur/", map_names[i]),
        dstfile = tempfile,
        s_srs = srs,
        t_srs = srs,
        te_srs = srs,
        tr = rep(resolution, 2),
        of = "AAIgrid",
        te = extent,
        r = method,
        dryrun = F,
        overwrite = T
      )
      # convert to pcraster format
      asc2map(clone = paste0(main_dir, "maps/mask.map"),
              map_in = tempfile,
              map_out = paste0(main_dir, "rain/", ev_name, "/", rain_maps[i]),
              options = "-S")
      # remove tmp rainfall file
      file.remove(tempfile)
      file.remove(paste0("data/tmp_", Sys.getpid(), ".prj"))
    }

    
    
    # make corresponding rainfall file
    t <- tibble(ts = hours,
                maps = rain_maps) %>%
      mutate(mins = as.numeric((hours - hours[1]) / 60),
             t_str = str_pad(as.character(mins), width = 4,
                             side = "left", pad = "0"),
             t_str = paste0("001:", t_str, " ", maps))
    
    rain_file <- paste0(main_dir, "rain/", ev_name, "/rain.txt")
    
    # write the header
    writeLines(paste0("# KNMI radar, resampled with GDAL warp, method = ", 
                      method, "\n2\ntime\nmaps"),
               rain_file)
    #append timeseries
    write(t$t_str, file = rain_file, append = T)
  }
  

# 2. Discharge tables for LISEM ---------------------------------------------

# start time,
# end time
# location of discharge
# type of discharge (wh or Q)
# which discharge points to include?

