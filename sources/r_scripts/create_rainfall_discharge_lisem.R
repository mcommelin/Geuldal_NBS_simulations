# mkae rainfall and discharge dat for selected event for LISEM
# !before running this code make the subcatchment db
#' 1. create input precipitation data for OpenLISEM for each event
#' 2. make easily readable discharge/waterheigt tables for the
#'    calibration of OpenLISEM.

# Initialization --------------------------------------------------------------
library(hms)
library(gdalUtilities)
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

# function that
# selects event
# selects resolution
# selects catchment
# and then makes the precipitation input

# settings on resolution, event and catchment
resolution = 20 # 5 or 20
event_num = 2 # 1, 2, or 3
catch_num = 12 # 1 = Geul 2 - 12 = subcatchments see points table


rain_maps_lisem <- function(
    event_num = event_num,
  resolution = resolution,
  catch_num = catch_num
) {
  
#load only the events that will be used for calibration events
events <- read_csv("sources/selected_events.csv") %>%
  filter(use == "cal") %>%
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
  
  rain_maps <- map_names %>%
    paste0("rain_", ., ".map")
  
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
  
    # make folder for rainfall event
    if (!dir.exists(paste0(main_dir, "rain/", ev_name))) {
      dir.create(paste0(main_dir, "rain/", ev_name), recursive = T)
    }
    
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

