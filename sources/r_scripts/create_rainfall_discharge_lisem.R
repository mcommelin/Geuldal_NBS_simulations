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

#1. make LISEM input precipitation maps ---------------------------------------

# function that
# selects event
# selects resolution
# selects catchment
# and then makes the precipitation input

#load only the events that will be used for calibration events
events <- read_csv("sources/selected_events.csv") %>%
  filter(use == "cal") %>%
  mutate(ts_start = ymd_hms(event_start),
         ts_end = ymd_hms(event_end))




for (k in seq_along(events$event_start)) {  
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
  
  # general settings
  srs = "EPSG:28992"
  cell_size <- c(5, 20)
  method = "near"
  
  # give the extent of the masks for both resolutions
  # set options to enough digits for accuracy extent
  options(digits = 10)
  extent <- matrix(
    byrow = TRUE,
    nrow = 2,
    c(
      178004.155600000,295805.049200000,207444.155600000,325845.049200000,
      178008.314700000,295800.098400000,207448.314700000,325840.098400000
    )
  )
  
  for (j in seq_along(cell_size)) {
    sub_dir <- paste0("Geul_", cell_size[j], "m/")
    
    # make folder for event
    if (!dir.exists(paste0("LISEM_data/", sub_dir, "rain/", ev_name))) {
      dir.create(paste0("LISEM_data/", sub_dir, "rain/", ev_name))
    }
    
    for (i in seq_along(map_names)) {
      # reproject raster with gdal warp
      gdalwarp(
        srcfile = paste0("data/raw_data/neerslag/KNMI_radar_1uur/", map_names[i]),
        dstfile = "data/tmp_rain.asc",
        s_srs = srs,
        t_srs = srs,
        te_srs = srs,
        tr = rep(cell_size[j], 2),
        of = "AAIgrid",
        te = extent[j, ],
        r = method,
        dryrun = F,
        overwrite = T
      )
      # convert to pcraster format
      asc2map(clone = paste0("LISEM_data/", sub_dir, "maps/mask.map"),
              map_in = "data/tmp_rain.asc",
              map_out = paste0("LISEM_data/", sub_dir, "rain/", ev_name, "/", rain_maps[i]),
              options = "-S")
    }
    # remove tmp rainfall file
    file.remove("data/tmp_rain.asc")
    file.remove("data/tmp_rain.prj")
    
    # make corresponding rainfall file
    t <- tibble(ts = hours,
                maps = rain_maps) %>%
      mutate(mins = as.numeric((hours - hours[1]) / 60),
             t_str = str_pad(as.character(mins), width = 4,
                             side = "left", pad = "0"),
             t_str = paste0("001:", t_str, " ", maps))
    
    rain_file <- paste0("LISEM_data/", sub_dir, "rain/", ev_name, "/rain.txt")
    
    # write the header
    writeLines(paste0("# KNMI radar, resampled with GDAL warp, method = ", 
                      method, "\n2\ntime\nmaps"),
               rain_file)
    #append timeseries
    write(t$t_str, file = rain_file, append = T)
  }
  
}



# 2. Discharge tables for LISEM ---------------------------------------------
