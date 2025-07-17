# make rainfall and discharge data for selected event for LISEM
# !before running this code make the subcatchment db
#' 1. create input precipitation data for OpenLISEM for each event
#' this is a combination of an ID.map at both resolutions and an input table
#' These tables can be used for all the subcatchments, so only on input table per 
#' event is needed.
#' 
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

#1. Input precipitation data ---------------------------------------

##1.1  Raster with ID zones ------------------------------------------------------- 
# make a map with numbers per cell as ID zones

# give numbers to cells
a <- raster(paste0("data/raw_data/neerslag/KNMI_radar_1uur/NSL_20200310_00.ASC"))
n_rows <- nrow(a)
n_cols <- ncol(a)
matrix_data <- matrix(1:(n_rows * n_cols), nrow = n_rows, ncol = n_cols)
a$ID_zones <- matrix_data

# write the ID zones to a asc map
writeRaster(a$ID_zone, paste0("data/processed_data/ID_zones_KNMI_radar.asc"), 
            overwrite = T)

#' We Assume that the extent of the input KNMI radar data is the same everywhere
#' checked with the current events and that is correct. When switching to 5 min
#' temporal resolution, check again.

## 1.2 ID maps for 5 and 20 m ------------------------------------------------------

resolution = c(5, 20) # 5 or 20

for (i in seq_along(resolution)) {
  # get the resolution
  res <- resolution[i]
  # set the correct directories
  main_dir <- paste0("LISEM_data/Geul_", res, "m/")
  
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
  
  # reproject ID raster with gdal warp
  gdalwarp(
    srcfile = paste0("data/processed_data/ID_zones_KNMI_radar.asc"),
    dstfile = paste0(main_dir, "maps/ID.asc"),
    s_srs = srs,
    t_srs = srs,
    te_srs = srs,
    tr = rep(res, 2),
    of = "AAIgrid",
    te = extent,
    r = method,
    dryrun = F,
    overwrite = T
  )
  
  a <- raster(paste0(main_dir, "maps/ID.asc"))
  
  # convert to pcraster format
  asc2map(clone = paste0(main_dir, "maps/mask.map"),
          map_in = paste0(main_dir, "maps/ID.asc"),
          map_out = paste0(main_dir, "maps/ID.map"),
          options = "-S")
  # remove tmp rainfall file
  file.remove(paste0(main_dir, "maps/ID.asc"))
  file.remove(paste0(main_dir, "maps/ID.prj"))
}

##1.3 Rain tables hourly per event -------------------------------------------------------

  #load the events
events <- read_csv("sources/selected_events.csv") %>%
  mutate(ts_start = ymd_hms(event_start),
         ts_end = ymd_hms(event_end))

# set options to enough digits for accuracy extent
options(digits = 10)

for (k in seq_along(events$ts_start)) {
  event_start <- events$ts_start[k]
  event_end <- events$ts_end[k]
  hours <- seq(event_start, event_end, by = "hours")
  
  map_names <- str_remove(hours, ":.*") %>%
    str_replace_all("-", "") %>%
    str_replace_all(" ", "_") %>%
    sapply(., add_suffix)
  
  map_names <- map_names %>%
    paste0("NSL_", ., ".ASC")
  
  # event name
  ev_name <- as.character(event_start) %>%
    str_remove_all("-") %>%
    str_extract("^([0-9]{8})") %>%
    paste0("rain_", .)
  
  # find the number of idzones in the radar map
  id_raster <- raster(paste0("data/processed_data/ID_zones_KNMI_radar.asc"))
  n_cols_rain <- max(as.matrix(id_raster)) + 1 # add 1 colum for the timestamp
  
  # loop over rainfall maps and make rain input table
  rain_file <- paste0("LISEM_data/rain/", ev_name, ".txt")
  ev_date <- date(event_start)
  # write the header
   writeLines(paste0("# KNMI radar for ", ev_date, "\n", n_cols_rain, "\ntime"),
             rain_file)
 # add all gauges in the header
   idn <- seq(1, n_cols_rain-1)
   header_part <- paste0("gauge ", idn)
   write(header_part, rain_file, append = T)
   
  # make the rain table 
   t <- tibble()
   # loop over the rainfall maps and add them row per row to a table
  for (i in seq_along(hours)) {
    # read the rainfall raster and convert to array
        x <- raster(paste0("data/raw_data/neerslag/KNMI_radar_1uur/", map_names[i]))
    x <- as.matrix(x)
    x <- round(as.vector(x), digits = 2)
    d <- t(data.frame(x))
    e <- as_tibble(d) %>%
      mutate(timestamp = hours[i])
  t <- bind_rows(t, e)
  }
  # add the timestamp to the table
  precip <- t %>%
   mutate(mins = as.numeric((timestamp - timestamp[1]) / 60),
          t_str = str_pad(as.character(mins), width = 4,
                          side = "left", pad = "0"),
          t_str = paste0("001:", t_str)) %>%
    select(t_str, everything()) %>%
    select(-mins, - timestamp)
  # append the table to the header
  write.table(precip, file = rain_file, append = T, col.names = F,
              row.names = F, sep = " ", quote = F)
}
  
##1.4 Tables 5 min rain ------------------------------------------------------------------
# NOTE!
# before running this code run the code from 'KNMI_precipitation.R'

# rain tables for the 4 events for calibration and control with 5 minute interval
#load the events
events <- read_csv("sources/selected_events.csv") %>%
  mutate(ts_start = ymd_hms(event_start),
         ts_end = ymd_hms(event_end)) %>%
  filter(use != "none")

rain_5min <- read_csv("data/raw_data/neerslag/KNMI_rain_5min.csv")

# set options to enough digits for accuracy extent
options(digits = 10)

for (k in seq_along(events$ts_start)) {
  event_start <- events$ts_start[k]
  event_end <- events$ts_end[k]
  minutes <- seq(event_start, event_end, by = "5 min")
  
  # event name
  ev_name <- as.character(event_start) %>%
    str_remove_all("-") %>%
    str_extract("^([0-9]{8})") %>%
    paste0("rain_5min_", .)
  
  # find the number of idzones in the radar map
  id_raster <- raster(paste0("data/processed_data/ID_zones_KNMI_radar.asc"))
  n_cols_rain <- max(as.matrix(id_raster)) + 1 # add 1 colum for the timestamp
  
  # loop over rainfall maps and make rain input table
  rain_file <- paste0("LISEM_data/rain/", ev_name, ".txt")
  ev_date <- date(event_start)
  # write the header
  writeLines(paste0("# 5min KNMI radar for ", ev_date, "\n", n_cols_rain, "\ntime"),
             rain_file)
  # add all gauges in the header
  idn <- seq(1, n_cols_rain-1)
  header_part <- paste0("gauge ", idn)
  write(header_part, rain_file, append = T)
  
  # make the rain table 
  precip <- rain_5min %>%
    filter(timestamp >= event_start & timestamp <= event_end) %>%
    mutate(mins = as.numeric((timestamp - timestamp[1]) / 60),
           t_str = str_pad(as.character(mins), width = 4,
                           side = "left", pad = "0"),
           t_str = paste0("001:", t_str)) %>%
    select(t_str, everything()) %>%
    select(-mins, - timestamp)
  # append the table to the header
  write.table(precip, file = rain_file, append = T, col.names = F,
              row.names = F, sep = " ", quote = F)
}


# 2. Observed discharge --------------------------------------------------------

## 2.1 WH to Q -----------------------------------------------------------------

### Load data ------------------------------
# load selected events  
events <- read_csv("sources/selected_events.csv") %>%
  mutate(ts_start = ymd_hms(event_start),
         ts_end = ymd_hms(event_end)) %>%
  filter(use != "none")

# load cross-section data based on fieldwork
cs <- read_csv("data/cross_sections_streams.csv") %>%
  mutate(cs_elev = cs_depth * -1 + ceiling(max(cs_depth))) %>%
  arrange(cs_length)

# load point info
Q_pars <- read_csv("sources/height_to_Q_mannings.csv")

# load data and filter for only for the selected events and measurement points
### Load wh data ----------------------------------
# make file list
q_file_dir = "data/raw_data/waterhoogte_metingen/metingen"
q_files <- dir(q_file_dir, recursive = TRUE, pattern = ".csv$")

# load file with station names and characteristics
station_names <- read_csv("sources/naam_meetpunten_Geuldal.csv")
# add long name for each station
station_names <- station_names %>%
  mutate(name_long = paste0(naam, "_", ondertitel)) %>%
  mutate(name_long = str_replace_all(name_long, " ", "_"))

# load all files
q_list <- vector("list", length = length(q_files))

for (i in seq_along(q_files)) {
  col_names_a <- names(read_delim(paste0(q_file_dir, "/", q_files[i]), delim = ";"))
  a <- read_delim(
    paste0(q_file_dir, "/", q_files[i]),
    delim = ";",
    col_names = col_names_a,
    skip = 2
  ) %>%
    rename(timestamp = 'GMT+1') %>%
    pivot_longer(
      cols = (col_names_a[2]:col_names_a[length(col_names_a)]),
      names_to = "code",
      values_to = "wh"
    ) %>%
    mutate(wh = if_else(wh < 0, NaN, wh)) %>%
    filter(!is.na(wh)) %>%
    left_join(station_names, by = "code")
  q_list[[i]] <- a
}
q_all <- bind_rows(q_list)

# in the raw data trailing zeros are trimmed and decimal sign is removed - ad them back.
h_data <- q_all %>%
  mutate(wh = if_else(wh < 40, wh * 10, wh)) %>%
  mutate(wh = if_else(wh > 40000, wh / 1000, wh)) %>%
  mutate(wh = if_else(wh > 4000, wh / 100, wh)) %>%
  mutate(wh = if_else(wh > 400, wh / 10, wh)) %>%
  filter(code %in% Q_pars$code)
# filter only observations during events of interest
h_data <- map2_dfr(events$ts_start, events$ts_end,
                   ~filter(h_data, timestamp >= .x & timestamp <= .y))

### Load Q waterboard ------------------------------------
# make file list
q_file_dir = "data/raw_data/debiet_ruwe_data"
q_files <- dir(q_file_dir, recursive = TRUE, pattern = ".csv$")

# load file with station names and characteristics
station_names <- read_csv("sources/naam_meetpunten_Geuldal.csv")
# add long name for each station
station_names <- station_names %>%
  mutate(name_long = paste0(naam, "_", ondertitel)) %>%
  mutate(name_long = str_replace_all(name_long, " ", "_"))

# load all files
q_list <- vector("list", length = length(q_files))

for (i in seq_along(q_files)) {
  col_names_a <- names(read_delim(paste0(q_file_dir, "/", q_files[i]), delim = ";"))
  a <- read_csv2(
    paste0(q_file_dir, "/", q_files[i]),
    col_names = col_names_a,
    skip = 2
  ) %>%
    rename(timestamp = 'GMT+1') 
  q_list[[i]] <- a
}
qall <- bind_rows(q_list) %>%
  pivot_longer(
    cols = (-timestamp),
    names_to = "code",
    values_to = "Q"
  ) %>%
  mutate(Q = if_else(Q < 0, NaN, Q)) %>%
  filter(!is.na(Q)) %>%
  filter(timestamp > "2020-01-01") %>%
  left_join(station_names, by = "code")

qwb <- map2_dfr(events$ts_start, events$ts_end,
                ~filter(qall, timestamp >= .x & timestamp <= .y))

#select qwb data for points that are relevant.
# TODO remove hardcoding of Q points!
# select all qwb points that align with an outpoint for LISEM modelling
points <- read_csv("LISEM_data/setup/outpoints_description.csv")

point_code <- points %>%
  distinct(point, code) %>%
  filter(!is.na(code))

qwb <- qwb %>%
  left_join(point_code, by = "code") %>%
  filter(!is.na(point)) %>%
  left_join(events, join_by(closest(timestamp >= ts_start))) %>%
  dplyr::select(timestamp, ev_num, code, Q, point, use)

Q_pars <- Q_pars %>%
  filter(code != "WATE_WATHTE_001") %>%
  filter(code != "GEUL_WATHTE_201") # remove the buffer data because we dont have a good calculation for now

#adjust the cross-section depth to elevation
cs <- cs %>%
  left_join(Q_pars, by = "code") %>%
  group_by(code) %>%
  mutate(elev = max(cs_depth) + bed_lvl - cs_depth)

h_data2 <- h_data %>%
  left_join(Q_pars, by = "code") %>%
  left_join(events, join_by(closest(timestamp >= ts_start)))

# loop over locations to calculate q
locs <- Q_pars$code
hdat <- vector("list", length = length(locs))

for (i in seq_along(locs)) {
  # cross section info
  cs_loc <- cs %>%
    filter(code == locs[i])
  stats <- cs_loc$cs_length
  elev <- cs_loc$elev
  
  pars <- Q_pars %>% filter(code == locs[i])
  eq <- pars$equation
  
  # calculate discharge for cross-section
  hdat[[i]] <- h_data2 %>%
    filter(code == locs[i]) %>%
    rowwise() %>%
    mutate(A = calc_channel_area(stations = stats, elev = elev,
                                 water_level = wh, return_par = "A"),
           P = calc_channel_area(stations = stats, elev = elev,
                                 water_level = wh, return_par = "P"),
           R = A/P,
           Q = 1/pars$n * A * R^(2/3) * sqrt(pars$S))
  
  
  if (eq == "tube") {
    # calculate discharge for culvert at instroom Pletsmolen
    nt <- 0.013 # typical Mannings n for concrete culvert
    St <- (0.07 + 0.0047) / 2 # slope estimate at inlet culvert, bit of trial and error.
    hdat[[i]] <- hdat[[i]] %>%
      filter(code == locs[i]) %>%
      rowwise() %>%
      mutate(At = calc_culvert_area(wh = wh-bed_lvl, D = pars$b, return_par = "A"),
             Pt = calc_culvert_area(wh = wh-bed_lvl, D = pars$b, return_par = "P"),
             Rt = At/Pt,
             Qt = 1/nt * At * Rt^(2/3) * sqrt(St))
    # make a comparison between the culvert and channel calculations of Q
    ggplot(hdat[[i]]) +
      geom_point(aes(x = timestamp, y = Q, color = "Q-stream")) +
      geom_point(aes(x = timestamp, y = Qt, color = "Q-culvert")) +
      facet_wrap(~ ev_num, scales = "free", nrow = 3) +
      theme_classic() +
      labs(title = "comparison Q crosssection and Q culvert - Pletsmolen",
           color = "",
           y = "Q (m3/s)") +
      scale_color_manual(values = c("Q-stream" = "blue", "Q-culvert" = "grey"))
    ggsave(paste0("images/subcatch_observations/discharge_compare_watervalderbeek.png"))
    # average the two calculation methods for watervalderbeek
    hdat[[i]] <- hdat[[i]] %>%
      mutate(Q = (Q + Qt) / 2)
    
  }
  
}

hdat <- bind_rows(hdat) %>%
  dplyr::select(timestamp, ev_num, code, Q, use, point)

dat <- bind_rows(hdat, qwb) 
# save high res discharge
write_csv(dat, "data/processed_data/obs_discharge/observed_discharge_high_res.csv")

## 2.2 Hourly discharge -------------------------------------------
# this are observations from the larger streams

# start time,
# end time
# location of discharge
# type of discharge (wh or Q)
# which discharge points to include?

points <- read_csv("LISEM_data/tables/outpoints_description.csv")

# load discharge data - load hourly data from WL
qall <- read_csv("data/raw_data/debiet_uur_data/debietgegevensgeul_VERKORT.csv",
                 skip = 8) %>%
  pivot_longer(cols = '12.Q.31':'10.Q.36',
               values_to = "Q",
               names_to = "code") %>%
  mutate(timestamp = mdy_hm(timestamp))

# load point locations of the discharge
q_points <- st_read("data/rainfall_discharge.gpkg", layer = "discharge_locations")

#filter discharge on location and event times
qall <- qall %>%
  filter(code %in% q_points$code) %>%
  left_join(q_points, by = "code")

#load the events
events <- read_csv("sources/selected_events.csv") %>%
  mutate(ts_start = ymd_hms(event_start),
         ts_end = ymd_hms(event_end))


# filter per event
qevent <- vector("list", length = nrow(events))

for (k in seq_along(events$event_start)) {
  event_start <- events$ts_start[k]
  event_end <- events$ts_end[k]
  
  qevent[[k]] <- qall %>%
    filter(timestamp > events$ts_start[k] &
             timestamp < events$ts_end[k]) %>%
    mutate(ev_num = k)
}
# combine to tabe and save
qtable <- bind_rows(qevent)
write_csv(qtable, "LISEM_data/tables/observed_discharge_hourly.csv")

