# make rainfall and discharge dat for selected event for LISEM
# !before running this code make the subcatchment db
#' 1. create input precipitation data for OpenLISEM for each event
#' this is a combination of an ID.map at both resolutions and an input table
#' These tables can be used for all the subcatchments, so only on input table per 
#' event is needed.
#' 2. make easily readable discharge/waterheigt tables for the
#'    calibration of OpenLISEM.
#' 3. make a function to compare the observed and simulated discharge
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

#1. make LISEM input precipitation data ---------------------------------------

## Raster with ID zones ------------------------------------------------------- 
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

## ID maps for 5 and 20 m ------------------------------------------------------

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

## Rain tables per event -------------------------------------------------------

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
  
## 5 min rain ------------------------------------------------------------------
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


# 2. Discharge tables for LISEM ---------------------------------------------

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

# 3. subcatch precipitation graph ----------------------------------------------

# get subcatch name, events and resolution
# make loop


# id.map to catchment size - is now included in standard db script
wdir <- "LISEM_runs/Watervalderbeek_5m/maps/"
ev_date <- "2023-06-22"


subcatch_observed <- function(wdir = NULL,
                              ev_date = NULL,
                              tres = "hour") {
 
 # pcrcalc(options = "ID.map=ID.map*catchment.map", work_dir = wdir)
  
  # map2asc
  # map2asc(map_in = "ID.map",
  #         map_out = "rain_ID.asc",
  #         sub_dir = wdir)
  # find unique grid id's
  rainIDs <- raster(paste0(wdir, "rain_ID.asc"))
  id <- as.vector(rainIDs)
  freq <- as_tibble(table(id)) %>%
    mutate(id_nm = paste0("gauge_", id))
  
  # load rain data
    date_str <- str_remove_all(ev_date, "-")
  if (tres == "min") {
    pfile <- paste0("LISEM_data/rain/rain_5min_", date_str, ".txt")
  } else {
    pfile <- paste0("LISEM_data/rain/rain_", date_str, ".txt")
  }
    skipval <- as.numeric(readLines(pfile)[2]) + 2
  rain_txt <- readLines(pfile)[-(1:skipval)]
  nms <- readLines(pfile)[3:(skipval)] %>%
    str_replace_all(., " ", "_")
  # make a tibble with numbers and row names
  a <- str_split(rain_txt, " ")
  b <- as_tibble(do.call(rbind, a)) %>%
    rename_with( ~ nms) %>%
    mutate(across(-time, as.numeric)) %>%
    mutate(time_min = str_remove(time, "001:"),
           time_min = as.numeric(time_min)) %>%
    select(time_min, all_of(freq$id_nm))
  
    c <- b %>%
    pivot_longer(cols = -time_min,
                 values_to = "P",
                 names_to = "id_nm") %>%
    left_join(freq, by = "id_nm") %>%
    mutate(Ptmp = P * n) %>%
    group_by(time_min) %>%
    summarize(P = round(sum(Ptmp) / sum(n), digits = 2))
  
  #TODO include discharge and add to figure

  #TODO redefine begin and end times for subcatch events based on P and Q observed
  
  
  # make figure
 if (tres == "min") {
   total = round(sum(c$P) / 12, digits = 2)
 } else {
   total = round(sum(c$P), digits = 2)
 }
  plot <- ggplot(c) +
    geom_bar(aes(x = time_min, y = P), stat = "identity") +
    theme_classic() +
    labs(x = "Minutes", y = "P mm/h", 
         title = paste0("Event total = ", total, " in ", subcatch_name, " on ", ev_date))
  
  rain_info <- vector("list", length = 2)
  rain_info[[1]] <- plot
  rain_info[[2]] <- total
  
  return(rain_info)
} # end function subcatchment observations

# 4. function to compare discharge ---------------------------------------------



# Precip and discharge for 10 & 14 --------------------------------------------

# events
events <- read_csv("sources/selected_events.csv") %>%
  mutate(ts_start = ymd_hms(event_start),
         ts_end = ymd_hms(event_end)) %>%
  filter(use == "cal")
# points
points_id <- c(10, 14)
points <- read_csv("LISEM_data/setup/outpoints_description.csv")

# rain all events
rain_5min <- read_csv("data/raw_data/neerslag/KNMI_rain_5min.csv")

## precipitation 5 minute resolution

# make loop over subcatch
rain_sub <- vector("list", length = length(points_id))
for (i in seq_along(points_id)) {
point_id <- points_id[i]
# select subcatchment
subcatch <- points %>%
  filter(point == point_id) %>%
  filter(cell_size == 5)
subcatch_name <- subcatch$subcatch_name
wdir <- paste0("LISEM_runs/", subcatch_name, "_5m/maps/")

# load rain id's from discharge
rainIDs <- raster(paste0(wdir, "rain_ID.asc"))
id <- as.vector(rainIDs)
freq <- as_tibble(table(id)) %>%
  mutate(id_nm = paste0("gauge_", id))

rain_sub[[i]] <- rain_5min %>%
  select(timestamp, all_of(freq$id))  %>%
  pivot_longer(cols = -timestamp,
               values_to = "P",
               names_to = "id") %>%
  left_join(freq, by = "id") %>%
  mutate(Ptmp = P * n) %>%
  group_by(timestamp) %>%
  summarize(P = round(sum(Ptmp) / sum(n), digits = 2)) %>%
  mutate(point = point_id,
         timestamp = timestamp - minutes(5)) # correct KNMI timestamp now data is for the coming 5 minutes

}
rain_sub <- bind_rows(rain_sub)

rain <- map2_dfr(events$ts_start, events$ts_end,
                   ~filter(rain_sub, timestamp >= .x & timestamp <= .y)) %>%
  left_join(events, join_by(
    closest(timestamp <= ts_end)
  )) %>%
  mutate(ev_name = as.character(date(ts_start)))

# add discharge
# first make 'dat' in Q_measurements_Geuldal
q_obs <- dat %>%
  mutate(timestamp = timestamp - minutes(60), # correct to GMT from GMT+1
         ev_name = as.character(date(ts_start)))

# make figures for both subcatch

for (i in seq_along(points_id)) {
  p <- filter(rain, point == points_id[i])
  q <- filter(q_obs, point == points_id[i])

coeff <- 6
  
ggplot() +
  geom_bar(data = p, aes(x = timestamp, y = P/coeff), stat = "identity") +
  geom_line(data = q, aes(x = timestamp, y = Q, color = code)) +
  facet_wrap(~ ev_name, scales = "free", nrow = 3) +
  labs(color = "Meetpunt") +
  scale_y_continuous("Q m3/sec",
                     sec.axis = sec_axis(~ . * coeff, name = "P mm/h")) +
  theme_classic()
ggsave(paste0("images/q_and_p_", points_id[i], ".png"))
}

