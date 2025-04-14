#' Visualize discharge and precipitation for selected events in the
#' Geul catchment. And prepare precipitation input and calibration
#' timeseries for the LISEM simulations.
#' 1. organize the data so we can select interesting events
#' 2. Visualize discharge and precipitation for these events:
#'  i. make a gif of the precipitation over time
#'  ii. make a graph of the discharge in the measurement points in
#'      the main streams
#'  iii. create summary stats of the events
#' 3. create input precipitation data for OpenLISEM for each event
#' 4. make easily readable discharge/waterheigt tables for the
#'    calibration of OpenLISEM.

# Initialization ---------------------------------------------------------------
library(tidyverse)
library(gifski)
library(hms)
library(gdalUtilities)
library(raster)
library(sf)
library(RColorBrewer)

# load pcraster functions
source("sources/pcrasteR.R")
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

# 1. Select events --------------------------------------------------------------

# make file list
q_file_dir = "data/raw_data/waterhoogte_metingen/metingen"
q_files <- dir(q_file_dir, recursive = TRUE,
               pattern = ".csv$")

# load file with station names and characteristics
station_names <- read_csv("data/raw_data/waterhoogte_metingen/naam_meetpunten_Geuldal.csv")
# add long name for each station
station_names <- station_names %>%
  mutate(name_long = paste0(naam, "_", ondertitel)) %>%
  mutate(name_long = str_replace_all(name_long, " ", "_"))

# load all files
q_list <- vector("list", length= length(q_files))

for (i in seq_along(q_files)) {
  
  col_names_a <- names(read_delim(paste0(q_file_dir, "/", q_files[i]), 
                                  delim = ";"))
  a <- read_delim(paste0(q_file_dir, "/", q_files[i]), delim = ";", col_names = col_names_a,
                  skip = 2) %>%
    rename(timestamp = 'GMT+1') %>%
    pivot_longer(cols = (col_names_a[2]:col_names_a[length(col_names_a)]),
                 names_to = "code", values_to = "wh") %>%
    mutate(wh = if_else(wh < 0, NaN, wh)) %>%
    filter(!is.na(wh)) %>%
    left_join(station_names, by = "code")
  q_list[[i]] <- a
}
q_all <- bind_rows(q_list)

# in the raw data trailing zeros are trimmed and decimal sign is removed - ad them back.
q_all <- q_all %>%
  mutate(wh = if_else(wh < 40, wh * 10, wh)) %>%
  mutate(wh = if_else(wh > 40000, wh / 1000, wh)) %>%
  mutate(wh = if_else(wh > 4000, wh / 100, wh)) %>%
  mutate(wh = if_else(wh > 400, wh / 10, wh))

# filter in the waterhieght datasets to find:
# 1. the events with high discharges at the outlet of the Geul in Meersen
b <- q_all %>%
  filter(str_detect(name_long, "Maastrichterlaan")) %>%
  filter(timestamp > "2019-01-01 12:00:00" & timestamp < "2024-12-30 12:00:00") %>%
  filter(wh > 44.2)
# 6 events with high discharge
ggplot(b) +
  geom_point(aes(x=timestamp, y = wh))


# 2. shorter peaks in the Eyserbeek
b <- q_all %>%
  filter(str_detect(name_long, "meetgoot_Eys")) %>%
  filter(timestamp > "2019-01-01 12:00:00" & timestamp < "2024-12-25 12:00:00") %>%
  filter(wh > 101.7)

ggplot(b) +
  geom_point(aes(x=timestamp, y = wh))

# the events + duration are added to 'sources/selected_events.csv'

# 2&3 Precipitation and discharge during events --------------------------------

#load selected events
events <- read_csv("sources/selected_events.csv") %>%
  mutate(ts_start = ymd_hms(event_start),
         ts_end = ymd_hms(event_end))

## GIF of precipitation events -------------------------------------------------
for (k in seq_along(events$event_start)) {
  event_start <- events$ts_start[k] 
  event_end <- events$ts_end[k] 
  hours <- seq(event_start, event_end, by = "hours")
  
  map_names <- str_remove(hours, ":.*") %>%
    str_replace_all("-", "") %>%
    str_replace_all(" ", "_") %>%
    sapply(., add_suffix) 
  
  rain_gifs <- map_names %>%
    paste0("rain_", ., ".png")
  
  map_names <- map_names %>%
    paste0("NSL_", ., ".ASC")
  
  ev_name <- as.character(event_start) %>%
    str_remove_all("-") %>%
    str_extract("^([0-9]{8})") %>%
    paste0("rain_", .)
  
  if (!dir.exists(paste0("images/neerslag/", ev_name))) {
    dir.create(paste0("images/neerslag/", ev_name))
  }
  
  colors <- brewer.pal(9, "Blues")
  breakpoints <- c(0,1,3,5,10,18,26,40,60)
  
  # sum raster for total precipitation map
  maxp <- 0
  for (i in seq_along(map_names)) {
    rain_map <- raster(paste0("data/raw_data/neerslag/KNMI_radar_1uur/", map_names[i]))
    png(filename = paste0("images/neerslag/", ev_name, "/", rain_gifs[i]))
    a <- plot(rain_map, col = colors, breaks = breakpoints)
    title(paste0(hours[i]))
    dev.off()
    mp <- max(as.data.frame(rain_map))
    maxp <- if(mp > maxp) mp else maxp
  }
  
  # make a gif
  files <- paste0("images/neerslag/", ev_name, "/", rain_gifs)
  gifski(
    files,
    gif_file = paste0("images/neerslag/", ev_name, ".gif"),
    delay = 0.3
  )
}








### Discharge figure events ----------------------------------------------------

# # load raw discharge data
# q_dir <- "data/raw_data/debiet_ruwe_data/"
# q_files <- dir(q_dir, recursive = TRUE, pattern = ".csv$")
# qlist <- vector("list", length = length(q_files))
# 
# for (i in seq_along(q_files)) {
#   q_code <- names(read_delim(paste0(q_dir, q_files[i]), delim = ";", n_max = 1))[2]
#   qlist[[i]] <- read_delim(
#     paste0(q_dir, q_files[i]),
#     delim = ";",
#     skip = 1,
#     locale = locale(decimal_mark = ",")
#   ) %>%
#     rename(timestamp = '...1') %>%
#     mutate(code = q_code)
#   
# }
# 
# #combine discharge from all stations to 1 list
# qall <- bind_rows(qlist)

# alternative approach - load hourly data from WL
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
  filter(code %in% q_points$code)

# filter per event and make figure
qevent <- vector("list", length = nrow(events))

for (i in seq_along(events$event_start)) {
  qevent[[i]] <- qall %>%
    filter(timestamp > events$ts_start[i] &
             timestamp < events$ts_end[i]) %>%
    left_join(q_points, by = "code")
  
  ggplot(qevent[[i]]) +
    geom_line(aes(x = timestamp, y = Q, color = naam)) +
    theme_classic()
  
  ggsave(paste0("images/discharge", events$event_start[i], ".png"))
  
}

# save the event summary data to a csv file
event_summary <- bind_cols(events, event_summary)
write_csv(event_summary, "data/processed_data/stats_selected_events.csv")

