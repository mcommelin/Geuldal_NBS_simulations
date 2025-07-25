#' Visualize discharge and precipitation for selected events in the
#' Geul catchment. And prepare precipitation input and calibration
#' timeseries for the LISEM simulations.
#' 1. organize the data so we can select interesting events
#' 2. Visualize discharge and precipitation for these events:
#'  i. make a gif of the precipitation over time
#'  ii. make a graph of the discharge in the measurement points in
#'      the main streams
#'  iii. create summary stats of the events


# Initialization ---------------------------------------------------------------
library(gifski)
library(hms)
library(gdalUtilities)
library(raster)
library(sf)
library(RColorBrewer)
library(tidyverse)

# load pcraster functions
source("sources/r_scripts/pcrasteR.R")
set_pcraster(env = "qgis", miniconda = "~/ProgramFiles/miniconda3")

#functions


# 1. Select events --------------------------------------------------------------

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
q_all <- q_all %>%
  mutate(wh = if_else(wh < 40, wh * 10, wh)) %>%
  mutate(wh = if_else(wh > 40000, wh / 1000, wh)) %>%
  mutate(wh = if_else(wh > 4000, wh / 100, wh)) %>%
  mutate(wh = if_else(wh > 400, wh / 10, wh))



# filter in the waterhieght datasets to find:
# 1. the events with high discharges at the outlet of the Geul in Meersen
b <- q_all %>%
  filter(str_detect(name_long, "Maastrichterlaan")) %>%
  filter(timestamp > "2019-01-01 12:00:00" &
           timestamp < "2024-12-30 12:00:00") %>%
  filter(wh > 44.2)
# 6 events with high discharge
ggplot(b) +
  geom_point(aes(x = timestamp, y = wh))

# 2. shorter peaks in the Eyserbeek
# from here we select 3 smaller events.
b <- q_all %>%
  filter(str_detect(name_long, "meetgeet_Eys")) %>%
  filter(timestamp > "2024-05-02 12:00:00" &
           timestamp < "2024-05-05 18:00:00") %>%
  filter(wh > 40.7)

ggplot(b) +
  geom_point(aes(x = timestamp, y = wh))

# the events + duration are added to 'sources/selected_events.csv'


wh_ev23 <- q_all %>%
  filter(timestamp > events$ts_start[1] &
           timestamp < events$ts_end[1]) %>%
  group_by(code) %>%
  mutate(wh = wh - min(wh, na.rm = T),
         whmax = max(wh, na.rm = T) - min(wh, na.rm = T)) %>%
  #filter(type != "buffer") %>%
  filter(naam == "Watervalderbeek")

ggplot(wh_ev23) +
  geom_line(aes(x = timestamp, y = wh, color = name_long)) + 
  theme_classic()

# 2 Visualize precipitation and discharge during events ------------------------

## setup loop over events -----------------------------------------------------
#load selected events
events <- read_csv("sources/selected_events.csv") %>%
  mutate(ts_start = ymd_hms(event_start),
         ts_end = ymd_hms(event_end))

event_summary <- tibble(pmax = c(), ptot = c(), maxQ = c())

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
  filter(code %in% q_points$code)

# filter per event and make figure
qevent <- vector("list", length = nrow(events))

# add catchment outline
gpx_line <- st_read("data/line_catchment.gpx", layer = "tracks") 
# Reproject the GPX line to match the raster CRS (EPSG:28992)
gpx_line <- st_transform(gpx_line, crs = 28992)

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
  
  # calculate the mean total precipitation per eventfor the summary stats
  rain_knmi <- stack(paste0("data/raw_data/neerslag/KNMI_radar_1uur/", map_names))
  sum_raster <- calc(rain_knmi, sum, na.rm = TRUE)
  ptot <- cellStats(sum_raster, stat = "mean", na.rm = TRUE)
  
  ev_name <- as.character(event_start) %>%
    str_remove_all("-") %>%
    str_extract("^([0-9]{8})") %>%
    paste0("rain_", .)
  
  
  ## GIF of precipitation events -------------------------------------------------
  if (!dir.exists(paste0("images/neerslag/", ev_name))) {
    dir.create(paste0("images/neerslag/", ev_name))
  }
  
  colors <- brewer.pal(9, "Blues")
  breakpoints <- c(0, 1, 3, 5, 10, 18, 26, 40, 60)
  
  # sum raster for total precipitation map
  maxp <- 0
  for (i in seq_along(map_names)) {
    rain_map <- raster(paste0("data/raw_data/neerslag/KNMI_radar_1uur/", map_names[i]))
    png(filename = paste0("images/neerslag/", ev_name, "/", rain_gifs[i]))
    # Plot the raster
    plot(rain_map, col = colors, breaks = breakpoints, main = paste0(hours[i]))
    
    # Overlay the GPX line
    plot(st_geometry(gpx_line), add = TRUE, col = "black", lwd = 2) # Adjust color and line width as needed
    
    dev.off()
    mp <- max(as.data.frame(rain_map))
    maxp <- if (mp > maxp)
      mp
    else
      maxp
  }
  
  # make a gif
  files <- paste0("images/neerslag/", ev_name, "/", rain_gifs)
  gifski(
    files,
    gif_file = paste0("images/neerslag/", ev_name, ".gif"),
    delay = 0.3
  )
  
## Discharge figure events ----------------------------------------------------
  qevent[[k]] <- qall %>%
    filter(timestamp > events$ts_start[k] &
             timestamp < events$ts_end[k]) %>%
    left_join(q_points, by = "code")
  
  ggplot(qevent[[k]]) +
    geom_line(aes(x = timestamp, y = Q, color = naam)) +
    theme_classic()
  
  ggsave(paste0("images/discharge", events$event_start[k], ".png"))
  
  # get max discharge at Meersen
  qmeersen <- qevent[[k]] %>%
    filter(naam == "Meersen")
  maxq <- max(qmeersen$Q, na.rm = TRUE)

  # make a summary table of the events
  sum_ev <- tibble(pmax = maxp, ptot = ptot, maxQ = maxq)
  event_summary <- bind_rows(event_summary, sum_ev)
  
}

# save the event summary data to a csv file
event_summary <- bind_cols(events, event_summary)
write_csv(event_summary,
          "data/processed_data/stats_selected_events.csv")

# 3. Load high res Q and H data for selected events --------------------------

# load the available Q data
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

# load selected events  
events <- read_csv("sources/selected_events.csv") %>%
  mutate(ts_start = ymd_hms(event_start),
         ts_end = ymd_hms(event_end)) %>%
  filter(use != "none")

qevent <- vector("list", length = nrow(events))

for (k in seq_along(events$event_start)) {
  qevent[[k]] <- qall %>%
    filter(timestamp > events$ts_start[k] &
             timestamp < events$ts_end[k])
  
  ggplot(qevent[[k]]) +
    geom_line(aes(x = timestamp, y = Q, color = name_long)) +
    theme_classic()
}


