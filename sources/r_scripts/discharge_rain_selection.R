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
library(RColorBrewer)


# # load pcraster functions
# source("sources/r_scripts/pcrasteR.R")
# set_pcraster(env = "qgis", miniconda = "~/ProgramFiles/miniconda3")

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
  filter(wh > 44)
# 6 events with high discharge
ggplot(b) +
  geom_point(aes(x = timestamp, y = wh))

# 2. shorter peaks in the Eyserbeek
# from here we select 3 smaller events.
b <- q_all %>%
  filter(str_detect(name_long, "meetgoot_Eys")) %>%
  filter(timestamp > "2018-04-29 12:00:00" &
           timestamp < "2018-05-02 18:00:00") %>%
  filter(wh > 101.5)

ggplot(b) +
  geom_point(aes(x = timestamp, y = wh))

# 3. additional calibration event Gulp
# 
b <- q_all %>%
  filter(str_detect(name_long, "Gulp_Gulpen_Azijnfabriek")) %>%
  filter(timestamp > "2015-01-01 12:00:00" &
           timestamp < "2024-12-30 12:00:00") %>%
  filter(wh > 91)

ggplot(b) +
  geom_point(aes(x = timestamp, y = wh))

p_events <- b %>%
  mutate(date = date(timestamp)) %>%
  group_by(date) %>%
  distinct(date)
# save all events with a bit higher runoff
# manually select what looks good. follow up in section 4
write_csv(p_events, "p.csv")

# the events + duration are added to 'sources/selected_events.csv'
events <- read_csv("sources/selected_events.csv")

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
         ts_end = ymd_hms(event_end)) %>%
  filter(use == "test")

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
  #filter(timestamp > "2020-01-01") %>%
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

# 4. Third event Gulp ------------------------------------------------


# load selected events from section 1
p_events <- read_csv("p.csv") %>%
  mutate(start_date = date - days(1),
         end_date = date + days(2))

# load q from section 1
qgulp <- qall %>% 
  filter(str_detect(name_long, "Gulp_Azijnfabriek_debiet"))

# make q plots for all selected dates
for(i in seq_along(p_events$date)) {
qdat <- qgulp %>%
  filter(date(timestamp) >= p_events$start_date[i] & 
           date(timestamp) <= p_events$end_date[i])
ggplot(qdat) +
  geom_point(aes(x = timestamp, y = Q))
ggsave(paste0("images/gulp_event/q_", p_events$date[i], ".png"))

}

#make hour rainfall for all p_events dates
# the available hour rain data only works after 2019
events <- p_events %>%
  mutate(ts_start = as_datetime(start_date),
         ts_end = as_datetime(end_date)) %>%
  filter(year(date) > 2019)

# get rain ID's for the Gulp sub catchment.
wdir <- "LISEM_runs/Gulp_10m/maps/"
# map2asc
map2asc(map_in = "ID.map",
        map_out = "rain_ID.asc",
        sub_dir = wdir)
# find unique grid id's
rainIDs <- raster(paste0(wdir, "rain_ID.asc"))
id <- as.vector(rainIDs)
freq <- as_tibble(table(id)) %>%
  mutate(id_nm = paste0("gauge_", id))


# plot Q and hourly P
# load rain data
for(i in seq_along(events$date)) {
ev_date <- as.character(events$start_date[i])
date_str <- str_remove_all(ev_date, "-")

  pfile <- paste0("LISEM_runs/rain/rain_", date_str, ".txt")

skipval <- as.numeric(readLines(pfile)[2]) + 2
rain_txt <- readLines(pfile)[-(1:skipval)]
nms <- readLines(pfile)[3:(skipval)] %>%
  str_replace_all(., " ", "_")
# make a tibble with numbers and row names
a <- str_split(rain_txt, " ")
b <- as_tibble(do.call(rbind, a)) %>%
  rename_with( ~ nms) %>%
  mutate(across(-time, as.numeric)) %>%
  mutate(doy = str_remove(time, ":\\d\\d\\d\\d"),
         mod = str_remove(time, "\\d\\d\\d:"),
         hours = str_pad(floor(as.numeric(mod)/60), width = 2, side = "left", pad = "0"),
         mins = str_pad(floor(as.numeric(mod) %% 60), width = 2, side = "left", pad = "0"),
         date = as.Date(as.numeric(doy), origin = paste0(year(events$date[i]), "-01-01")),
         datestring = paste0(date, " ", hours, ":", mins),
         timestamp = ymd_hm(datestring) - days(1)) %>%
  select(timestamp, all_of(freq$id_nm))

c <- b %>%
  pivot_longer(cols = -timestamp,
               values_to = "P",
               names_to = "id_nm") %>%
  left_join(freq, by = "id_nm") %>%
  mutate(Ptmp = P * n) %>%
  group_by(timestamp) %>%
  summarize(P = round(sum(Ptmp) / sum(n), digits = 2))

# make figure
  total = round(sum(c$P), digits = 2)

  qdat <- qgulp %>%
    filter(date(timestamp) >= events$start_date[i] & 
             date(timestamp) <= events$end_date[i])
  
  
ggplot() +
  geom_bar(data = c, aes(x = timestamp, y = P), stat = "identity") +
  geom_line(data = qdat, aes(x = timestamp, y = Q)) +
  theme_classic() +
  labs(x = "time", y = "P mm/h")
ggsave(paste0("images/gulp_event/q_p", events$date[i], ".png"))

}


#download KNMI radar for events
#use the separate script for this and add events to
# the csv file with 5 minute rain data.






# Prepare all combinations of points, events, and temporal resolutions
Tres <- c("hour", "min")
combos <- expand.grid(
  point = points_id,
  event = events$ts_start,
  tres = Tres,
  stringsAsFactors = FALSE
)

rain_list <- vector("list", nrow(combos))

for (x in seq_len(nrow(combos))) {
  point_id <- combos$point[x]
  event_idx <- combos$event[x]
  tres_val <- combos$tres[x]
  
  # select subcatchment
  subcatch <- points %>%
    filter(point == point_id) %>%
    filter(cell_size == 10)
  subcatch_name <- subcatch$subcatch_name
  wdir <- paste0("LISEM_runs/", subcatch_name, "_10m/maps/")
  evdate <- date(combos$event[x])
  
  rain_list[[x]] <- subcatch_rain_compare(
    wdir = wdir,
    ev_date = evdate,
    tres = tres_val
  )
}

# combine the plots
# Extract total values from rain_list (assuming total is in the 2nd position)
combos$total <- sapply(rain_list, function(x) x[[2]])
x = 9
for (i in 1:x) {
  
  plot_grid(rain_list[[i]][[1]], rain_list[[i+x]][[1]],
            nrow = 2, align = "hv")
  ggsave(paste0("images/rain_compare_", date(combos$event[i]), "_", combos$point[i], ".png"))
  
}

# 5. explore possible events - Q and hourly P

point = 4 # Gulp
evdate = "20230401" # or select from 'selected_events.csv'

# load Q obs
 # qall from section 3

# find rain from hourly maps

# plot


#load selected events
events <- read_csv("sources/selected_events.csv") %>%
  mutate(ts_start = ymd_hms(event_start),
         ts_end = ymd_hms(event_end)) %>%
  filter(use == "test")

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
  
  # calculate the mean total precipitation per event for the summary stats
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
