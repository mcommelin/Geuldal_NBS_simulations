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

# 1. Select events --------------------------------------------------------------

# make file list
q_file_dir = "data/GeulStroomgebied_Tijdreeksen/metingen"
q_files <- dir(q_file_dir, recursive = TRUE,
               pattern = ".csv$")

# load file with station names and characteristics
station_names <- read_csv("data/GeulStroomgebied_Tijdreeksen/naam_meetpunten_Geuldal.csv")
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

# trailing zeros are trimmed and decimal sign is removed - ad them back.
q_all <- q_all %>%
  mutate(wh = if_else(wh < 40, wh * 10, wh)) %>%
  mutate(wh = if_else(wh > 40000, wh / 1000, wh)) %>%
  mutate(wh = if_else(wh > 4000, wh / 100, wh)) %>%
  mutate(wh = if_else(wh > 400, wh / 10, wh))

b <- q_all %>%
  filter(str_detect(code, "")) %>%
  filter(timestamp > "2020-01-10 12:00:00" & timestamp < "2024-05-30 12:00:00")




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

