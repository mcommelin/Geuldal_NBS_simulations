#' load discharge and rainfall data for points in Geul catchment
#' 1. organize the data so we can select interesting evcents
#' 2. add functionality to display discharge data on a map - shiny?

## initialization ---------
library(tidyverse)


## process discharge data ---------------

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

# Possible events:
# 2 mei 2024
# 2021
# 

# test plot
ggplot(b) +
  geom_point(aes(x = timestamp, y = wh))
          