# Script to download 5 min radar precipitation from KNMI and select data the Geul catchment

#load packages
library(tidyverse)
library(lubridate)
library(httr)
library(terra)
library(sf)
library(sp)
library(mapview)
library(rhdf5) #part of Bioconductor install with BiocManager::install("rhdf5")

#turn of stringsAsFactors
options(stringsAsFactors = FALSE)

# Download RADAR from KNMI API --------------------------------------------

#use anonymous key from: https://developer.dataplatform.knmi.nl/get-started
key <- "eyJvcmciOiI1ZTU1NGUxOTI3NGE5NjAwMDEyYTNlYjEiLCJpZCI6ImVlNDFjMWI0MjlkODQ2MThiNWI4ZDViZDAyMTM2YTM3IiwiaCI6Im11cm11cjEyOCJ9"
#download files from KNMI data portal
events <- read_csv("sources/selected_events.csv") %>%
  mutate(ts_start = ymd_hms(event_start),
         ts_end = ymd_hms(event_end)) %>%
  filter(use != "none")
days_list <- vector("list", length = length(events$event_start))
for (j in seq_along(events$event_start)) {
  days_list[[j]] <- seq(date(events$ts_start[j]), date(events$ts_end[j]), by = 'days') %>%
    as.character() %>%
    str_remove_all("-")
}
dates <- unlist(days_list)
# download raster data for all selected events
for (i in seq_along(dates)) {
  date2 <- str_remove_all(as.character(ymd(dates[i]) + 1), "-")
  file <- str_c("RAD25_OPER_R___TARRRF__L2__", dates[i], "T080500_", date2, "T080000_0001.tar")
  file_save <- str_c("data/radar/", file)
  url <- str_c("https://api.dataplatform.knmi.nl/open-data/datasets/nl_rdr_data_rfcor_5m/versions/1.0/files/", file, "/url")
  r <- GET(url = url, add_headers(Authorization = key))
  con <- content(r, "parsed")
  url_2 <- con$temporaryDownloadUrl
  GET(url =  url_2, write_disk(file_save, overwrite = T))
}

# the api endpoints for the three different datasets
# early reanalysis
# https://api.dataplatform.knmi.nl/open-data/datasets/nl_rdr_data_recor_5m/versions/1.0/files
# final reanalysis
# https://api.dataplatform.knmi.nl/open-data/datasets/nl_rdr_data_rfcor_5m/versions/1.0/files
# archive
# https://api.dataplatform.knmi.nl/open-data/datasets/nl_rdr_data_rtcor_5m_tar/versions/1.0/files
# 2018 from the old archive:
# https://dataplatform.knmi.nl/catalog/datasets/index.html?x-dataset=rad_nl25_rac_mfbs_em_5min&x-dataset-version=2.0


#untar files
for (i in seq_along(dates)) {
  date2 <- str_remove_all(as.character(ymd(dates[i]) + 1), "-")
  file <- str_c("RAD25_OPER_R___TARRRF__L2__",
                dates[i],
                "T080500_",
                date2,
                "T080000_0001.tar")
  file_save <- str_c("data/radar/", file)
  untar(file_save, exdir = "data/radar/h5/")
}


#seems to be 1 day later stored in file than file name???


# Save precipitation for Geulcatchment only --------------------------------------
# find pixels in catchment
# the file with radar pixel coordinates was received from Claudia Brauwer - HWM group - WUR
df <- read.table("sources/radarcoordinaten_NL25_1km2_RD.txt", col.names = c("row","col","x","y","V5")) %>%
  select(-V5) %>%
  mutate(latitude = x * 1000,
         longitude = y * 1000)
coordinates(df) <- ~ latitude + longitude
proj4string(df) = CRS("+init=epsg:28992")
df <- st_as_sf(df)
# load raster of cell ids
ras <- rast("data/processed_data/ID_zones_KNMI_radar.asc")
ras <- ras * 0 + 1
pol <- as.polygons(ras)
crs(pol) <- "epsg:28992"
ca_map <- st_as_sf(pol) %>%
  rename(id = ID_zones_KNMI_radar) 
# check which radar pixels fall in catchment
pixels <- st_join(df, ca_map) %>%
  filter(!is.na(id))
st_write(pixels, "data/processed_data/GIS_data/KNMI_radar_points.gpkg", "pixels", append = F)

# plot the catchment with points the radar pixels that fall inside the catchment
p <- mapView(pixels, color="red")
c <- mapView(ca_map)
p + c

# read HDF5 files list
files <- dir("data/radar/h5", pattern = "\\.h5$", full.names = T)
P <- matrix(nrow = length(files), ncol=nrow(pixels))
datetime = vector("character", length = length(files))
for(i in seq_along(files)){
  print(round(i/length(files)*100))
  dat <- h5read(files[i], "image1")
  d <- dat$image_data
  P[i,] <- d[cbind(pixels$col, pixels$row)]
  datetime[i] = str_extract(files[i], "20\\d*")
  h5closeAll()
}
P[P == 65535] = NA
P <- P/100 # change data to mm KNMI data is given in accumulations of 0.01mm

rain <- as_tibble(P, .name_repair = "unique")
names(rain) <- str_extract(names(rain), "\\d.*")
timestmp <- as_tibble_col(datetime, column_name = "timestamp")
rain <- bind_cols(rain, timestmp) %>%
  mutate(timestamp = ymd_hm(timestamp))

write_csv(rain, "data/raw_data/neerslag/KNMI_rain_5min.csv")
