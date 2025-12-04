#' this file contains code to prepare base layers for the Geuldal simulations with
#' OpenLISEM. these steps normally don't have to be done, and the resulting files
#' can be found in ./spatial_data

# Initialization ------------------------------------------------------------
library(sf)
library(tidyverse)

source("sources/r_scripts/pcrasteR.R")
set_pcraster(env = "lisem", miniconda = "~/ProgramFiles/miniconda3")

#set digits to 10 for detail in coordinates
options(digits = 10)


# 1. create (sub)catchment maps -------------------------------------------------
# WARNING !! the whole section 1 has moved to functions in 'source_to_base_maps.R'

# # 1.1 delineate catchment
# # load base layers spatial data
# mask20 <- rast("spatial_data/mask_20m.map")
# base_dem_5m <- rast("spatial_data/dem_region_5m.map")

#!!!!!!!!!!!!!!!!!!!!!!!
# Code below to show workflow, not used due to slightly different results!!!

# # resample dem with terra
# base_dem_20 <- terra::resample(base_dem_5m, mask20, method = "average")
# 
# #write as PCRaster map
# writeRaster(base_dem_20, "spatial_data/dem_region_20m.map" , filetype = "PCRaster", NAflag = -9999,
#             overwrite = TRUE, gdal = "PCRASTER_VALUESCALE = VS_SCALAR")
# 
# # # resample dem with PCraster
# # resample(clone = "mask_20m.map", map_in = "dem_region_5m.map", 
# #          map_out = "dem_region_20m.map", dir = "spatial_data/")
# 
# # convert outlet coordinates to PCRaster map
# col2map(clone = "mask_20m.map", col_in = "outlet.txt", map_out = "outlet.map",
#         sub_dir = "spatial_data/")
# 
# # make a local drain direction for the whole regional dem on 20m
# pcrcalc(options = "ldd_base.map=lddcreate(dem_region_20m.map,1e31,1e31,1e31,1e31)",
#         work_dir = "spatial_data/")
# 
# # delineate the catchement based on the outlet
# pcrcalc(options = "catchment.map=cover(catchment(ldd_base.map, nominal(outlet.map)),0)",
#         work_dir = "spatial_data/")

#WARNING - the catchment map was produced early in the modelling project 
# - and newer (so the version produced with the code above) versions slightly differ at the edges 
#!!!!!!!!!!!!!!!!!!!!
# 
# ## 1.1 catchment based dem etc --------------------------------
# # load the base_catchment_20m
# base_catch_20m <- rast("spatial_data/base_catchment_20m.map")
# 
# # we use 3 resolutions for the dataset
# cell_size <- unique(points$cell_size)
# 
# # create maps for base database if they do not exist
# 
# for(i in seq_along(cell_size)) {
#   # make folder structure
#   res_dir <- paste0("LISEM_data/Geul_", cell_size[i], "m/")
#   map_dir <- paste0(res_dir, "maps/")
#   if(!dir.exists(res_dir)) {
#     dir.create(res_dir)
#     dir.create(map_dir)
#   }
#   
#   #write maps
#   #mask
#   mask <- rast(paste0("spatial_data/mask_", cell_size[i],"m.map"))
#   out <- paste0(res_dir, "maps/mask.map")
#   writeRaster(mask, out, filetype = "PCRaster", NAflag = -9999,
#                            overwrite = TRUE, gdal = "PCRASTER_VALUESCALE = VS_SCALAR")
#   #dem
#   out <- paste0(res_dir, "maps/dem.map")
#   dem <- terra::resample(base_dem_5m, mask, method = "average")
#   writeRaster(dem, out, filetype = "PCRaster", NAflag = -9999,
#               overwrite = TRUE, gdal = "PCRASTER_VALUESCALE = VS_SCALAR")
#   
#   #catchment
#   out <- paste0(res_dir, "maps/catchment.map")
#   catch <- terra::resample(base_catch_20m, mask, method = "near")
#   writeRaster(catch, out, filetype = "PCRaster", NAflag = -9999,
#               overwrite = TRUE, gdal = "PCRASTER_VALUESCALE = VS_SCALAR")
#   
#   # generate ldd
#   pcr_script("base_ldd.mod", script_dir = "sources/pcr_scripts/",
#              work_dir = map_dir)
# 
# }
# ## 1.2 make map with subcatchments ---------------------------------------------
# #based on csv file with outpoint coordinates
# 
# # load the outpoints csv file
# # if more subcatchment or outpoints are required, these can manually be added
# # to this file
# 
# 
# # loop over resolutions
# cell_size <- unique(points$cell_size)
# 
# for(i in seq_along(cell_size)) {
#   subdir <- paste0("LISEM_data/Geul_", cell_size[i], "m/maps/")
#   res <- cell_size[i]
#   # filter the correct resolution
#   points_res <- points %>%
#     filter(cell_size == res) %>%
#     select(x, y, point)
#   # write csv table
#   write_csv(points_res, file = paste0(subdir, "outpoints.txt"),
#             col_names = FALSE)
#   # run col2map
#   col2map(col_in = "outpoints.txt", map_out = "outpoints.map",
#           sub_dir = subdir, options = "-N")
#   # make the subcatchment map
#   pcrcalc(
#     work_dir = subdir,
#     options = paste0("subcatch.map=subcatchment(ldd.map, outpoints.map)")
#   )
#   
#   # clean up
#   # remove outpoints.txt
#   file.remove(paste0(subdir, "outpoints.txt"))
#   # remove all aux.xml files
#   aux_files <- list.files(subdir, pattern = "aux.xml", full.names = TRUE)
#   if (length(aux_files) > 0) {
#     file.remove(aux_files)
#   }
# }
# 



# 2. roads map clean up data ---------------------------------------------------

# Load the OSM data
osm_raw <- st_read("data/osm_data.gpkg", layer = "highway_lines")
# remove steps, proposed, elevator, services and rest_area
osm_data <- osm_raw %>%
  filter(!(highway %in% c("steps", "proposed", "elevator", "services", "rest_area")))

# Start with cleaning the data and assigning hard or caompacted surface to all
# road types.

# find different road types
road_types <- osm_data %>%
  st_drop_geometry() %>%
  select(highway, surface) %>%
  distinct() 

# save road surfaces and manually assign hard and compacted surfaces
road_surfaces <- road_types %>%
  select(surface) %>%
  distinct()

#write_csv(road_surfaces, "sources/GIS_manual/road_surface_types.csv")

# read manual assigned road surfaces
road_surfaces <- read_csv("sources/GIS_manual/road_surface_types.csv", show_col_types = FALSE)

# add new surface to road types
road_types <- road_types %>%
  left_join(road_surfaces, by = "surface")

# write road types and manually add hard and compacted surfaces for the road types
# with NA for surface
#write_csv(road_types, "sources/GIS_manual/road_types.csv")

# check for all NA surfaces if a class can visually be assigned.
a <- osm_data %>%
  filter(highway == "rest_area" & is.na(surface)) %>%
  select(highway, surface)

st_write(a, "data/temp.gpkg", layer = "temp", delete_layer = TRUE)

# read the new road types with all surfaces assigned
road_types <- read_csv("sources/GIS_manual/road_types.csv", show_col_types = FALSE)

# add new hard and compacted surfaces to osm data
osm_data <- osm_data %>%
  left_join(road_types, by = c("highway", "surface"))

# tracks and paths have extra surface information, 
# assign compacted or hard surface based on tracktype
# https://wiki.openstreetmap.org/wiki/Key:tracktype?uselang=en

tracks <- osm_data %>%
  st_drop_geometry() %>%
  filter(highway == "track" | highway == "path") %>%
  filter(is.na(surface)) %>%
  select(highway, surface, tracktype) %>%
  distinct() %>%
  mutate(hard_surface = if_else(tracktype == "grade1", 1, 0),
         compacted = if_else(tracktype != "grade1", 1, 0))

a <- osm_data %>%
  filter(is.na(hard_surface) | is.na(compacted)) %>%
  select(-hard_surface, -compacted) %>%
  left_join(tracks, by = c("highway", "surface", "tracktype")) %>%
  mutate(hard_surface = if_else(is.na(hard_surface) & highway == "track", 0, hard_surface),
         hard_surface = if_else(is.na(hard_surface) & highway == "path", 1, hard_surface),
         compacted = if_else(is.na(compacted) & highway == "track", 1, compacted),
         compacted = if_else(is.na(compacted) & highway == "path", 0, compacted))

# add new hard and compacted surfaces to osm data
osm_data <- osm_data %>%
  filter(!is.na(hard_surface) | !is.na(compacted)) %>%
  bind_rows(a) %>%
  filter(!is.na(highway))

# step 2 add the width for each highway type
# find all highway type with hard_surface
road_widths <- osm_data %>%
  st_drop_geometry() %>%
  #filter(hard_surface == 1) %>%
  select(highway, width) %>%
  mutate(width = as.numeric(width)) %>%
  group_by(highway) %>%
  summarise(m_width = mean(width, na.rm = TRUE))

# write the road width and adjust manually
#write_csv(road_widths, "sources/GIS_manual/road_widths.csv")

# read the road widths with manual adjustments
road_widths <- read_csv("sources/GIS_manual/road_widths.csv", show_col_types = FALSE)

# add road widths to osm data
osm_data <- osm_data %>%
  left_join(road_widths, by = "highway") %>%
  select(highway, hard_surface, compacted, m_width, width) %>%
  mutate(width = as.numeric(width)) %>%
  mutate(m_width = if_else(is.na(width), m_width, width))

# transform to EPSG:28992
roads <- st_transform(osm_data, crs = " EPSG:28992")

# save the roads data
st_write(roads, "data/processed_data/GIS_data/roads_buildings.gpkg", layer = "roads_region", delete_layer = TRUE)

# make polygon with buffer in QGIS and save as polygon
# with terra write to LISEM_data

# 3. channels, buffers and culverts -----------------------------------------------------

# the channel layer are digitized to 1 connected network in QGIS.
# with Lines Ranking plugin additional statistics are calculated.

# load the qgis channels layer
ranked_chan <- st_read("data/processed_data/GIS_data/channels.gpkg", layer = "rank_attributes")

# clean up and assign baseflow
chan <- ranked_chan %>%
  mutate(waterway = if_else(!is.na(STATUS) & is.na(waterway), "ditch", waterway),
         waterway = if_else(waterway == "fish_pass", "stream", waterway),
         waterway = if_else(waterway == "canal", "ditch", waterway),
         waterway = if_else(waterway == "river", "stream", waterway),
         intermittent = if_else(is.na(intermittent), "no", intermittent),
         baseflow = if_else(waterway == "stream", 1, 0),
         baseflow = if_else(intermittent == "yes", 0, baseflow))

#histogram of Shreve order to make classes for width
# see sources/GIS_manual/Schreve_width_channels.csv
shreve_lookup <- read_csv("sources/GIS_manual/Shreve_width_channels.csv", show_col_types = FALSE)

# the channel width from OSM only covers a few features and does not seem useful
# so for now we neglect it.

# add shape and dimensions.
chanshape <- chan %>%
  mutate(shape = if_else(waterway == "drain", "rectangle", "trapezium"),
         shape = if_else(waterway == "stream", "rectangle", shape),
         tunnel = if_else(tunnel == "yes", "culvert", tunnel),
         tunnel = if_else(is.na(tunnel), "no", tunnel),
         shape = if_else(tunnel == "culvert", "round", shape)) %>%
  rename(width_osm = width)

chandim <- left_join(chanshape, shreve_lookup, 
                     join_by(closest(ValueShreve >= ClassShreve))) %>%
  #mutate(width = if_else(tunnel == "culvert", diameter, width)) %>%
  select(waterway, width, depth, shape, tunnel, diameter, baseflow) %>%
  mutate(culvert_bool = if_else(tunnel == "culvert", 1, 0),
         chan_type = if_else(waterway == "stream", 1, 2))

st_write(chandim, "spatial_data/channel_buffer.gpkg", layer = "channels", delete_layer = TRUE)

# # for baseflow make a new layer that only contains channel sections with baseflow
# chan_bf <- chandim %>%
#   filter(baseflow == 1)
# st_write(chan_bf, "data/processed_data/GIS_data/channels.gpkg", layer = "channels_baseflow", delete_layer = TRUE)
# 


## 3.1 buffer features --------------------------------------------------------

# load WL data (not openly available)
wl_data_dir <- "data/data_wl/Data_buffers_Geul_openLisem_WRL/"

# hoogtelijnen buffers
# for ease of operation the hoogtelijnen are converted to polygons in QGIS
hoogtel <- st_read(paste0(wl_data_dir, 
                          "DAMO_buffers_openLisem_Geul_20251017.gpkg"), 
                   layer = "hoogtelijnen_polygon")
# afsluitmiddel_Geul
afsluit <- st_read(paste0(wl_data_dir, 
                          "DAMO_buffers_openLisem_Geul_20251017.gpkg"), 
                   layer = "afsluitmiddel_Geul")
# duikersifonhevel_Geul
duiker <- st_read(paste0(wl_data_dir, 
                          "DAMO_buffers_openLisem_Geul_20251017.gpkg"), 
                   layer = "duikersifonhevel_Geul")


# from the general dataset load the buffer features:
buffers <- st_read("data/processed_data/GIS_data/channels.gpkg", layer = "buffers")

# merge buffers and hoogtelijnen - select hoogtelijnen if available
hl <- hoogtel %>%
  st_drop_geometry() %>%
  mutate(RegenwaterbufferCOMPCode = as.character(RegenwaterbufferCOMPCode))
hl2 <- hoogtel %>%
  rename('CODE' = 'RegenwaterbufferCOMPCode') %>%
  select(CODE) %>%
  mutate(CODE = as.character(CODE))

buffeat <- buffers %>%
  anti_join(hl, by = c('CODE' = 'RegenwaterbufferCOMPCode')) %>%
  select(CODE) %>%
  bind_rows(hl2) %>%
  st_make_valid()

# after saving manual edits in QGIS are done to solve errors that are hard to code
# so in general don't overwrite this layer except is you are sure that this is needed!
# save for QGIS manual editing
#st_write(buffeat, "spatial_data/channel_buffer.gpkg", layer = "buffers",
#         delete_layer = T)

# select all duikers in buffer
duik_buf <- duiker %>%
  select(HOOGTEOPENING) %>%
  st_filter(buffeat, .predicate = st_intersects)


# add 50 meter buffer around selected buffers to include correct afsluiter points
af_range <- afsluit %>%
  st_buffer(5) %>%
  select(WS_SCHUIFHOOGTEINST, CODE)

# select all afsluitmiddel for duikers in buffer
# calculate new culvert diameter based on orig diameter and height of weir
af_buf <- duik_buf %>%
  st_join(af_range) %>%
  mutate(WS_SCHUIFHOOGTEINST = if_else(WS_SCHUIFHOOGTEINST > 2, 
                                       WS_SCHUIFHOOGTEINST / 100, 
                                       WS_SCHUIFHOOGTEINST)) %>%
  group_by(CODE) %>%
  summarise(WS_SCHUIFHOOGTEINST = mean(WS_SCHUIFHOOGTEINST, na.rm = T),
            HOOGTEOPENING = mean(HOOGTEOPENING, na.rm = T)) %>%
  ungroup() %>%
  mutate(WS_SCHUIFHOOGTEINST = if_else(is.na(WS_SCHUIFHOOGTEINST), 
                                       0.1, 
                                       WS_SCHUIFHOOGTEINST),
         HOOGTEOPENING = if_else(is.na(HOOGTEOPENING), 
                                       0.4, 
                                       HOOGTEOPENING),
         r = HOOGTEOPENING / 2,
         h = HOOGTEOPENING - WS_SCHUIFHOOGTEINST,
         theta = 2 * acos((r - h)/r),
         A = pi * r^2 - (r^2 * (theta - sin(theta)))/2,
         d_new = round(sqrt(A/pi) * 2, digits = 2),
         d_new = if_else(d_new < 0.07, 0.07, d_new)) %>% # culverts cannot be fully closed.
  select(CODE, d_new)

# after saving manual edits in QGIS are done to solve errors that are hard to code
# so in general don't overwrite this layer except is you are sure that this is needed!
# save for QGIS manual editing
#st_write(af_buf, "spatial_data/channel_buffer.gpkg", layer = "buffer_outlet",
#         delete_layer = T)

## 3.2 natural ponds -----------------------------------------------------------

# load osm natural water data
osm_water <- st_read("data/osm_data.gpkg", layer = "natural_water") %>%
  st_transform(crs = "EPSG:28992")

# filter only the ponds; remove stream, river, wastewater etc
unique(osm_water$water)

pond_types <- c("lake", "pond", "moat", "basin", "reservoir", "unknown")

ponds <- osm_water %>%
  select(water) %>%
  mutate(water = if_else(is.na(water), "unknown", water)) %>%
  filter(water %in% pond_types)

st_write(ponds, "spatial_data/channel_buffer.gpkg", layer = "ponds",
         delete_layer = T)

# in the PCRaster code the ponds that are within the DHydro domain are removed.
# mainly because around the Geul river some ponds don't function as buffer.


# 4. stormdrains ---------------------------------------------------------------

# Load WL data
# load WL data
wl_data_dir <- "data/data_wl/Data_buffers_Geul_openLisem_WRL/"
# riooleringsgebied
riool_NL <- st_read("spatial_data/urban.gpkg", 
                    layer = "riool_NL_geul")

# overstorten
over_NL <- st_read(paste0(wl_data_dir, 
                          "GEU_paved_data_update01_20251017.gpkg"), 
                   layer = "GEU_overstorten_DHydamo")
o2 <- st_drop_geometry(over_NL)

r2 <- riool_NL %>%
  left_join(o2, by = c("code" = "codegerelateerdobject")) %>%
  group_by(code) %>%
  select(code, berging_in_riolering_totaal_mm) %>%
  summarise(berging_in_riolering_totaal_mm = sum(berging_in_riolering_totaal_mm))

# 
# st_write(r2, "spatial_data/urban.gpkg", 
#          layer = "riool_NL_geul_corrected")
# mean storage in NL for other regions:
mean(r2$berging_in_riolering_totaal_mm, na.rm = T)
# 11.7 mm

#combine with build_up_area outside NL to tilestorage map.
r_other <- st_read("spatial_data/urban.gpkg", layer = "riool_other_geul") %>%
  mutate(berging = 11.7) %>%
  select(berging)

r_nl <- r2 %>%
  select(berging_in_riolering_totaal_mm) %>%
  rename("berging" = "berging_in_riolering_totaal_mm")

r_all <- r_other %>%
  bind_rows(r_nl)
# save new riool file
st_write(r_all, "spatial_data/urban.gpkg", layer = "tilestorage")

# further calculations in PCRASTER code

#calculate drain diameter based on the Strahler order of the 5 m resolution
#assign buffer capacity to all pits (ldd = 5) based on the upstream area