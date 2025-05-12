# Prepare the base maps from processed spatial data for LISEM
# the maps are converted to PCRaster format and clipped to the correct catchment
# size.

# Current list of base maps:
# - dem.map: digital elevation model
# - mask.map: mask map on which all other maps extents are based
# - landuse.map: land use map
# - soil.map: soil map
# - subcatch.map: subcatchment map

#Note: curently only the subcatchment map is created, the other maps are already
# manually created.

# Initialization ------------------------------------------------------------
library(sf)
library(tidyverse)

source("sources/r_scripts/pcrasteR.R")
set_pcraster(env = "qgis", miniconda = "~/ProgramFiles/miniconda3")

#set digits to 10 for detail in coordinates
options(digits = 10)


# 1. create subcatchment maps -------------------------------------------------

# load the outpoints csv file
# if more subcatchment or outpoints are required, these can manually be added
# to this file
points <- read_csv("LISEM_data/tables/outpoints_description.csv")
cell_size <- unique(points$cell_size)

# loop over resolutions
for (j in seq_along(cell_size)) {
  subdir <- paste0("LISEM_data/Geul_", cell_size[j], "m/maps/")
  res <- cell_size[j]
  # filter the correct resolution
  points_res <- points %>%
    filter(cell_size == res) %>%
    select(x, y, point)
  # write csv table
  write_csv(points_res, file = paste0(subdir, "outpoints.txt"),
            col_names = FALSE)
  # run col2map
  col2map(col_in = "outpoints.txt", map_out = "outpoints.map",
          sub_dir = subdir, options = "-N")
  # make the subcatchment map
  pcrcalc(
    work_dir = subdir,
    options = paste0("'subcatch.map=subcatchment(ldd.map, outpoints.map)'")
  )
  
  # clean up
  # remove outpoints.txt
  file.remove(paste0(subdir, "outpoints.txt"))
  # remove all aux.xml files
  aux_files <- list.files(subdir, pattern = "aux.xml", full.names = TRUE)
  if (length(aux_files) > 0) {
    file.remove(aux_files)
  }
}


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
road_surfaces <- read_csv("sources/GIS_manual/road_surface_types.csv")

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
road_types <- read_csv("sources/GIS_manual/road_types.csv")

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
road_widths <- read_csv("sources/GIS_manual/road_widths.csv")

# add road widths to osm data
osm_data <- osm_data %>%
  left_join(road_widths, by = "highway") %>%
  select(highway, hard_surface, compacted, m_width, width) %>%
  mutate(width = as.numeric(width)) %>%
  mutate(m_width = if_else(is.na(width), m_width, width))

# save the roads data
st_write(osm_data, "data/processed_data/GIS_data/roads_buildings.gpkg", layer = "roads_region", delete_layer = TRUE)

# 3. channels and culverts -----------------------------------------------------

# load osm data culverst and waterways
osm_waterways <- st_read("data/osm_data.gpkg", layer = "waterways")
osm_culverts <- st_read("data/osm_data.gpkg", layer = "culverts")

a <- osm_waterways %>%
  select(waterway, width, intermittent)

b <- osm_culverts %>%
  select(waterway, width, intermittent)
