# make lisem input for sub catchment

# Initialization --------------------------------------------------------------
library(gdalUtilities)
library(terra)
library(tidyverse)

source("sources/r_scripts/pcrasteR.R")
set_pcraster(env = "qgis", miniconda = "~/ProgramFiles/miniconda3")

# Function to create subcatchment base maps -----------------------------------
base_maps_subcatchment <- function(
    cell_size = NULL,
    sub_catch_number = NULL # adjust the number to select the subcatchment you want
    ) {

  
# load subcatchment points csv file
points <- read_csv("LISEM_data/tables/outpoints_description.csv")

res = cell_size

# select subcatchment
subcatch <- points %>%
  filter(point == sub_catch_number) %>% 
  filter(cell_size == res)
subcatch_name <- subcatch$subcatch_name # give best describing name for the subcatchment
# create dir for subcatch
sub_catch_dir <- paste0("LISEM_data/subcatchments/", subcatch_name, "_", res, "m/maps/")
main_dir <- paste0("LISEM_data/Geul_", res, "m/maps/")


if (!dir.exists(sub_catch_dir)) {
  dir.create(sub_catch_dir, recursive = TRUE)
}

# copy base maps from main_dir to new subcatch dir
base_maps <- c("dem.map", "mask.map", "landuse.map", "soils.map", 
               "catchment.map", "ID.map", "buildings.map", "subcatch.map")
# add "base" suffix to the base maps names in the subcatch dir
for (i in seq_along(base_maps)) {
  file.copy(
    from = paste0(main_dir, base_maps[i]),
    to = paste0(sub_catch_dir, "base_", base_maps[i]),
    overwrite = TRUE
  )
}

# pcraster filter subcatch
pcrcalc(
  work_dir = sub_catch_dir,
  options = paste0("'sub.map=boolean(if(base_subcatch.map eq ", subcatch$point, ", 1))'")
)

# map2asc
map2asc(
  map_in = "sub.map",
  map_out = "sub.asc",
  sub_dir = sub_catch_dir
)

# vectorize subcatchment
ras <- rast(paste0(sub_catch_dir, "/sub.asc"))
pol <- as.polygons(ras)
writeVector(pol, paste0(sub_catch_dir, "/sub.shp"), overwrite = TRUE)

# use polygon of subcatchment to cut the raster
# general settings
srs = "EPSG:28992"
method = "near"

# gdal warp with cutline
gdalwarp(
  srcfile = paste0(sub_catch_dir, "/sub.asc"),
  dstfile = paste0(sub_catch_dir, "/catchment.asc"),
  s_srs = srs,
  t_srs = srs,
  tr = rep(res, 2),
  cutline = paste0(sub_catch_dir, "/sub.shp"),
  crop_to_cutline = T,
  of = "AAIgrid",
  r = method,
  dryrun = F,
  overwrite = T
)

# make pcraster mask

# read the catchment.asc
options(digits = 10)
subcatch_header <- readLines(paste0(sub_catch_dir, "/catchment.asc"), n = 5)
# extract only digits or '.' from the strings in subcatch_header
subcatch_header <- gsub("[^0-9.]", "", subcatch_header)

newmap(
  nrows = as.numeric(subcatch_header[2]),
  ncols = as.numeric(subcatch_header[1]),
  xulc = as.numeric(subcatch_header[3]),
  yulc = as.numeric(subcatch_header[4]) + as.numeric(subcatch_header[2]) * res,
  cellsize = res,
  dir = sub_catch_dir,
)

# convert catchment.asc to pcraster
asc2map(
  map_in = "catchment.asc",
  map_out = "catchment.map",
  sub_dir = sub_catch_dir
)

# resample the base maps to the new mask.map
for (i in seq_along(base_maps)) {
  resample(
    clone = "mask.map",
    map_in = paste0("base_", base_maps[i]),
    map_out = base_maps[i],
    dir = sub_catch_dir
  )
}

# remove all intermediate maps and files
file.remove(
  list.files(
    path = sub_catch_dir,
    pattern = "base_|sub|catchment\\.asc|\\.prj",
    full.names = TRUE
  )
)

}
