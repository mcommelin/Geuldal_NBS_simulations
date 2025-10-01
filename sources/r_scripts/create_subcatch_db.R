# make lisem input for sub catchment

# Initialization --------------------------------------------------------------
# Function to create subcatchment base maps -----------------------------------
base_maps_subcatchment <- function(
    cell_size = NULL,
    sub_catch_number = NULL, # adjust the number to select the subcatchment you want
    calc_ldd = FALSE
    ) {

# load subcatchment points csv file
points <- read_csv("sources/setup/outpoints_description.csv")

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
base_maps <- readLines("sources/base_maps.txt")
# add "base" suffix to the base maps names in the subcatch dir
for (i in seq_along(base_maps)) {
  file.copy(
    from = paste0(main_dir, base_maps[i]),
    to = paste0(sub_catch_dir, "base_", base_maps[i]),
    overwrite = TRUE
  )
}

# delineate the subcatchment

# pcraster create map with selected outpoint
pcrcalc(
  work_dir = sub_catch_dir,
  options = paste0("sub_point.map=boolean(if(base_outpoints.map eq ", subcatch$point, ", 1))")
)
# delineate the subcatchment
pcr_script(
  script = "delineate_catchment.mod",
  script_dir = "sources/pcr_scripts",
  work_dir = sub_catch_dir
)

# map2asc
map2asc(
  map_in = "sub.map",
  map_out = "sub.asc",
  sub_dir = sub_catch_dir
)

# vectorize subcatchment
ras <- rast(paste0(sub_catch_dir, "sub.asc"))
pol <- as.polygons(ras)
writeVector(pol, paste0(sub_catch_dir, "sub.shp"), overwrite = TRUE)

# use polygon of subcatchment to cut the raster
# general settings
srs = "EPSG:28992"
method = "near"

# gdal warp with cutline
gdalwarp(
  srcfile = paste0(sub_catch_dir, "sub.asc"),
  dstfile = paste0(sub_catch_dir, "catchment.asc"),
  s_srs = srs,
  t_srs = srs,
  tr = rep(res, 2),
  cutline = paste0(sub_catch_dir, "sub.shp"),
  crop_to_cutline = T,
  of = "AAIgrid",
  r = method,
  dryrun = F,
  overwrite = T
)

# make pcraster mask

# read the catchment.asc
options(digits = 10)
subcatch_header <- readLines(paste0(sub_catch_dir, "catchment.asc"), n = 5)
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

#remove ldd map because cannot be resampled.
base_maps <- gsub("^ldd\\.map$", "", base_maps)
# remove catchment because it is already correct size
base_maps <- gsub("^catchment\\.map$", "", base_maps)
base_maps <- base_maps[base_maps != ""]  # Remove empty lines

# resample the base maps to the new mask.map
# make parrallel

# resample with parallel processes to speed up
  n_cores <- detectCores() # number of cores
  registerDoParallel(cores = n_cores - 2) # register the cluster

  for (i in seq_along(base_maps)) {
    file.copy(paste0(sub_catch_dir, "mask.map"), paste0(sub_catch_dir, i, ".map"))
  }
  
foreach (i = seq_along(base_maps)) %dopar% {
#for (i in seq_along(base_maps)) {
  tmp_mask <- paste0(i, ".map")
  
  resample(
    clone = tmp_mask,
    map_in = paste0("base_", base_maps[i]),
    map_out = base_maps[i],
    dir = sub_catch_dir
  )
  file.remove(paste0(sub_catch_dir, tmp_mask))
}

# run pcraster script to create base maps for subcatch
if (calc_ldd == TRUE) {
pcr_script(
  script = "ldd_subcatch.mod",
  script_dir = "sources/pcr_scripts",
  work_dir = sub_catch_dir
)
}
pcr_script(
  script = "base_maps_subcatch.mod",
  script_dir = "sources/pcr_scripts",
  work_dir = sub_catch_dir
)


# remove all intermediate maps and files
file.remove(
  list.files(
    path = sub_catch_dir,
    pattern = "base_|sub|catchment\\.asc|\\.prj",
    full.names = TRUE
  )
)

}
