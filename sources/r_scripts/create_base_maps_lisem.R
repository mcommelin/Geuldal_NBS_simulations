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
  write_csv(points_res, file = paste0(subdir, "outpoint.txt"),
            col_names = FALSE)
  # run col2map
  col2map(col_in = "outpoint.txt", map_out = "outpoint.map",
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
