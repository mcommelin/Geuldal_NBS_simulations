# code to prepare lisem input for 5 and 20 m based on the prcessed input maps
# this code takes the following steps:
# 1. create subcatchment maps from the table with coordinates
# 2. run the pcraster script to make the input database

# Initialization --------------------------------------------------------------
library(tidyverse)

source("sources/pcrasteR.R")
set_pcraster(env = "qgis", miniconda = "~/ProgramFiles/miniconda3")

# 1. create subcatchment maps -------------------------------------------------

# load the outpoints csv file
points <- read_csv("LISEM_data/tables/outpoints_description.csv")

cell_size <- unique(points$cell_size)

# loop over resolutions
for (j in seq_along(cell_size)) {
  subdir <- paste0("LISEM_data/Geul_", cell_size[j], "m/maps/")
  
  # filter the correct resolution
  points_res <- points %>%
    filter(cell_size == cell_size[j]) %>%
    select(x, y, point)
  # write csv table
  write_csv(points_res, file = paste0(subdir, "outpoints.txt"),
            col_names = FALSE)
  # run col2map
  col2map(col_in = "outpoints.txt", map_out = "outpoints.map",
          sub_dir = subdir, options = "-S")
  
  #copy landuse and soil tables to subdir
  lutbl <- read_csv("LISEM_data/tables/lu_tbl.csv") %>%
    select(-description, -notes)
  nms <- as.character(seq(0, ncol(lutbl) - 1))
  names(lutbl) <- nms
  #write space delimited lutbl with colnumbers instead of names
  write.table(lutbl, file = paste0(subdir, "lu.tbl"),
              sep = " ", row.names = FALSE,
              quote = FALSE)
  # same for soiltbl
  soiltbl <- read_csv("LISEM_data/tables/soil_tbl.csv") %>%
    select(-class_en, -notes)
  nms <- as.character(seq(0, ncol(soiltbl) - 1))
  names(soiltbl) <- nms
  #write space delimited soiltbl with colnumbers instead of names
  write.table(soiltbl, file = paste0(subdir, "soil.tbl"),
              sep = " ", row.names = FALSE,
              quote = FALSE)
  # run pcraster db script
  pcr_script(
    script = "prepare_db.mod",
    script_dir = "sources/pcr_scripts",
    work_dir = subdir
  )
  
  #delete intermediate files
  file.remove(paste0(subdir, "outpoints.txt"))
  file.remove(paste0(subdir, "lu.tbl"))
  file.remove(paste0(subdir, "soil.tbl"))
  
}




