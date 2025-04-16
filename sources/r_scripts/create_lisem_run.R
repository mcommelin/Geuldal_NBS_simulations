# code to prepare lisem input for 5 and 20 m based on the prcessed input maps
# this code takes the following steps:
# 1. create subcatchment maps from the table with coordinates
# 2. run the pcraster script to make the input database

# Initialization --------------------------------------------------------------
library(tidyverse)

source("sources/pcrasteR.R")
set_pcraster(env = "qgis", miniconda = "~/ProgramFiles/miniconda3")

# 1. create subcatchment maps -------------------------------------------------

cell_size <- unique(points$cell_size)

# copy basemap to a lisem_runs folder

# create full run db

  # loop over resolutions
  for (j in seq_along(cell_size)) {
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
  #2. Run pcraster db script----------------------------------------------------
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




