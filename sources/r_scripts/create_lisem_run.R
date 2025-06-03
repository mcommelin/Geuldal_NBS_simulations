# code to prepare lisem input for 5 and 20 m based on the prcessed input maps
# this code takes the following steps:
# 1. run the pcraster script to make the input database

# Initialization --------------------------------------------------------------
library(tidyverse)

source("sources/r_scripts/pcrasteR.R")
set_pcraster(env = "qgis", miniconda = "~/ProgramFiles/miniconda3")

#1. Run pcraster db script----------------------------------------------------
points <- read_csv("LISEM_data/tables/outpoints_description.csv")

# settings
resolution <- 5 # fill resolution here
catch_num <- 14 # fill catchment number here (see points table)

# function create_lisem_run
create_lisem_run <- function(
    resolution = NULL,
    catch_num = NULL) {
catch_info <- points %>%
  filter(point == catch_num) %>%
  filter(cell_size == resolution)
## copy basemaps to a lisem_runs folder ---------------------------------------
catch_dir <- paste0(catch_info$subcatch_name, "_", catch_info$cell_size, "m/")

base_dir <- paste0("LISEM_data/", catch_dir)
# if catch_num > 1 add subcatchements after LISEM_data/
if (catch_num > 1) {
  base_dir <- paste0("LISEM_data/subcatchments/", catch_dir)
}

run_dir <- paste0("LISEM_runs/", catch_dir)

# create subdir for the run
if (!dir.exists(run_dir)) {
  dir.create(run_dir, recursive = TRUE)
}

# create the following folders in the run_dir: maps, rain, res, runfiles
dirs <- c("maps", "rain", "res", "runfiles")
for (dir in dirs) {
  dir_path <- paste0(run_dir, dir)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path)
  }
}

base_maps <- c("dem.map", "mask.map", "landuse.map", "soils.map", 
               "catchment.map", "ID.map", "buildings.map",
               "roads_fraction.map",
               "chanmask.map", "chanwidth.map", "chandepth.map",
               "culvertmask.map")
# copy the maps to the run_dir
subdir <- paste0(run_dir, "maps/")
for (map in base_maps) {
  file.copy(paste0(base_dir, "maps/", map), paste0(subdir, map))
}

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
  
  pcr_script(
    script = "prepare_db.mod",
    script_dir = "sources/pcr_scripts",
    work_dir = subdir
  )
  
  #delete intermediate files
  file.remove(paste0(subdir, "lu.tbl"))
  file.remove(paste0(subdir, "soil.tbl"))
}




