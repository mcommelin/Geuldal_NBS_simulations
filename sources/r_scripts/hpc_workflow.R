# read the arguments from the command line
# run with:
# Rscript --vanilla ./hpc_workflow.R args1
# example: Rscript --vanilla ./sources/r_scripts/hpc_workflow.R example.ini

args = commandArgs(TRUE)

# set boolean for choices in functions which are shared with manual wrokflow
do_hpc = TRUE

# load or install yaml library
if (!require("yaml")) install.packages("yaml")
library(yaml)

# load all settings
ini_file <- args[1]
ini <- read_yaml(ini_file)

# Initialization ---------------------------------------------------------------

# load and set configured settings from config.yaml
source("sources/r_scripts/configuration.R")


# Data preparation -------------------------------------------------------------
if (ini$do_data_preparation == TRUE) {
source("sources/r_scripts/source_to_base_maps.R")
# make folders and copy to right location
copy_spatial_data()

res_dp <- ini$res_data_prep

# maps dem etc
catch_maps_res()

# the function below makes PCRaster maps for all resolutions from the data
# in ./spatial_data/
spatial_data_to_pcr(res = res_dp) # you can also select only 1 resolution -> faster
# some processing and make ldd
ldd_subcatch(force_ldd = FALSE, res = res_dp)

}

# load config chices for lisem simulations
#load the csv file to identify all sub catch numbers
hpc_ids <- read_csv("sources/setup/hpc/subcatch_id_link.csv")

# produce all subcatchments base maps
if (ini$subset == -1) {
  subnums <- hpc_ids$LISEM_ID
} else {
  # or use a subset (now Belgian part of the Gulp)
  subnums <- ini$subset
}

#load choices from config file
reso <- ini$resolution
runtype <- ini$run_type

# Make base databases ----------------------------------------------------------
if (ini$do_base_db == TRUE) {

# cut all the subcatchments from the Geul
source("sources/r_scripts/create_subcatch_db.R")
for (i in seq_along(subnums)) {
  base_maps_subcatchment(cell_size = reso, sub_catch_number = subnums[i],
                         run_type = runtype, do_hpc = TRUE)
}

}
# make lisem runs ------------------------------------------------
if (ini$do_lisem_run == TRUE) {

# make the swatre base file to produce inputs for the model runs
# when a NBS is added, or settings are changed, repeat this step!
# update landuse table, this works for all NBS solutions.
source("sources/r_scripts/prepare_landuse_table.R")
landuse_table_nbs()

# make a new swatre file, this works for all NBS solutions.
source("sources/r_scripts/swatre_input.R")
swatre_file <- "swatre_NBS.csv"
soil_landuse_to_swatre(file = "sources/setup/swatre/UBC_texture.csv",
                       swatre_out = paste0("sources/setup/calibration/", swatre_file),
                       do_NBS = TRUE
)

# make the actual run databases for the hpc
# choices are:
# include NBS: set a number, 0 = no nbs
# whole Geul or subset?
source("sources/r_scripts/create_hpc_run.R")
create_hpc_run(subset = subnums,
               swatre_file = swatre_file,
               NBS_num = ini$NBS_num,    
               resolution = reso,
               dir_name = ini$dir_name,
               run_type = runtype,
               do_runfile = TRUE,
               cpu_cores = ini$cpu_cores)

}
