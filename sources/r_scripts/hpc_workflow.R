# read the arguments from the command line
# run with:
# Rscript --vanilla ./hpc_workflow.R args1 args2 args3 args4

args = commandArgs(TRUE)

prepare_data <- args[1]
make_db <- args[2]
make_hpc_run <- args[3]
ini_file <- args[4]

# Read ini file ----------------------------------------------------------------

# -- resolution data preparation


# Initialization ---------------------------------------------------------------

# load and set configured settings from config.yaml
source("sources/r_scripts/configuration.R")


# Data preparation -------------------------------------------------------------
if (prepare_data == TRUE) {
source("sources/r_scripts/source_to_base_maps.R")

#TODO function to copy maps etc from spatial data folder!


# maps dem etc
catch_maps_res()

# the function below makes PCRaster maps for all resolutions from the data
# in ./spatial_data/
spatial_data_to_pcr(res = c(5, 10, 20)) # you can also select only 1 resolution -> faster
# some processing and make ldd
ldd_subcatch(force_ldd = FALSE, res = c(5, 10, 20))

}

# Make base databases ----------------------------------------------------------