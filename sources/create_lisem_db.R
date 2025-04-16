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

# loop over resolutions

# write .txt for resolution

# run c0l2map

# run pcraster db script