# configuration
if (!require("yaml")) install.packages("yaml")
library(yaml)
config <- yaml.load_file("config.yaml")

ins <- config$install_packages
#if (ins == "Y") {
if (!require("hydroGOF")) install.packages("hydroGOF")
if (!require("gdalUtilities")) install.packages("gdalUtilities")
if (!require("terra")) install.packages("terra")
#install.packages('terra', repos='https://rspatial.r-universe.dev')
#if (!require("remotes")) install.packages("remotes")
#remotes::install_github("rspatial/terra")
if (!require("raster")) install.packages("raster")
if (!require("cowplot")) install.packages("cowplot")
if (!require("sf")) install.packages("sf")
if (!require("conflicted")) install.packages("conflicted")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("sensobol")) install.packages("sensobol")
if (!require("foreach")) install.packages("foreach")
if (!require("doParallel")) install.packages("doParallel")
if (!require("reticulate")) install.packages("reticulate")
if(!require("rosettaPTF")) remotes::install_github("ncss-tech/rosettaPTF")
#} else {
 # print("Make sure all packages required are installed, see 'sources/r_scripts/configuration.R'")
#  Sys.sleep(2)
#}
# load all packages

library(hydroGOF)
library(rosettaPTF)
library(gdalUtilities)
library(terra)
library(raster)
library(cowplot)
library(sf)
library(conflicted)
library(tidyverse)
library(sensobol)
library(foreach)
library(doParallel)
library(reticulate)

# load configuration
DEBUGm = TRUE #<- if (config$debug_messages == "Y") {TRUE} else {FALSE}

# make global choices for conflicting functions
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

# load pcraster functions
source("sources/r_scripts/pcrasteR.R")
set_pcraster(env = config$conda_env, miniconda = config$miniconda_path)

#set digits to 10 for detail in coordinates
options(digits = 10)

# load helper functions coded for this project
source("sources/r_scripts/aux_functions.R")

#! Always load the following data - adjust if needed for custom settings
points_id <- config$subcatchments #, 18, 4, 12, 90)
reso <- config$resolution

# load subcatchment points csv file
points <- read_csv("sources/setup/outpoints_description.csv", show_col_types = FALSE)


# swatre file
swatre_file <- config$infiltration_table