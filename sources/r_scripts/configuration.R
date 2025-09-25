# configuration
if (!require("yaml")) install.packages("yaml")
library(yaml)
config <- yaml.load_file("config.yaml")

ins <- config$install_packages
if (ins == "Y") {
# install packages if needed
if (!require("pacman")) install.packages("pacman")
pacman::p_load(yaml, hydroGOF, gdalUtilities, terra, raster, cowplot, sf, 
               conflicted, tidyverse, sensobol, foreach, doParallel)

if(!require("rosettaPTF")) remotes::install_github("ncss-tech/rosettaPTF")
} else {
  print("Make sure all packages required are installed, see 'sources/r_scripts/configuration.R'")
}
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

# load configuration


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
points <- read_csv("sources/setup/outpoints_description.csv")


# swatre file
swatre_file <- config$infiltration_table