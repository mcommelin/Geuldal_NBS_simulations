# configuration
if (!require("yaml")) install.packages("yaml")
library(yaml)
config <- yaml.load_file("config.yaml")

ins <- config$install_packages

# load all packages
if (ins == "Y") {
if (!require("hydroGOF")) install.packages("hydroGOF")
if (!require("gdalUtilities")) install.packages("gdalUtilities")
if (!require("terra")) install.packages("terra")
if (!require("remotes")) install.packages("remotes")
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
# install known working version of rosetta-soil
py_install("rosetta-soil==0.1.2", pip = TRUE)
} else {
  print("Make sure all packages required are installed, see 'sources/r_scripts/configuration.R'")
  Sys.sleep(1)


library(hydroGOF)
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
  # set python etc before loading rosettaPTF
  conda_path <- paste0(config$miniconda_path, "/envs/", config$conda_env)
  use_condaenv(condaenv = conda_path, required = T)
library(rosettaPTF)

}
# load configuration
DEBUGm = if (config$debug_messages == "Y") {TRUE} else {FALSE}

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
points_id <- config$subcatchments 
reso <- config$resolution

# load subcatchment points csv file
points <- read_csv("sources/setup/outpoints_description.csv", show_col_types = FALSE)

# swatre file
swatre_file <- "cal_OM_swatre.csv"

# cpu cores
ncpu <- config$cpu_cores
if (ncpu == -1) {
  ncpu <- floor(num_cores() / 2)
}
 
# CODE PROBABLY NOT NEEDED FOR rosetta
# set python etc before loading rosettaPTF
#conda_path <- paste0(config$miniconda_path, "/envs/", config$conda_env)
#use_condaenv(condaenv = conda_path, required = T)
