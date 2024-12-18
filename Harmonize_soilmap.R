#Update Soil table for the Geuldal from different sources.

## initialization
library(tidyverse)
library(sf)

## clean up attribute table of soil map

# load layer
soils <- st_read("data/soil_layers.gpkg", layer = "soils_region")

names(soils)

# Identifier per country:
# Dutch: OBJECTID_1 BODEM1 CLUS_2020 (link naar staringreeks)
# Flanders: gid Bodemtype Textuurklasse
# Wallony:  OBJECTID CODE DESCRIPTION
# Germany: name TYP_TEXT  (ART_TEXT)

dat <- soils %>%
  select(OBJECTID_1, OBJECTID, gid, Name, BODEMCODE, Bodemtype, CODE, TYP_TEXT, 
         CLUS_2020, Textuurklasse, DESCRIPTION, ART_TEXT)

#' soil characteristics
#' depth
#' layers
#' swatre params
#' ksat
#' texture class
#' 
#' d50
#' cohesion
#' 

#load staring reeks series:
staring <- read_delim("data/StaringReeks.txt", delim = ";") %>%
  rename(bouwsteen = 'Staring Series')

BtoStaring <- read_csv("data/BOFEKtoStaring.csv", col_types = paste0(c("ii"),strrep("c", 36))) %>%
  pivot_longer(cols = B01:O18, names_to = "bouwsteen", values_to = "val") %>%
  filter(dominant == 1) %>%
  filter(!is.na(val))

# only link for the topsoil
BtoStaring_top <- BtoStaring %>%
  filter(str_detect(bouwsteen, "^B")) %>%
  select(cluster1, bouwsteen) %>%
  left_join(staring, by = "bouwsteen")
