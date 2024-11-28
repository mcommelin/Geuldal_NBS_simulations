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
