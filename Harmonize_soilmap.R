#Update Soil table for the Geuldal from different sources.

## initialization
library(tidyverse)
library(sf)

## clean up attribute table of soil map

# load layer
soils <- st_read("data/soil_layers.gpkg", layer = "soils_region")

names(soils)
