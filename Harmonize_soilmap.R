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
  rename(staring_code = 'Staring Series')

BtoStaring <- read_csv("data/BOFEKtoStaring.csv", col_types = paste0(c("ii"),strrep("c", 36))) %>%
  pivot_longer(cols = B01:O18, names_to = "staring_code", values_to = "val") %>%
  filter(dominant == 1) %>%
  filter(!is.na(val))

# only for the topsoil
BtoStaring_top <- BtoStaring %>%
  filter(str_detect(staring_code, "^B")) %>%
  select(cluster1, staring_code) %>%
  left_join(staring, by = "staring_code") %>%
  rename(CLUS_2020 = cluster1)

#add staring to dutch clusters
dat <- dat %>%
  left_join(BtoStaring_top, by = "CLUS_2020")

# translate german and french descriptions to dutch + english list
soil_descr <- tibble(original = c(unique(dat$Textuurklasse), unique(dat$DESCRIPTION),
                                  unique(dat$ART_TEXT), unique(dat$Omschrijving)))

# write to csv and translate
write_csv(soil_descr, "soil_descriptions_languages.csv")
