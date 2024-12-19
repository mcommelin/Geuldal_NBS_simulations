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
# DONT OVERWRITE!!! # write_csv(soil_descr, "soil_descriptions_languages.csv")
# load after manual classification
soil_descr <- read_csv("soil_descriptions_languages.csv")

# Identifier per country:
# Dutch: OBJECTID_1 BODEM1 CLUS_2020 (link naar staringreeks)
# Flanders: gid Bodemtype Textuurklasse
# Wallony:  OBJECTID CODE DESCRIPTION
# Germany: name TYP_TEXT  (ART_TEXT)

#reorganize map to link a harmonized soil texture class to each unit.
id <- c("OBJECTID_1", "gid", "OBJECTID", "Name")
country <- c("NL", "BEF", "BEW", "DE")
original <- c("Omschrijving", "Textuurklasse", "DESCRIPTION", "ART_TEXT")

#remove the OBJECTID for the NL dataset, we use OBJECTID_1 instead
dat <- dat %>%
  mutate(OBJECTID = if_else(is.na(OBJECTID_1), OBJECTID, NA))

new_soil <- vector("list", length = length(id))

for (i in seq_along(id)) {
  new_soil[[i]] <- dat %>%
    select(id[i], original[i]) %>%
    mutate(country = country[i]) %>%
    rename(id = id[i], original = original[i]) %>%
    filter(!is.na(id)) %>%
    mutate(id = as.character(id))
}
soil_map <- bind_rows(new_soil) %>%
  left_join(soil_descr, by = "original")




#write as new layer
st_write(soil_map, "data/soil_layers.gpkg", layer = "soils_uniform", append = FALSE)
