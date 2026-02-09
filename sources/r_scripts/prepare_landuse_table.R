# note field OM for forests is too high, now a correction done in the csv file
#TODO move OM adjustment to code?
pars_lu <- read_csv("sources/setup/tables/fieldwork_to_classes.csv", show_col_types = FALSE) %>%
  mutate(nbs_type = if_else(nbs_type == "extensieve begrazing", NA, nbs_type)) %>%
  # remove 1 nbs label to include in natural grassland group
  filter(is.na(nbs_type)) %>%
  group_by(lu_nr) %>%
  summarise(rr = round(mean(rr), digits = 2),
            n_res = round(mean(n_res), digits = 2),
            n_veg = round(mean(n_veg), digits = 2),
            om = round(mean(om_corr), digits = 2))

# load lu table
lu_tbl <- read_csv("sources/setup/tables/lu_tbl.csv", show_col_types = FALSE)
lu_add <- lu_tbl %>%
  filter(rr != -9) %>%
  select(-description, -smax_eq, - notes) 
s_eq <- lu_tbl %>% select(lu_nr, smax_eq)

# the O horizon has the high OM values measured in the fieldcampaign
# a value between 1 and < 30 cm can be chosen for each landuse
# a value of 30 or more will give errors in the current code!
# lu types: 1 = akker, 2 = loofbos, 3 = productie gras, 4 = natuur gras,
# 5 = verhard, 6 = water, 7 = naaldbos
O_depth <- c(10, 20, 10, 10, 7, 1, 20)

# add average summer plant cover to create per.map and all derivatives
# change cover values for other seasons: maps per, lai, manning and smax 
# Alternatively we also have data from the fieldwork for per.
per <- c(0.7,0.9,0.7,0.8,0.05,0,0.9)

lu_pars <- bind_rows(pars_lu, lu_add) %>%
  left_join(s_eq, by = "lu_nr")%>%
  arrange(lu_nr) %>%
  mutate(od = O_depth, 
         cover = per)

#include calibration already here
# create landuse calibration table: used in prepare_db.map AND prepare_ndvi.mod
cal_lu <- read_csv("sources/setup/calibration/calibration_landuse.csv") %>%
  select(-cal_comment) %>%
  rename(lu_nr = landuse)

# mannings N based on philips 1989: n = RR/100 + n_residue + n_vegetation * per
lu_pars <- lu_pars %>%
  left_join(cal_lu, by = "lu_nr") %>%
  mutate(rr = rr * rr_cal * 10, # the field data were not very conclusive, at least multiply by 10 or more!
         n = (rr/100 + n_res + n_veg * per) * n_cal) %>%
  select(-ksat_cal, -n_cal, -rr_cal)

# why not this equation? (n_res + 0.1*rr+0.104*per**1.2) * mann_cal -> comes from the date specific calibration 'prepare_ndvi.mod' 

nms <- as.character(seq(0, ncol(lu_pars) - 1))
names(lu_pars) <- nms

# save the landuse parameters as table for PCRaster
write.table(lu_pars, file = "sources/setup/calibration/lu.tbl",
            sep = " ", row.names = FALSE,
            quote = FALSE)
# 1 = RR, 2 = n_res; 3 = n_veg; 4 = om; 5 = smax; 6 = o depth; 7 = cover; 8 = n
#note: here only cols 1,2, 3, 5 and 7 are used 1=RR; 2=n_res; 3 = n_veg; 5=SMAX, 7=cover
#the other columns are used in SWATRE creation, swatre_input.R