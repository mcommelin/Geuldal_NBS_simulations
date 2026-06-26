# code to run an analysis for different scenarios 

library(flextable)
library(officer)
library(scales)

# 1. Spatial results ----------------------------------------------------------

# fill the directory where the runs that need to be analyzed are located
base_dir <- "results/nbs_simulations_new_rain_20260527"
#base_dir <- "LISEM_runs"
# find different scenarios - assume workflow based folder structure!
scen_dirs <- dir(base_dir, full.names = TRUE)

# find baseline catchment runs, and scenario runs
baseline_dirs <- grep("_10m$", scen_dirs, value = TRUE)
scen_dirs <- grep("_10m$", scen_dirs, value = TRUE, invert = TRUE)

scen_names <- basename(scen_dirs)
baseline_names <- basename(baseline_dirs)

# find the different rain and moisture conditions for each scenario
conditions <- grep("^res", dir(scen_dirs[1]), value = TRUE)


# loop over all scenario folders:
# 1. copy maps from the baseline results and input maps
# 2. execute PCR script to analyse spatial effects
# 3. write results to PCR table
# 4. load table in list in R

# list the baseline maps we want to copy
base_res_maps <- c("whmax.map",
               "infiltration.map",
               "interception.map"
              )
base_input_maps <- c("landuse.map",
                     "landuse_base.map",
                     "mask.map")

# make list for all scenarios
list1 <- vector("list", length = length(baseline_dirs))

# loop over catchments
for (i in seq_along(baseline_dirs)) {
  # select the scenarios that belong to the baseline
  scen_bool <- startsWith(scen_names, baseline_names[i])
  scenario_dirs <- scen_dirs[scen_bool]
  
  if (length(scenario_dirs) == 0) {
    message("No scenario folders found for ", baseline_name)
    next
  }
  
  # make a list for the result tables
  list2 <- vector("list", length = length(scenario_dirs))
  
  # loop over NBS scenarios
  for (j in seq_along(scenario_dirs)) {
    
    list3 <- vector("list", length = length(conditions))
    
    # loop over conditions within NBS scenario
    for (k in seq_along(conditions)) {
      dest <- file.path(scenario_dirs[j], conditions[k], "res/")
      
      # copy baseline result files
      # add "base" suffix to the base maps names in the subcatch dir
      for (l in seq_along(base_res_maps)) {
        file.copy(
          from = file.path(baseline_dirs[i], conditions[k], "res", base_res_maps[l]),
          to = paste0(dest, "base_", base_res_maps[l]),
          overwrite = TRUE
        )
      }
      
      # copy input maps
      for (m in seq_along(base_input_maps)) {
       file.copy(
          from = paste0(scenario_dirs[j], "/maps/", base_input_maps[m]),
          to = paste0(dest, base_input_maps[m]),
          overwrite = TRUE
        )
      }

      # all maps are now in the results folder, do PCR script
      pcr_script(script = "analyse_nbs_results.mod",
                 script_dir = "sources/pcr_scripts",
                 work_dir = dest)
      
      # write PCR results to table
      pcrtable(work_dir = dest,
               maps = "lu_nom.map infil_md.map icep_md.map",
               outfile = "res.txt")

      #read table in R
      tab <- read.table(paste0(dest, "/res.txt"), header = F)
      
      # add information to table
      nms <- c("lu", "infil_md", "icep_md", "area")
      sc <- basename(scenario_dirs[j])
      list3[[k]] <- as_tibble(tab) %>%
        rename_with( ~ nms) %>%
        mutate(catch = baseline_names[i],
               scen = sc,
               cond = conditions[k])

    } # end conditions loop
    list2[[j]] <- list3
  } # end catchment loop
  
  # store list in other list
  list1[[i]] <- list2
} # end loop reading all scenario results

# combine the whole list to a table
a1 <- bind_rows(list1[[1]])
a2 <- bind_rows(list1[[2]])
a3 <- bind_rows(list1[[3]])

res_pcr <- bind_rows(a1, a2, a3)

# save and load so whole analysis can be skippen
saveRDS(res_pcr, "documenten_en_literatuur/results/r_tables/res_pcr.rds")
res_pcr <- readRDS("documenten_en_literatuur/results/r_tables/res_pcr.rds")

# 2. Hydrographs --------------------------------------------------------------

# find all hydrograph files in the result folders
hydr_files <- dir(path = base_dir, pattern = "hydrographs-",
                  recursive = TRUE, full.names = T)
# list to store everyting
hydr_list <- vector("list", length = length(hydr_files))

# loop over all folders and load results
for (i in seq_along(hydr_files)) {
  hy_names <- readLines(hydr_files[i])[2] %>%
    str_split(",", simplify = TRUE) %>%
    str_remove_all(" |#")
  runtype <- sub("^([^/]+/){2}([^/]+).*$", "\\2", hydr_files[i])
  rain = str_extract(hydr_files[i], "(res_)[^/]+")
  hydr_list[[i]] <- read_csv(hydr_files[i], skip = 2) %>%
    rename_with(~hy_names) %>%
    arrange(Time) %>%
    mutate(Q = Qall + Qbound,
           R = runtype,
           P = rain,
           run = i) %>%
    select(Time, Pavg, R, P, Q, run) #, Qchan1)
  
}
all_hy <- bind_rows(hydr_list) 

# save and load so whole analysis can be skippen
saveRDS(all_hy, "documenten_en_literatuur/results/r_tables/all_hydrographs.rds")
all_hy <- readRDS("documenten_en_literatuur/results/r_tables/all_hydrographs.rds")


# summary all NBS 
all <- all_hy %>%
  rename("scen" = "R", "cond" = "P") %>%
  mutate(Pmm = Pavg / 360) %>%
  group_by(scen, cond) %>%
  summarise(Qmax = max(Q),
            Q = sum(Q) * 10,
            Ptot = sum(Pmm)) #

base_hy <- all %>%
  ungroup() %>%
  filter(str_detect(scen, "10m$")) %>%
  rename("catch" = "scen")


scen_hy <- all %>%
  ungroup() %>%
  filter_out(str_detect(scen, "10m$")) %>%
  mutate(catch = str_extract(scen, "^.*10m"))


a2 <- all %>%
  ungroup() %>%
  group_by(R) %>%
  summarise(Qmax = mean(Qmax),
            Q = mean(Q))


# load hydrograph results as well.
# - recalculate total outflow volume to mm outflow and Q/P ratio


# plot
q_ev <- all_hy %>%
  filter(R == "Pesaken_10m" &
           P == "res_T25_dry")

titel <- paste0("Afvoer en neerslag voor ", q_ev$R[1], " met conditie: ", 
                str_remove(q_ev$P[1], "res_"))

# plot
# axis constants
q_max_round <- ceiling(max(c(q_ev$Q), na.rm = TRUE))
p_max       <- max(q_ev$Pavg, na.rm = TRUE)
k           <- q_max_round / (p_max * 1.2)
y_top       <- q_max_round * 2

# plot regular and inverted y-axis
ggplot(q_ev) +
  geom_linerange(aes(x = Time, ymin = y_top,
                               ymax = y_top - Pavg * k), color = "grey") +
  geom_line(aes(x = Time, y = Q), linewidth = 0.3) +
  scale_y_continuous(
    name     = "Debiet (L s⁻¹)",
    limits   = c(0, y_top),
    sec.axis = sec_axis(
      ~ (y_top - .) / k,
      name = "Neerslag (mm h⁻¹)"
    ),
    expand = c(0,0)) +
  labs(title = titel) +
  theme_bw()

# 3. Summarise results ---------------------------------------------------------

## helper settings -------------------------------------------------------------

# flextable layout
style_ft <- function(ft) {
  ft %>%
    flextable::align(align = "center", part = "all") %>%
    bold(part = "header") %>%
    fontsize(size = 8, part = "all") %>%          # smaller font
    padding(padding = 1, part = "all") %>%        # tighter cells
    height_all(height = 0.18) %>%                 # compact rows
    fit_to_width(max_width = 7)
}



# desired order of conditions
cond_order <- c(
  "T10_dry", "T10_wet",
  "T25_dry", "T25_wet",
  "T100_dry", "T100_wet",
  "T500_dry", "T500_wet"
)




# produce different results

# 1: area of each catchment
# 2: table with area of each lu in each catchment baseline
# 3: table with area of each nbs in each catchment
# 4: 

landuse_info <- read_csv("sources/setup/tables/lu_NBS_tbl.csv", show_col_types = FALSE) %>%
  select(lu_nr, description) %>%
  rename("lu" = "lu_nr")

# catchment areas
area_catch <- res_pcr %>%
  group_by(catch, scen, cond) %>%
  summarise(catch_area = sum(area)) %>%
  ungroup() %>%
  distinct(catch, catch_area)
 
# nbs areas
nbs_areas <- res_pcr %>%
  select(lu, area, catch) %>%
  distinct() %>%
  filter(lu > 10) %>%
  left_join(area_catch, by = "catch") %>%
  mutate(rel_area = area / catch_area) %>%
  left_join(landuse_info, by = "lu")

# combine spatial and outlet results for more info
lu_nr_nbs <- res_pcr %>%
  filter(lu > 10) %>%
  distinct(lu, scen, catch)

nbs_areas <- nbs_areas %>%
  left_join(lu_nr_nbs, by = c("lu", "catch")) %>%
  select(-catch)

scen_all <- scen_hy %>%
  left_join(nbs_areas, by = "scen") %>%
  mutate(Qmm = round(Q / catch_area, digits = 2),
         QP = Qmm / Ptot)

base_all <- base_hy %>%
  left_join(area_catch, by = "catch")%>%
  mutate(Qmm = round(Q / catch_area, digits = 2),
         QP = Qmm / Ptot)

base_left <- base_all %>%
  select(catch, cond, Qmm, Q) %>%
  rename("Qmm_base" = "Qmm", "Q_base" = "Q")

scen_all_rel <- scen_all %>%
  left_join(base_left, by = c("catch", "cond")) %>%
  mutate(Qdiff = Q - Q_base,
         Q_area_diff = Qdiff / area)
  
# make some figures or tables

ggplot() + 
  geom_point(data = scen_all_rel, aes(x = Ptot, y = Qmax, color = catch), size = 3, alpha = 0.2) +
  geom_point(data = base_all, aes(x = Ptot, y = Qmax, color = catch)) +
  theme_classic() +
 # geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  #ylim(c(0,100)) + xlim(c(0,100)) +
  facet_wrap(~ description)


ggplot() +
  geom_point(data = scen_all_rel, aes(x = as.factor(cond), y = Qmax, color = description)) +
  geom_point(data = base_all, aes(x = as.factor(cond), y = Qmax), size = 4, alpha = 0.5, shape = 2) +
  theme_classic() +
  facet_wrap(~ catch, nrow = 3, scales = "free_y")

## Table xx: Results, Discharge baseline runs ----------------------------------



## Table xx : Results, NBS specific share per catchment ------------------------
# table with area and or volume

area_nbs <- scen_all_rel %>%
  select(lu, description, catch, area, rel_area) %>%
  distinct() %>%
  arrange(lu) %>%
  mutate(rel_area = sprintf("%.0f", rel_area * 100 ),
         vol = if_else(lu == 17, area * 2 / 10, NA),
         vol = if_else(lu == 21, area * 150 / 100, vol),
         area = sprintf("%.1f", area / 10000),
         catch = str_remove(catch, "_10m"),
         description = str_replace(description, "_", " "))

# save so table can be remade
saveRDS(area_nbs, "documenten_en_literatuur/results/r_tables/area_nbs.rds")

# make and write flextable
area_nbs <- readRDS("documenten_en_literatuur/results/r_tables/area_nbs.rds")

# TODO change from per catch to per NBS!
subc <- unique(area_nbs$catch)

ft_area_list <- vector("list", length = length(subc))

for (i in seq_along(subc)) {
t_area_nbs <- area_nbs %>%
  filter(catch == subc[i]) %>%
  select(-lu, -catch) %>%
  mutate(rel_area = sprintf("%s%%", rel_area)) %>%
  rename_with(~ c("NBS", "Oppervlakte \n(ha)", "opp. %", "volume \n(m³)"))



ft_area_list[[i]] <- flextable(t_area_nbs) %>%
  style_ft()
}

ft_area_nbs[[1]]

## Table xx : Results, normalised effects NBS area -----------------------------
# table with effects per measure and per area

# TODO adjust to flextable
# the multiplication with -1 is used to express the reduction as positive value
effect_nbs <- scen_all_rel %>%
  mutate(Qdiff = Qdiff / -1000) %>% # to m3
  group_by(lu, description, cond) %>%
  summarise(Qmm_red_av = mean((Qmm - Qmm_base) * -1),
            Qmm_red_min = min((Qmm - Qmm_base)* -1),
            Qmm_red_max = max((Qmm - Qmm_base)* -1),
            Qar_red_av = mean(Q_area_diff* -1),
            Qar_red_min = min(Q_area_diff* -1),
            Qar_red_max = max(Q_area_diff* -1),
            Q_red_av = mean(Qdiff),
            Q_red_min = min(Qdiff),
            Q_red_max = max(Qdiff)) %>%
  arrange(lu) %>%
  mutate(Qmm_red = sprintf("%.1f (%.1f - %.0f)", Qmm_red_av, Qmm_red_min, Qmm_red_max),
         Qar_red = sprintf("%.1f (%.1f - %.1f)", Qar_red_av, Qar_red_min, Qar_red_max),
         Q_red = sprintf("%.0f (%.0f - %.0f)", Q_red_av, Q_red_min, Q_red_max),
         description = str_replace(description, "_", " "),
         cond = str_remove(cond, "res_"),
         cond = factor(cond, levels = cond_order)) %>%
  select(lu, description, cond, Qar_red, Q_red) %>%
  arrange(cond)

ef_norm <- effect_nbs %>%
  select(-Q_red) %>%
  pivot_wider(names_from = cond, values_from = Qar_red) %>%
  rename("NBS" = "description") %>%
  ungroup() %>%
  select(-lu)


# save so table can be remade
saveRDS(ef_norm, "documenten_en_literatuur/results/r_tables/ef_norm.rds")

# make and write flextable
ef_norm <- readRDS("documenten_en_literatuur/results/r_tables/ef_norm.rds")

ft_ef_norm <- flextable(ef_norm) %>%
  style_ft()

ft_ef_norm
# printing is done at the bottom of the script

## Table xx : Results, normalised effects NBS vol ------------------------------
# different tables for volumes?
vol_nbs <- c(17, 21)

effect_nbs_vol <- scen_all_rel %>%
  mutate(Qdiff = Qdiff / -1000) %>% # to m3
  filter(lu %in% vol_nbs) %>%
  mutate(catch = str_remove(catch, "_10m"),
         description = str_replace(description, "_", " ")) %>%
  select(catch, description, cond, Qdiff) %>%
  left_join(area_nbs, by = c("catch", "description")) %>%
  mutate(rel_vol_ef = Qdiff / vol) %>%
  group_by(lu, description, cond) %>%
  summarise(rel_mean = mean(rel_vol_ef),
            rel_min = min(rel_vol_ef),
            rel_max = max(rel_vol_ef)) %>%
  arrange(lu) %>%
  ungroup() %>%
  mutate(vol_red = sprintf("%.1f (%.1f - %.1f)", rel_mean, rel_min, rel_max),
         cond = str_remove(cond, "res_"),
         cond = factor(cond, levels = cond_order)) %>%
  select(description, cond, vol_red) %>%
  arrange(cond) %>%
  pivot_wider(names_from = cond, values_from = vol_red) %>%
  rename("NBS" = "description")
  
# save so table can be remade
saveRDS(effect_nbs_vol, "documenten_en_literatuur/results/r_tables/effect_nbs_vol.rds")

# make and write flextable
effect_nbs_vol <- readRDS("documenten_en_literatuur/results/r_tables/effect_nbs_vol.rds")

ft_ef_vol <- flextable(effect_nbs_vol) %>%
  style_ft()
 
ft_ef_vol


## Table xx: Results, NBS Qpeak reduction -------------------------------------

# heat table qmax
# desired order of conditions
cond_order <- c(
  "T10_dry", "T10_wet",
  "T25_dry", "T25_wet",
  "T100_dry", "T100_wet",
  "T500_dry", "T500_wet"
)


base_select <- base_all %>%
  select(catch, Qmax, cond)
# add label for baseline
base_tab <- base_select %>%
  mutate(nbs = "baseline")

scen_tab <- scen_all %>%
  select(catch, Qmax, description, cond) %>%
  rename("nbs" = "description")
# combine baseline + nbs scenarios
all_tab <- bind_rows(base_tab, scen_tab) %>%
  left_join(
    base_all %>% rename(Qmax_base = Qmax),
    by = c("catch", "cond")
  ) %>%
  mutate(rel_change = if_else(nbs == "baseline", 0,
            100 * (Qmax - Qmax_base) / Qmax_base),
         cond = str_remove(cond, "res_"),
         catch = str_remove(catch, "_10m"),
         Qmax = Qmax / 1000
  )

# 3. order rows: baseline first within each catchment
all_tab <- all_tab %>%
  mutate(
    nbs = factor(nbs, levels = c("baseline", sort(setdiff(unique(nbs), "baseline")))),
    cond = factor(cond, levels = cond_order)
  ) %>%
  arrange(catch, nbs, cond)

show_tab <- all_tab %>%
  select(catch, nbs, cond, Qmax) %>%
  pivot_wider(names_from = cond, values_from = Qmax) %>%
  arrange(catch, nbs)

color_tab <- all_tab %>%
  select(catch, nbs, cond, rel_change) %>%
  pivot_wider(names_from = cond, values_from = rel_change) %>%
  arrange(catch, nbs)

cond_cols <- cond_cols <- cond_order
max_abs_change <- max(abs(all_tab$rel_change), na.rm = TRUE)

pal <- col_numeric(
  palette = c("#1a9850", "#f7f7f7", "#d73027"),
  domain = c(-max_abs_change, max_abs_change)
)

show_tab_fmt <- show_tab %>%
  mutate(across(all_of(cond_cols), ~ sprintf("%.2f", .x)))

ft <- flextable(show_tab_fmt) %>%
  flextable::align(align = "center", part = "all") %>%
  bold(part = "header") %>%
  merge_v(j = "catch") %>%
  valign(j = "catch", valign = "center") %>%
  fontsize(size = 8, part = "all") %>%          # smaller font
  padding(padding = 2, part = "all") %>%        # tighter cells
  height_all(height = 0.18) %>%                 # compact rows
  autofit()

for (cl in cond_cols) {
  fills <- pal(color_tab[[cl]])
  fills[color_tab$nbs == "baseline"] <- "#D9D9D9"
  ft <- bg(ft, j = cl, bg = fills, part = "body")
}

#ft <- bold(ft, i = ~ nbs == "baseline", part = "body")

# thicker line between catchments
end_rows <- c(
  which(show_tab_fmt$catch[-1] != show_tab_fmt$catch[-nrow(show_tab_fmt)]),
  nrow(show_tab_fmt)
)

thick_border <- fp_border(color = "darkgrey", width = 2)

ft <- hline(ft, i = end_rows, border = thick_border, part = "body")

ft <- 

ft

doc <- read_docx() %>%
  body_add_par("Peak discharge table", style = "heading 1") %>%
  body_add_flextable(ft)

print(doc, target = "peak_discharge_table.docx")

## Table xx: Results, NBS Qtot reduction ---------------------------------------

# heat table qmm total
# desired order of conditions
cond_order <- c(
  "T10_dry", "T10_wet",
  "T25_dry", "T25_wet",
  "T100_dry", "T100_wet",
  "T500_dry", "T500_wet"
)


base_select <- base_all %>%
  select(catch, Qmm, cond)
# add label for baseline
base_tab <- base_select %>%
  mutate(nbs = "baseline")

scen_tab <- scen_all %>%
  select(catch, Qmm, description, cond) %>%
  rename("nbs" = "description")
# combine baseline + nbs scenarios
all_tab <- bind_rows(base_tab, scen_tab) %>%
  left_join(
    base_all %>% rename(Qmm_base = Qmm),
    by = c("catch", "cond")
  ) %>%
  mutate(rel_change = if_else(nbs == "baseline", 0,
            100 * (Qmm - Qmm_base) / Qmm_base),
         cond = str_remove(cond, "res_"),
         catch = str_remove(catch, "_10m")
  )

# 3. order rows: baseline first within each catchment
all_tab <- all_tab %>%
  mutate(
    nbs = factor(nbs, levels = c("baseline", sort(setdiff(unique(nbs), "baseline")))),
    cond = factor(cond, levels = cond_order)
  ) %>%
  arrange(catch, nbs, cond)

show_tab <- all_tab %>%
  select(catch, nbs, cond, Qmm) %>%
  pivot_wider(names_from = cond, values_from = Qmm) %>%
  arrange(catch, nbs)

color_tab <- all_tab %>%
  select(catch, nbs, cond, rel_change) %>%
  pivot_wider(names_from = cond, values_from = rel_change) %>%
  arrange(catch, nbs)

cond_cols <- cond_cols <- cond_order
max_abs_change <- max(abs(all_tab$rel_change), na.rm = TRUE)

pal <- col_numeric(
  palette = c("#1a9850", "#f7f7f7", "#d73027"),
domain = c(-max_abs_change, max_abs_change)
)

show_tab_fmt <- show_tab %>%
  mutate(across(all_of(cond_cols), ~ sprintf("%.2f", .x)))

ft <- flextable(show_tab_fmt) %>%
  flextable::align(align = "center", part = "all") %>%
  bold(part = "header") %>%
  merge_v(j = "catch") %>%
  valign(j = "catch", valign = "center") %>%
  fontsize(size = 8, part = "all") %>%          # smaller font
  padding(padding = 2, part = "all") %>%        # tighter cells
  height_all(height = 0.18) %>%                 # compact rows
  autofit()

for (cl in cond_cols) {
  fills <- pal(color_tab[[cl]])
  fills[color_tab$nbs == "baseline"] <- "#D9D9D9"
  ft <- bg(ft, j = cl, bg = fills, part = "body")
}

#ft <- bold(ft, i = ~ nbs == "baseline", part = "body")

# thicker line between catchments
end_rows <- c(
  which(show_tab_fmt$catch[-1] != show_tab_fmt$catch[-nrow(show_tab_fmt)]),
  nrow(show_tab_fmt)
)

thick_border <- fp_border(color = "darkgrey", width = 2)

ft <- hline(ft, i = end_rows, border = thick_border, part = "body")

ft <- fit_to_width(ft, max_width = 9)

ft

doc <- read_docx() %>%
  body_add_par("Total discharge table", style = "heading 1") %>%
  body_add_flextable(ft)

print(doc, target = "Total_discharge_table.docx")


# 4. Catchment overview -------------------------------------------------------

## Table xx : Introduction, subcatch overview ----------------------------------
# select directory in LISEM_data
geul_dir <- "LISEM_data/Geul_10m/maps/"
sub_catch_name <- c("Bildchen", "Bocholtz", "Pesaken", "Mechelderbeek", "LangeGracht", 
                   "Grunstrasserbach")
sub_catch_dir <- sub_catch_name %>%
  paste0("LISEM_data/subcatchments/", ., "_10m/maps/")
catch_names <- c("Geul", sub_catch_name)
dirs <- c(geul_dir, sub_catch_dir)

area_list <- vector("list", length = length(dirs))

for (i in seq_along(dirs)) {
    # do PCR script
pcr_script(script = "catchment_overview.mod",
           script_dir = "sources/pcr_scripts",
           work_dir = dirs[i])

# write PCR results to table
pcrtable(work_dir = dirs[i],
         maps = "lu_nom.map av_slope.map",
         outfile = "areas.txt")

#read table in R
tab <- read.table(paste0(dirs[i], "/areas.txt"), header = F)

# add information to table
nms <- c("lu", "slope", "area")

area_list[[i]] <- as_tibble(tab) %>%
  rename_with( ~ nms) %>%
  mutate(catch = catch_names[i])

# remove maps and pcrtable - clean_up
files <- c("lu_nom.map", "av_slope.map", "grad.map", "areas.txt")
file.remove(paste0(dirs[i], files))

} # end catchment loop


# load results and format as flextable
table <- bind_rows(area_list)

# reorder
full_area <- table %>%
  group_by(catch) %>%
  summarize(full_area = sum(area) / 1000000,
            slope = min(slope))

lu_areas <- table %>%
  select(-slope) %>%
  filter(lu != 0) %>%  # remove landuse class 0 - it is an artiefact and doesnt add a lot.
  mutate(area = area / 1000000) %>%
  left_join(full_area, by = "catch") %>%
  mutate(fraction = (area / full_area) * 100,
         value = sprintf("%.1f - %.0f%%", area, fraction)) %>%
  select(-slope, -area, - fraction) %>%
  pivot_wider(names_from = lu, values_from = value) %>%
  left_join(full_area, by = c("catch", "full_area")) %>%
  rename_with(~ c("gebied", "oppervlakte", "akker", "loofbos", "prod. gras", "nat. gras", "verhard", 
                  "water", "naaldbos", "helling")) %>%
  mutate(naaldbos = if_else(is.na(naaldbos), "0.0 - 0%", naaldbos),
         oppervlakte = sprintf("%.1f", oppervlakte),
         helling = sprintf("%.0f%%", helling)) %>%
  select(gebied, oppervlakte, helling, everything())
  

# save so table can be remade
saveRDS(lu_areas, "documenten_en_literatuur/results/r_tables/lu_areas.rds")

# make and write flextable
lu_areas <- readRDS("documenten_en_literatuur/results/r_tables/lu_areas.rds")

ft_lu_area <- flextable(lu_areas) %>%
  style_ft()

ft_lu_area




# 5. Print results ------------------------------------------------------------

# make a word document with bookmarks for all tables, this will be filled
# while running the code below.
doc <- read_docx()

doc <- doc %>%
  #Heading
  body_add_par("NBS results report - tables and figures", style = "heading 1") %>%
  body_add_par("") %>%
  
  #table
  
  body_add_par("Normalised effects NBS per area", style = "heading 2") %>%
  body_add_flextable(ft_ef_norm) %>%
  
  body_add_par("Normalised effects NBS per volume", style = "heading 2") %>%
  body_add_flextable(ft_ef_vol) %>%
  
  body_add_par(paste0("NBS area in ", subc[1]), style = "heading 2") %>%
  body_add_flextable(ft_area_list[[1]]) %>%
  
  body_add_par(paste0("NBS area in ", subc[2]), style = "heading 2") %>%
  body_add_flextable(ft_area_list[[2]]) %>%
  
  body_add_par(paste0("NBS area in ", subc[3]), style = "heading 2") %>%
  body_add_flextable(ft_area_list[[3]])%>%
  
  body_add_par("Gebied overzicht", style = "heading 2") %>%
  body_add_flextable(ft_lu_area)




# save everything
print(doc, target = "documenten_en_literatuur/results/nbs_report_tables_figures.docx")


# figure
body_add_par("Peak discharge figure", style = "heading 1") %>%
  body_add_gg(value = p_peak, width = 6.5, height = 4.5)