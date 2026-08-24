# code to run an analysis for different scenarios 

library(flextable)
library(officer)
library(scales)

# 1. Spatial results ----------------------------------------------------------

# fill the directory where the runs that need to be analyzed are located
base_dir <- "results/nbs_simulations_20260629"
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
a4 <- bind_rows(list1[[4]])
a5 <- bind_rows(list1[[5]])
a6 <- bind_rows(list1[[6]])

res_pcr <- bind_rows(a1, a2, a3, a4, a5, a6) %>%
  mutate(cond = str_remove(cond, "res_"))


# save and load so whole analysis can be skipped
saveRDS(res_pcr, "documenten_en_literatuur/results/r_tables/res_pcr.rds")


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
all_hy <- bind_rows(hydr_list) %>%
  rename("scen" = "R", "cond" = "P") %>%
  mutate(Pmm = Pavg / 360,
         cond = str_remove(cond, "res_"))

# save and load so whole analysis can be skippen
saveRDS(all_hy, "documenten_en_literatuur/results/r_tables/all_hydrographs.rds")



# 3. Summarise results ---------------------------------------------------------

# there is an error in the calculation of swale landuse surface area (2026-08-19)
#' the workflow below is corrected, and the res_pcr.rds file also. The code above this
#' lines will rewrite res_pcr and undo the correction!!!


# load results section 1 and 2
res_pcr <- readRDS("documenten_en_literatuur/results/r_tables/res_pcr.rds")
all_hy <- readRDS("documenten_en_literatuur/results/r_tables/all_hydrographs.rds")




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
  "T10_wet", "T10_dry",
  "T25_wet", "T25_dry",
  "T100_wet", "T100_dry",
  "T500_wet", "T500_dry"
)


# colors
pal6 <- c(
  "#0072B2", # blue
  "#E69F00", # orange
  "#009E73", # bluish green
  "#D55E00", # vermillion
  "#CC79A7", # reddish purple
  "#56B4E9"  # sky blue
)

pal6_catch <- c(
  "Bildchen" = "#0072B2", # blue
  "Bocholtz" =  "#E69F00", # orange
  "Grunstrasserbach" = "#009E73", # bluish green
  "LangeGracht" = "#D55E00", # vermillion
  "Mechelderbeek" = "#CC79A7", # reddish purple
  "Pesaken" = "#56B4E9"  # sky blue
)

# fix colors for each catchment
fixed_color_scale <- scale_color_manual(
  name = NULL,
  values = pal6_catch,                    # your named palette
  drop = FALSE                      # keep all levels even if not in data
)

## 3.1 Organise hydrographs ----------------------------------------------------

# summary all NBS 
all <- all_hy %>%
  group_by(scen, cond) %>%
  mutate( t_min = Time * 24 * 60,                 # minutes since start
          t_bin = floor(t_min / 2) * 2) %>%     
  group_by(scen, cond, t_bin) %>%
  summarise(Q = mean(Q),
            Pavg = mean(Pavg),
            ) %>%
  group_by(scen, cond) %>%
  summarise(Qmax = max(Q),
            Q = sum(Q) * 120,
            Ptot = sum(Pavg)/30) # 2 minutes to hour

base_hy <- all %>%
  ungroup() %>%
  filter(str_detect(scen, "10m$")) %>%
  rename("catch" = "scen")


scen_hy <- all %>%
  ungroup() %>%
  filter_out(str_detect(scen, "10m$")) %>%
  mutate(catch = str_extract(scen, "^.*10m"))

### 3.1.1. Calculate peak time change ------------------------------------------
all_10 <- all_hy %>%
  group_by(scen, cond) %>%
  mutate( t_min = Time * 24 * 60,                 # minutes since start
          t_bin = floor(t_min / 10) * 10) %>%     
  group_by(scen, cond, t_bin) %>%
  summarise(Q = mean(Q),
            Pavg = mean(Pavg),
  )

# max Q and peak time
peak_10 <- all_10 %>%
  ungroup() %>%
  group_by(scen, cond) %>%
  slice_max(Q)

# filter base
peak_10_base <- peak_10 %>%
  ungroup() %>%
  filter(str_detect(scen, "10m$")) %>%
  rename("catch" = "scen") %>%
  select(catch, cond, t_bin) %>%
  rename("t_base" = "t_bin")

peak_10_scen <- peak_10 %>%
  ungroup() %>%
  filter_out(str_detect(scen, "10m$")) %>%
  mutate(catch = str_extract(scen, "^.*10m")) %>%
  left_join(peak_10_base, by = c("catch", "cond")) %>%
  mutate(t_diff = t_bin - t_base)



## 3.2 Organise spatial data ---------------------------------------------------

landuse_info <- read_csv("sources/setup/tables/lu_NBS_tbl.csv", 
                         show_col_types = FALSE) %>%
  select(lu_nr, description) %>%
  rename("lu" = "lu_nr")

# catchment areas
area_catch <- res_pcr %>%
  group_by(catch, scen, cond) %>%
  summarise(catch_area = sum(area)) %>%
  ungroup() %>%
  group_by(catch) %>%
  summarise(catch_area = max(catch_area))

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

## 3.3 Tables ------------------------------------------------------------------

### Table xx: Results, Discharge baseline runs ----------------------------------
qp_cond <- base_all %>%
  ungroup() %>%
  group_by(cond) %>%
  summarise(Ptot = mean(Ptot),
            Qmm_sd = sd(Qmm),
            Qmm = mean(Qmm),
            QP_sd = sd(QP),
            QP = mean(QP)) %>%
  mutate(cond = factor(cond, levels = cond_order)) %>%
  arrange(cond)
  
base_qp <- qp_cond %>%
  mutate(P = sprintf("%.1f", Ptot),
         Qmm = sprintf("%.1f ± %.1f", Qmm, Qmm_sd),
         QP = sprintf("%.0f%% ± %.0f%%", QP * 100, QP_sd * 100)) %>%
  select(cond, P, Qmm, QP) %>%
  rename_with(~ c("cond", "neerslag (mm)", "afstroming (mm)", "Q/P")) %>%
  pivot_longer(cols = -cond, names_to = "par", values_to = "value") %>%
  pivot_wider(names_from = cond)

ft_base_qp <- flextable(base_qp) %>%
  set_header_labels(par = "") %>%
  style_ft()

ft_base_qp

### Table xx : Results, NBS specific share per catchment ------------------------
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


subc <- unique(area_nbs$description)

ft_area_list <- vector("list", length = length(subc))

for (i in seq_along(subc)) {
t_area_nbs <- area_nbs %>%
  filter(description == subc[i]) %>%
  select(-lu, -description) %>%
  mutate(rel_area = sprintf("%s%%", rel_area)) %>%
  rename_with(~ c("Deelgebied", "Oppervlakte \n(ha)", "opp. %", "volume \n(m³)"))

if (i != 7 & i != 11) {
  t_area_nbs  <- t_area_nbs %>%
    select(-4)
}


ft_area_list[[i]] <- flextable(t_area_nbs) %>%
  style_ft() %>%
  flextable::width(j = 1, width = 1.2) %>%
  flextable::width(j = 2, width = 1.2)
}

ft_area_list[[7]]

### Table xx : Results, normalised effects NBS area -----------------------------
# table with effects per measure and per area

#' **
#' The results for graften and infiltratiestroken are negative for Bocholts
#' we believe that this is strange model behaviour and so we exclude this from
#' the main table. 
#' comment out the line with the ** to make a smaller table that does include 
#' the data to show the effect in a lower section of the report.

# the multiplication with -1 is used to express the reduction as positive value
effect_nbs <- scen_all_rel %>%
  filter_out(lu %in% c(19, 20) & catch == "Bocholtz_10m") %>% # **
  mutate(Qdiff = Qdiff / -1000) %>% # to m3
  group_by(lu, description, cond) %>%
  summarise(Qmm_red_av = mean((Qmm - Qmm_base) * -1),
            Qmm_red_sd = sd((Qmm - Qmm_base)* -1),
            #Qmm_red_max = max((Qmm - Qmm_base)* -1),
            Qar_red_av = mean(Q_area_diff* -1),
            Qar_red_sd = sd(Q_area_diff* -1),
            #Qar_red_max = max(Q_area_diff* -1),
            Q_red_av = mean(Qdiff),
            #Q_red_min = min(Qdiff),
            Q_red_sd = sd(Qdiff)) %>%
  arrange(lu) %>%
  mutate(Qmm_red = sprintf("%.1f ± %.1f", Qmm_red_av, Qmm_red_sd),
         Qar_red = sprintf("%.1f ± %.1f", Qar_red_av, Qar_red_sd),
         Qar_red = if_else(is.na(Qar_red_sd), sprintf("%.1f", Qar_red_av), Qar_red),
         Q_red = sprintf("%.1f ± %.1f", Q_red_av, Q_red_sd),
         description = str_replace(description, "_", " "),
         cond = str_remove(cond, "res_"),
         cond = factor(cond, levels = cond_order)) %>%
  select(lu, description, cond, Qar_red, Q_red) %>%
  arrange(cond)

ef_norm <- effect_nbs %>%
  select(-Q_red) %>%
  pivot_wider(names_from = cond, values_from = Qar_red) %>%
  rename("NBS" = "description") %>%
  mutate(NBS = if_else(NBS == "omvorming naaldbos", paste0(NBS, "¹"), NBS),
         NBS = if_else(NBS == "graften" | NBS == "infiltratiestroken", paste0(NBS, "²"), NBS)) %>%
  ungroup() %>%
  select(-lu)

ft_ef_norm <- flextable(ef_norm) %>%
  add_footer_row(
    values = "¹ Voor omvorming naaldbos is alleen Bildchen gebruikt, waardoor geen standaard deviatie kan worden berekent.
    ² De resultaten van Bocholtz zijn voor deze maatregelen niet meegenomen, zie sectie xx voor verdere uitleg.",
    colwidths = ncol(ef_norm)
  ) %>%
  style_ft() %>%
  flextable::align(part = "footer", align = "left")

ft_ef_norm

# we make a table for graften en infiltratiestroken including the data from Bocholtz
# to show the effect including the strange results for these measures.
ef_boch <- ef_norm %>%
  filter(NBS == "graften" | NBS == "infiltratiestroken")

ft_ef_graf <- flextable(ef_boch) %>%
  style_ft()

ft_ef_graf
# printing is done at the bottom of the script

### Table xx : Results, normalised effects NBS vol ------------------------------
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
            rel_sd = sd(rel_vol_ef)) %>%
  arrange(lu) %>%
  ungroup() %>%
  mutate(vol_red = sprintf("%.1f ± %.1f", rel_mean, rel_sd),
         cond = str_remove(cond, "res_"),
         cond = factor(cond, levels = cond_order)) %>%
  select(description, cond, vol_red) %>%
  arrange(cond) %>%
  pivot_wider(names_from = cond, values_from = vol_red) %>%
  rename("NBS" = "description")
  
ft_ef_vol <- flextable(effect_nbs_vol) %>%
  style_ft()
 
ft_ef_vol


## 3.4 Figures -----------------------------------------------------------------

### Figure xx: Results - individual hydrographs ---------------------------------
# - recalculate total outflow volume to mm outflow and Q/P ratio

subc <- unique(base_all$catch)

for (i in seq_along(subc)) {
  for (j in seq_along(cond_order)) {

    c <- subc[i]
    o <- cond_order[j]
    
# plot
q_ev <- all_hy %>%
  filter(scen == c &
           cond == o) %>%
  mutate( t_min = Time * 24 * 60,                 # minutes since start
          t_bin = floor(t_min / 2) * 2) %>%     
  group_by(t_bin) %>%
  summarise(Q = mean(Q),
            Pavg = mean(Pavg)) %>%
  mutate(
    Time = as.POSIXct("2000-01-01 00:00:00", tz = "UTC") + t_bin * 60,
    thour = t_bin / 60
  )

c <- str_remove(c, "_10m")

titel <- paste0("Afvoer en neerslag voor ", c, " met conditie: ", 
                o)

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
  scale_x_datetime(
    date_labels = "%H:%M",
    date_breaks = "4 hours"
  ) +
  labs(x = "Tijd") +
  theme_bw() +
  theme(plot.title = element_text(size = 10), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  

ggsave(paste0("images/results/nbs_base_hydrographs/", c, "_", o, ".png"), width = 3, height = 3)
  }
}
### Figure xx: Results - NBS effects comparison ---------------------------------  

dat <- scen_all_rel %>%
  mutate(catch = str_remove(catch, "_10m"))


ggplot(
  dat,
  aes(
    x = factor(cond, levels = cond_order),
    y = Qmm_base - Qmm,
    color = catch
  )
) +
  geom_point(size = 1.8, alpha = 0.9) +
  facet_wrap(~description, nrow = 4) +
  theme_bw(base_size = 9) +
  labs(
    x = NULL,
    y = "Totale reductie van afstroming (mm)",   # or "Qmm_base - Qmm (mm)"
    color = "Deelgebied"
  ) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),  # vertical labels
    panel.spacing = unit(0.8, "lines"),
    legend.position = c(0.92, 0.06),   # lower-right in plotting area
    legend.justification = c("right", "bottom"),
    legend.background = element_rect(fill = "white", color = "grey80"),
    legend.key.height = unit(0.35, "cm"),
    legend.key.width  = unit(0.55, "cm"),
    strip.text = element_text(size = 8)
  ) +
  fixed_color_scale

ggsave(
  "images/results/nbs_report/nbs_effects_A4.png",
  width = 6.3, height = 9.7, dpi = 300)


### Figure xx: Results - Discharge per catchment -------------------------------

dat <- base_all %>%
  mutate(catch = str_remove(catch, "_10m"))


ggplot(dat) +
  geom_point(aes(
    x = factor(cond, levels = cond_order),
    y = Qmm,
    color = catch
  ), size = 1.8, alpha = 0.9) +
  geom_point(aes(
    x = factor(cond, levels = cond_order),
    y = Ptot, shape = "Neerslag volume"), size = 2) +
  theme_bw(base_size = 9) +
  labs(
    x = NULL,
    y = "Neerslag en afstroming (mm)"
  ) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),  # vertical labels
    panel.spacing = unit(0.8, "lines"),
    legend.position = c(0.31, 0.66),   # lower-right in plotting area
    legend.justification = c("right", "bottom"),
    legend.background = element_rect(fill = "white", color = "grey80"),
    legend.spacing.y = unit(0, "pt"), 
    legend.key.height = unit(0.35, "cm"),
    legend.key.width  = unit(1, "cm"),
    strip.text = element_text(size = 8)
  ) +
  scale_color_manual(values = pal6) +
  scale_shape_manual(values = c("Neerslag volume" = 2)) +
  guides(
    color = guide_legend(title = "Deelgebied", keywidth = unit(20, "pt")),
    shape = guide_legend(title = NULL, keywidth = unit(23, "pt"))
  )


ggsave(
  "images/results/nbs_report/base_runoff_subc.png",
  width = 5, height = 5, dpi = 300
)

### Figure xx: Results - NBS normalised comparison -----------------------------  
# change the lu number to make a figure for a specific NBS.


dat <- scen_all_rel %>%
  mutate(catch = str_remove(catch, "_10m")) %>%
  filter(lu == 17) # filter(lu != 17 & lu != 21)
  
# for 17 = contourgreppels
# and 21 = waterbuffer droogdal
# adjust to volume per installed m3 NBS

# cal 17 = Q_area_diff / 200
# cal 21 = Q_area_diff / 1500

# y title = "Berging per aangelegd volume (m3 per m3 NBS)"
# y title org: "Genormaliseerde reductie (mm per m² NBS)"


ggplot(
  dat,
  aes(
    x = factor(cond, levels = cond_order),
    y = Q_area_diff / 200 * -1,
    color = catch
  )
) +
  geom_point(size = 1.8, alpha = 0.9) +
  facet_wrap(~description, nrow = 4) +
  theme_bw(base_size = 9) +
  labs(
    x = NULL,
    y = "Berging per aangelegd \nvolume (m3 per m3 NBS)",   # or "Qmm_base - Qmm (mm)"
    color = "Deelgebied"
  ) +
  guides(
    color = guide_legend(
      nrow = 2,           # or 3
      byrow = TRUE,
      override.aes = list(size = 2)
    )
  ) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),  # vertical labels
    panel.spacing = unit(0.8, "lines"),
    legend.position = "bottom",   # lower-right in plotting area
    legend.justification = "left",  
    legend.box.just = "left",
    legend.margin = margin(t = 0, r = 0, b = 0, l = -30),
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 6.5),   # smaller text
    legend.key.height = unit(0.25, "cm"),
    legend.key.width  = unit(0.4, "cm"),
    legend.spacing.x = unit(0.1, "cm"),
    strip.text = element_text(size = 8)
  ) +
  fixed_color_scale #+ ylim(c(0,16))


ggsave(
  "images/results/nbs_report/nbs_effects_contourgreppels_corrected.png",
  width = 3, height = 3.5, dpi = 300)

### Figure xx: Results - compare NBS - base hydrographs ------------------------

subc <- unique(base_all$catch) # all base scnearios
scens <- unique(all$scen)
scens <- scens[!scens %in% subc] # all nbs scenarios

for (i in seq_along(subc)) {
  for (j in seq_along(cond_order)) {
    
    base_scens <- scens[str_detect(scens, subc[i])]
    
    c <- subc[i]
    o <- cond_order[j]
    
    # plot
    q_ev <- all_hy %>%
      filter(scen == c &
               cond == o) %>%
      mutate( t_min = Time * 24 * 60,                 # minutes since start
              t_bin = floor(t_min / 2) * 2) %>%     
      group_by(t_bin) %>%
      summarise(Q = mean(Q),
                Pavg = mean(Pavg)) %>%
      mutate(
        Time = as.POSIXct("2000-01-01 00:00:00", tz = "UTC") + t_bin * 60
      )
    
    
    for (k in seq_along(base_scens)) {
      
      s <- base_scens[k]
      
    q_scen <- all_hy %>%
      filter(scen == s &
               cond == o) %>%
      mutate( t_min = Time * 24 * 60,                 # minutes since start
              t_bin = floor(t_min / 2) * 2) %>%     
      group_by(t_bin) %>%
      summarise(Q = mean(Q),
                Pavg = mean(Pavg)) %>%
      mutate(
        Time = as.POSIXct("2000-01-01 00:00:00", tz = "UTC") + t_bin * 60
      )
    
    s <- str_remove(s, "_10m")
    titel <- paste0("Afvoer en neerslag voor ", s, " met conditie: ", 
                    o)
    
    # plot
    # axis constants
    q_max_round <- ceiling(max(c(q_scen$Q, q_ev$Q), na.rm = TRUE))
    p_max       <- max(q_scen$Pavg, na.rm = TRUE)
    k           <- q_max_round / (p_max * 1.2)
    y_top       <- q_max_round * 2
    
    # plot regular and inverted y-axis
    ggplot() +
      geom_linerange(data = q_ev, aes(x = Time, ymin = y_top,
                         ymax = y_top - Pavg * k), color = "grey") +
      geom_line(data = q_ev, aes(x = Time, y = Q, color = "basis"), linewidth = 0.3) +
      geom_line(data = q_scen, aes(x = Time, y = Q, color = "nbs"), linewidth = 0.3) +
      scale_y_continuous(
        name     = "Debiet (L s⁻¹)",
        limits   = c(0, y_top),
        sec.axis = sec_axis(
          ~ (y_top - .) / k,
          name = "Neerslag (mm h⁻¹)"
        ),
        expand = c(0,0)) +
      scale_x_datetime(
        date_labels = "%H:%M",
        date_breaks = "4 hours"
      ) +
      labs(x = "Tijd") + # title = titel,
      theme_bw() +
      theme(plot.title = element_text(size = 10), 
            panel.grid.minor = element_blank(),
        legend.position = c(0.95, 0.75),   # lower-right in plotting area
        legend.justification = c("right", "bottom"),
        legend.background = element_rect(fill = "white", color = "grey80"),
        legend.key.height = unit(0.35, "cm"),
        legend.key.width  = unit(0.55, "cm"),
        strip.text = element_text(size = 8),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
      ) +
      scale_color_manual(name = NULL,
                         values = c("basis" = "black",
                                    "nbs" = "blue"))
    
    
    ggsave(paste0("images/results/nbs_scenarios_hydrographs/", s, "_", o, ".png"), width = 3, height = 3)
    }
  }
}

### Figure xx: Results - Qpeak reduction ---------------------------------------  

base_qmax <- base_all |>
  select(catch, cond, Qmax) |>
  rename("Qmax_base" = "Qmax")


dat <- scen_all_rel |>
  left_join(base_qmax, by = c("catch", "cond")) |>
  mutate(catch = str_remove(catch, "_10m"),
         Qpeak_red = (1 - (Qmax / Qmax_base)) * 100,
         description = if_else(description == "graften", "graften ¹", description))


ggplot(
  dat,
  aes(
    x = factor(cond, levels = cond_order),
    y = Qpeak_red,
    color = catch
  )
) +
  geom_point(size = 1.8, alpha = 0.9) +
  facet_wrap(~description, nrow = 4) +
  theme_bw(base_size = 9)  +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),  # vertical labels
    panel.spacing = unit(0.8, "lines"),
    legend.position = c(0.92, 0.06),   # lower-right in plotting area
    legend.justification = c("right", "bottom"),
    legend.background = element_rect(fill = "white", color = "grey80"),
    legend.key.height = unit(0.35, "cm"),
    legend.key.width  = unit(0.55, "cm"),
    strip.text = element_text(size = 8)
  ) +
  labs(
    x = NULL,
    y = "Relatieve afname piek afstroming (%)"
  ) +
  scale_color_manual(
    name = "Deelgebied",
    values = pal6_catch,                    # your named palette
    drop = FALSE                      # keep all levels even if not in data
  ) +
  ylim(c(-13, 100))

ggsave(
  "images/results/nbs_report/nbs_peak_reduction.png",
  width = 6.3, height = 9.7, dpi = 300)

### Figure xx: Results - Qpeak base per catchment -------------------------------

dat <- base_all %>%
  mutate(catch = str_remove(catch, "_10m"))


ggplot(dat) +
  geom_point(aes(
    x = factor(cond, levels = cond_order),
    y = Qmax / 1000,
    color = catch
  ), size = 1.8, alpha = 0.9) +
  theme_bw(base_size = 9) +
  labs(
    x = NULL,
    y = "Maximale afstroming m³ sec⁻¹"
  ) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),  # vertical labels
    panel.spacing = unit(0.8, "lines"),
    legend.position = c(0.31, 0.72),   # lower-right in plotting area
    legend.justification = c("right", "bottom"),
    legend.background = element_rect(fill = "white", color = "grey80"),
    legend.spacing.y = unit(0, "pt"), 
    legend.key.height = unit(0.35, "cm"),
    legend.key.width  = unit(1, "cm"),
    strip.text = element_text(size = 8)
  ) +
  scale_color_manual(values = pal6) +
  guides(
    color = guide_legend(title = "Deelgebied", keywidth = unit(20, "pt")),
    shape = guide_legend(title = NULL, keywidth = unit(23, "pt"))
  )


ggsave(
  "images/results/nbs_report/base_qmax_subc.png",
  width = 5, height = 5, dpi = 300
)


### Figure xx: Results - QPeak time change -------------------------------------
dat <- peak_10_scen |>
  mutate(catch = str_remove(catch, "_10m"),
         description = str_extract(scen, "(?<=10m_).*"))


ggplot(
  dat,
  aes(
    x = factor(cond, levels = cond_order),
    y = t_diff,
    color = catch
  )
) +
  geom_point(size = 1.8, alpha = 0.9) +
  facet_wrap(~description, nrow = 4) +
  theme_bw(base_size = 9)  +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),  # vertical labels
    panel.spacing = unit(0.8, "lines"),
    legend.position = c(0.92, 0.06),   # lower-right in plotting area
    legend.justification = c("right", "bottom"),
    legend.background = element_rect(fill = "white", color = "grey80"),
    legend.key.height = unit(0.35, "cm"),
    legend.key.width  = unit(0.55, "cm"),
    strip.text = element_text(size = 8)
  ) +
  labs(
    x = NULL,
    y = "Relatieve afname piek afstroming (%)"
  ) +
  scale_color_manual(
    name = "Deelgebied",
    values = pal6_catch,                    # your named palette
    drop = FALSE                      # keep all levels even if not in data
  ) #+
 # ylim(c(-13, 100))

ggsave(
  "images/results/nbs_report/nbs_peak_time_shift.png",
  width = 6.3, height = 9.7, dpi = 300)

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
  body_add_par("Neerslag en afstroming per conditie", style = "heading 2") %>%
  body_add_flextable(ft_base_qp) %>%
  
  body_add_par("Normalised effects NBS per area - with Bocholtz", style = "heading 2") %>%
  body_add_flextable(ft_ef_graf) %>%
  
  body_add_par("Normalised effects NBS per area", style = "heading 2") %>%
  body_add_flextable(ft_ef_norm) %>%
  
  body_add_par("Normalised effects NBS per volume", style = "heading 2") %>%
  body_add_flextable(ft_ef_vol) %>%
  
  body_add_par(paste0("NBS area for ", subc[1]), style = "heading 2") %>%
  body_add_flextable(ft_area_list[[1]]) %>%
  
  body_add_par(paste0("NBS area for ", subc[2]), style = "heading 2") %>%
  body_add_flextable(ft_area_list[[2]]) %>%
  
  body_add_par(paste0("NBS area for ", subc[3]), style = "heading 2") %>%
  body_add_flextable(ft_area_list[[3]]) %>%
  
  body_add_par(paste0("NBS area for ", subc[4]), style = "heading 2") %>%
  body_add_flextable(ft_area_list[[4]]) %>%
  
  body_add_par(paste0("NBS area for ", subc[5]), style = "heading 2") %>%
  body_add_flextable(ft_area_list[[5]]) %>%
  
  body_add_par(paste0("NBS area for ", subc[6]), style = "heading 2") %>%
  body_add_flextable(ft_area_list[[6]]) %>%
  
  body_add_par(paste0("NBS area for ", subc[7]), style = "heading 2") %>%
  body_add_flextable(ft_area_list[[7]]) %>%
  
  body_add_par(paste0("NBS area for ", subc[8]), style = "heading 2") %>%
  body_add_flextable(ft_area_list[[8]]) %>%
  
  body_add_par(paste0("NBS area for ", subc[9]), style = "heading 2") %>%
  body_add_flextable(ft_area_list[[9]]) %>%
  
  body_add_par(paste0("NBS area for ", subc[10]), style = "heading 2") %>%
  body_add_flextable(ft_area_list[[10]]) %>%
  
  body_add_par(paste0("NBS area for ", subc[11]), style = "heading 2") %>%
  body_add_flextable(ft_area_list[[11]]) %>%
  
  body_add_par("Gebied overzicht", style = "heading 2") %>%
  body_add_flextable(ft_lu_area)




# save everything
print(doc, target = "documenten_en_literatuur/results/nbs_report_tables_figures.docx")


# figure
body_add_par("Peak discharge figure", style = "heading 1") %>%
  body_add_gg(value = p_peak, width = 6.5, height = 4.5)