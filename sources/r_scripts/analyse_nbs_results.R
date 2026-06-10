# code to run an analysis for different scenarios 

# fil the directory where the runs that need to be analyzed are located
base_dir <- "results/nbs_simulations_new_rain_20260527"

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

    } # end conditions
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

# load hydrograph results as well.
# - recalculate total outflow volume to mm outflow and Q/P ratio


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
