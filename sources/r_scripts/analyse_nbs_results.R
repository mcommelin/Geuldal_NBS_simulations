# code to calculate difference in whmax for all results

base_dir <- "results/nbs_simulations_new_rain_20260527/"

conditions <- c(
  "res_T10_dry",
  "res_T10_wet",
  "res_T25_dry",
  "res_T25_wet",
  "res_T100_dry",
  "res_T100_wet",
  "res_T500_dry",
  "res_T500_wet"
)

dirs <- list.dirs(base_dir, full.names = TRUE, recursive = FALSE)
dir_names <- basename(dirs)

baseline_idx <- grepl("_10m$", dir_names)
baseline_dirs <- dirs[baseline_idx]
baseline_names <- dir_names[baseline_idx]

for (i in seq_along(baseline_dirs)) {
  baseline_dir <- baseline_dirs[i]
  baseline_name <- baseline_names[i]
  
  scenario_idx <- startsWith(dir_names, paste0(baseline_name, "_"))
  scenario_dirs <- dirs[scenario_idx]
  
  if (length(scenario_dirs) == 0) {
    message("No scenario folders found for ", baseline_name)
    next
  }
  
  for (scenario_dir in scenario_dirs) {
    for (cond in conditions) {
      src_file <- file.path(baseline_dir, cond, "res/whmax.map")
      dest_dir <- file.path(scenario_dir, cond, "res/")
      dest_file <- file.path(dest_dir, "whmax_base.map")
      
      if (!file.exists(src_file)) {
        message("Skipping missing source: ", src_file)
        next
      }
      
      if (!dir.exists(dest_dir)) {
        message("Skipping missing destination dir: ", dest_dir)
        next
      }
      
      ok <- file.copy(from = src_file, to = dest_file, overwrite = TRUE)
      
      if (!ok) {
        message("Copy failed: ", dest_file)
        next
      }
      
      message("Copied: ", src_file, " -> ", dest_file)
      
      pcrcalc("diff.map=whmax.map-whmax_base.map", dest_dir)
      
    }
  }
}
