# interactive_run.R
#
# Purpose:
#   Interactive guided entry point for the LISEM Geuldal manual workflow.
#   Run this script in an RStudio session to be guided through all major
#   settings via a series of questions. It reads/writes ./config.yaml,
#   then executes exactly the same functions as LISEM_Geuldal_full_workflow.R
#   based on your answers.
#
# How to run:
#   Open the project in RStudio and execute:
#     source("sources/r_scripts/interactive_run.R")
#   Or open the file and press Ctrl+Shift+Enter to source it.
#
#   The script also works non-interactively (Rscript --vanilla), but in that
#   case all prompts will use the defaults from config.yaml without asking.
#
# Depends on:
#   yaml (already used elsewhere in the project), base R only.

# ---------------------------------------------------------------------------- #
# 0. Helpers                                                                    #
# ---------------------------------------------------------------------------- #

# load yaml – install only if missing (same pattern as configuration.R)
if (!require("yaml")) install.packages("yaml", repos = "https://cloud.r-project.org")
library(yaml)

# simple wrapper: ask a question and return the trimmed answer.
# In non-interactive sessions (Rscript --vanilla) returns the default silently.
.ask <- function(prompt, default = "") {
  if (interactive()) {
    ans <- readline(prompt)
    if (nchar(trimws(ans)) == 0) default else trimws(ans)
  } else {
    default
  }
}

# wrapper around menu() that falls back to returning the default index
# in non-interactive sessions
.menu <- function(choices, title = "") {
  if (interactive()) {
    menu(choices, title = title)
  } else {
    1L  # always pick first option when non-interactive
  }
}

# ---------------------------------------------------------------------------- #
# 1. Config file handling                                                       #
# ---------------------------------------------------------------------------- #

config_path   <- "config.yaml"
template_path <- "config_template.yaml"

if (!file.exists(config_path)) {
  if (!file.exists(template_path)) {
    stop("Neither config.yaml nor config_template.yaml found. ",
         "Make sure you are running from the project root directory.")
  }
  message("config.yaml not found – copying from config_template.yaml ...")
  file.copy(template_path, config_path)
  message("config.yaml created. Please review it later if needed.\n")
}

config <- yaml.load_file(config_path)

# set safe defaults for new fields that may not exist in an older config.yaml
if (is.null(config$run_type))  config$run_type  <- "cal"
if (is.null(config$NBS_num))   config$NBS_num   <- 0
if (is.null(config$dir_name))  config$dir_name  <- ""
if (is.null(config$inithcal))  config$inithcal  <- 1.0
if (is.null(config$calc_ldd))  config$calc_ldd  <- "N"

# ---------------------------------------------------------------------------- #
# 2. Interactive question flow                                                  #
# ---------------------------------------------------------------------------- #

cat("\n")
cat("# ------------------------------------------------------------------- #\n")
cat("#          LISEM Geuldal – Interactive workflow setup                  #\n")
cat("# ------------------------------------------------------------------- #\n")
cat("Press Enter to accept the default value shown in [brackets].\n\n")

# --- Q1: Run mode -----------------------------------------------------------
cat("-- Step 1: Run mode --\n")
run_mode <- .menu(
  choices = c(
    "Data preparation only  (base catchment maps, no LISEM run)",
    "Calibration run        (run_type = 'cal')",
    "NBS run                (run_type = 'base', with NBS measure(s))",
    "NBS scenario run       (run_type = 'base', combined scenario map)"
  ),
  title = "Which workflow steps do you want to execute?"
)

if (run_mode == 0) stop("No selection made – aborting.")

run_mode_label <- c("data_prep", "cal", "nbs", "scenario")[run_mode]

# --- Q2: Subcatchment number(s) ---------------------------------------------
cat("\n-- Step 2: Subcatchment(s) --\n")

# load the lookup table for validation and display
points_tbl <- tryCatch(
  read.csv("sources/setup/outpoints_description.csv", stringsAsFactors = FALSE),
  error = function(e) NULL
)

.show_subcatch_list <- function() {
  if (!is.null(points_tbl)) {
    unique_pts <- unique(points_tbl[, c("point", "name", "description")])
    cat("  Available subcatchments:\n")
    for (i in seq_len(nrow(unique_pts))) {
      cat(sprintf("    %3d  %-20s  %s\n",
                  unique_pts$point[i],
                  unique_pts$name[i],
                  unique_pts$description[i]))
    }
  } else {
    cat("  (outpoints_description.csv not found – cannot list catchments)\n")
  }
}

valid_pts <- if (!is.null(points_tbl)) unique(points_tbl$point) else NULL
default_pts <- paste(config$subcatchments, collapse = ", ")

repeat {
  raw <- .ask(
    sprintf("  Enter subcatchment number(s), comma-separated [%s]: ", default_pts),
    default = default_pts
  )
  if (tolower(trimws(raw)) == "list") {
    .show_subcatch_list()
    next
  }
  pts_vec <- suppressWarnings(as.integer(trimws(strsplit(raw, ",")[[1]])))
  if (any(is.na(pts_vec))) {
    cat("  Invalid input – please enter integer(s), or type 'list' for options.\n")
    next
  }
  if (!is.null(valid_pts) && !all(pts_vec %in% valid_pts)) {
    bad <- pts_vec[!pts_vec %in% valid_pts]
    cat(sprintf("  Unknown subcatchment number(s): %s\n",
                paste(bad, collapse = ", ")))
    cat("  Type 'list' to see valid options.\n")
    next
  }
  break
}
points_id <- pts_vec

# --- Q3: Resolution ----------------------------------------------------------
cat("\n-- Step 3: Resolution --\n")
default_res <- paste(config$resolution, collapse = ", ")

repeat {
  raw <- .ask(
    sprintf("  Enter resolution(s) in metres (5, 10, 20), comma-separated [%s]: ",
            default_res),
    default = default_res
  )
  res_vec <- suppressWarnings(as.integer(trimws(strsplit(raw, ",")[[1]])))
  if (any(is.na(res_vec)) || !all(res_vec %in% c(5L, 10L, 20L))) {
    cat("  Invalid input – please enter one or more of: 5, 10, 20.\n")
    next
  }
  break
}
reso <- res_vec

# --- Q4: NBS number(s) – only for NBS / scenario modes ----------------------
nbs_ids    <- 0
scen_num   <- NULL
lu_classes <- NULL

if (run_mode_label %in% c("nbs", "scenario")) {

  # load NBS lookup for validation
  nbs_tbl <- tryCatch(
    read.csv("sources/setup/tables/lu_NBS_tbl.csv", stringsAsFactors = FALSE),
    error = function(e) NULL
  )
  valid_nbs <- if (!is.null(nbs_tbl)) nbs_tbl$lu_nr else NULL

  .show_nbs_list <- function() {
    if (!is.null(nbs_tbl)) {
      cat("  Available NBS land-use numbers:\n")
      for (i in seq_len(nrow(nbs_tbl))) {
        cat(sprintf("    %3d  %s\n", nbs_tbl$lu_nr[i], nbs_tbl$description[i]))
      }
      cat("    0    no NBS (base run)\n")
    } else {
      cat("  (lu_NBS_tbl.csv not found – cannot list NBS options)\n")
    }
  }

  if (run_mode_label == "nbs") {
    cat("\n-- Step 4: NBS number(s) --\n")
    default_nbs <- paste(config$NBS_num, collapse = ", ")

    repeat {
      raw <- .ask(
        sprintf("  Enter NBS number(s), comma-separated (0 = base/no NBS) [%s]: ",
                default_nbs),
        default = default_nbs
      )
      if (tolower(trimws(raw)) == "list") {
        .show_nbs_list()
        next
      }
      nbs_vec <- suppressWarnings(as.integer(trimws(strsplit(raw, ",")[[1]])))
      if (any(is.na(nbs_vec))) {
        cat("  Invalid input – enter integer(s), or type 'list' for options.\n")
        next
      }
      bad_nbs <- nbs_vec[nbs_vec != 0 & !is.null(valid_nbs) & !(nbs_vec %in% valid_nbs)]
      if (length(bad_nbs) > 0) {
        cat(sprintf("  Unknown NBS number(s): %s\n  Type 'list' to see valid options.\n",
                    paste(bad_nbs, collapse = ", ")))
        next
      }
      break
    }
    nbs_ids <- nbs_vec

  } else {
    # scenario mode
    cat("\n-- Step 4: NBS scenario settings --\n")

    repeat {
      raw <- .ask("  Enter scenario number (must be > 100) [101]: ", default = "101")
      scen_num <- suppressWarnings(as.integer(trimws(raw)))
      if (is.na(scen_num) || scen_num <= 100) {
        cat("  Invalid – scenario number must be an integer greater than 100.\n")
        next
      }
      break
    }

    repeat {
      raw <- .ask("  Enter lu_classes ('wrl' or 'def') [def]: ", default = "def")
      lu_classes <- tolower(trimws(raw))
      if (!lu_classes %in% c("wrl", "def")) {
        cat("  Invalid – choose 'wrl' or 'def'.\n")
        next
      }
      break
    }
  }
}

# --- Q5: Recalculate LDD? ----------------------------------------------------
cat("\n-- Step 5: LDD recalculation --\n")
cat("  WARNING: recalculating the LDD for the whole catchment can take a very\n")
cat("  long time (15-30+ minutes). Only set to Y if the DEM has changed.\n")
default_ldd <- toupper(config$calc_ldd)

repeat {
  raw <- .ask(sprintf("  Recalculate LDD? (Y/N) [%s]: ", default_ldd),
              default = default_ldd)
  calc_ldd_input <- toupper(trimws(raw))
  if (!calc_ldd_input %in% c("Y", "N")) {
    cat("  Please enter Y or N.\n")
    next
  }
  break
}
calc_ldd_bool <- calc_ldd_input == "Y"

# --- Q6: CPU cores -----------------------------------------------------------
cat("\n-- Step 6: CPU cores for LISEM --\n")
default_cpu <- as.character(config$cpu_cores)

repeat {
  raw   <- .ask(sprintf("  Number of CPU cores (-1 = 50%%, 0 = all) [%s]: ",
                        default_cpu),
                default = default_cpu)
  ncpu  <- suppressWarnings(as.integer(trimws(raw)))
  if (is.na(ncpu)) {
    cat("  Invalid – please enter an integer.\n")
    next
  }
  break
}

# --- Q7: inith_cal – only for calibration runs ------------------------------
inith_cal <- as.numeric(config$inithcal)

if (run_mode_label == "cal") {
  cat("\n-- Step 7: Initial soil moisture calibration factor --\n")
  default_inith <- as.character(config$inithcal)

  repeat {
    raw       <- .ask(sprintf("  inith_cal (numeric, e.g. 0.65) [%s]: ", default_inith),
                      default = default_inith)
    inith_cal <- suppressWarnings(as.numeric(trimws(raw)))
    if (is.na(inith_cal)) {
      cat("  Invalid – please enter a number.\n")
      next
    }
    break
  }
}

# ---------------------------------------------------------------------------- #
# 3. Write updated values back to config.yaml                                  #
# ---------------------------------------------------------------------------- #

config$subcatchments <- as.list(points_id)
config$resolution    <- as.list(reso)
config$cpu_cores     <- ncpu
config$calc_ldd      <- calc_ldd_input
config$inithcal      <- inith_cal

# run_type and NBS_num: derive from run mode
if (run_mode_label == "cal") {
  config$run_type <- "cal"
  config$NBS_num  <- 0
} else if (run_mode_label %in% c("nbs", "scenario")) {
  config$run_type <- "base"
  config$NBS_num  <- as.list(nbs_ids)
} else {
  # data_prep only – keep existing run_type in config
}

write_yaml(config, config_path)
message("config.yaml updated with current session settings.\n")

# ---------------------------------------------------------------------------- #
# 4. Summary + confirmation                                                     #
# ---------------------------------------------------------------------------- #

cat("\n")
cat("# ------------------------------------------------------------------- #\n")
cat("#              Summary of selected settings                           #\n")
cat("# ------------------------------------------------------------------- #\n")
cat(sprintf("  Run mode         : %s\n",
            c(data_prep = "Data preparation only",
              cal        = "Calibration run",
              nbs        = "NBS run",
              scenario   = "NBS scenario run")[run_mode_label]))
cat(sprintf("  Subcatchment(s)  : %s\n", paste(points_id, collapse = ", ")))
cat(sprintf("  Resolution(s)    : %s m\n", paste(reso, collapse = ", ")))
if (run_mode_label == "nbs") {
  cat(sprintf("  NBS number(s)    : %s\n", paste(nbs_ids, collapse = ", ")))
}
if (run_mode_label == "scenario") {
  cat(sprintf("  Scenario number  : %d\n", scen_num))
  cat(sprintf("  lu_classes       : %s\n", lu_classes))
}
cat(sprintf("  Recalculate LDD  : %s\n", calc_ldd_input))
cat(sprintf("  CPU cores        : %d\n", ncpu))
if (run_mode_label == "cal") {
  cat(sprintf("  inith_cal        : %.4f\n", inith_cal))
}
cat("\n")

confirm <- .menu(
  choices = c("Yes – proceed", "No  – cancel (config.yaml already updated)"),
  title   = "Run the workflow with the settings above?"
)

if (confirm != 1) {
  message("Run cancelled. config.yaml has been updated but no steps were executed.")
  stop("Cancelled by user.", call. = FALSE)
}

# ---------------------------------------------------------------------------- #
# 5. Execution                                                                  #
# ---------------------------------------------------------------------------- #

# 5.0 Initialisation ---------------------------------------------------------
# load and set configured settings from config.yaml (sets points_id, reso, ncpu,
# swatre_file, DEBUGm, etc.)
source("sources/r_scripts/configuration.R")

# override configuration.R defaults with the interactively chosen values so
# they are consistent even if the user has not saved the yaml yet.
points_id  <- pts_vec       # subcatchments chosen above
reso       <- res_vec       # resolutions chosen above

# 5.1 Data preparation -------------------------------------------------------
message("\n--- Step 1: Data preparation ---")

source("sources/r_scripts/source_to_base_maps.R")

# copy relevant spatial data files into the folder structure
copy_spatial_data()

# build catchment maps for all configured resolutions
catch_maps_res()

# convert spatial data to PCRaster format for selected resolution(s)
spatial_data_to_pcr(res = reso)

# calculate local drain direction and subcatchment masks
# NOTE: force_ldd = TRUE triggers a full LDD recalculation which can take 15-30 min
ldd_subcatch(force_ldd = calc_ldd_bool, res = reso)

if (run_mode_label == "data_prep") {
  message("\nData preparation complete. Stopping here as requested.")
  message("Results are stored in LISEM_data/Geul_<res>m/maps/")
  stop("Data preparation only – done.", call. = FALSE)
}

# 5.2 Landuse and soil tables ------------------------------------------------
message("\n--- Step 2: Landuse and SWATRE tables ---")

source("sources/r_scripts/prepare_landuse_table.R")

# build calibrated landuse table (lu.tbl)
landuse_table_cal()

if (run_mode_label %in% c("nbs", "scenario")) {
  # extend lu table with NBS landuse classes (lu_nbs.tbl)
  landuse_table_nbs()
}

source("sources/r_scripts/swatre_input.R")

if (run_mode_label %in% c("nbs", "scenario")) {
  # use NBS-extended swatre file
  swatre_file <- "swatre_NBS.csv"
  soil_landuse_to_swatre(
    file       = "sources/setup/swatre/UBC_texture.csv",
    swatre_out = paste0("sources/setup/calibration/", swatre_file),
    do_NBS     = TRUE
  )
} else {
  # calibration run: use the standard calibration swatre file
  # swatre_file is set by configuration.R; re-generate it to reflect any changes
  soil_landuse_to_swatre(
    file       = "sources/setup/swatre/UBC_texture.csv",
    swatre_out = paste0("sources/setup/calibration/", swatre_file)
  )
}

# 5.3 Optional: load combined NBS scenario maps ------------------------------
if (run_mode_label == "scenario") {
  message("\n--- Step 3: Loading NBS scenario maps ---")
  source("sources/r_scripts/source_to_base_maps.R")
  for (j in seq_along(reso)) {
    load_scenario_maps(scen_num  = scen_num,
                       lu_classes = lu_classes,
                       res        = reso[j])
  }
}

# 5.4 Subcatchment databases -------------------------------------------------
message("\n--- Step 4: Building subcatchment databases ---")

source("sources/r_scripts/create_subcatch_db.R")

run_type <- if (run_mode_label == "cal") "cal" else "base"

for (i in seq_along(points_id)) {
  for (j in seq_along(reso)) {
    message(sprintf("  base_maps_subcatchment: catchment %d, %d m", points_id[i], reso[j]))
    base_maps_subcatchment(
      cell_size        = reso[j],
      sub_catch_number = points_id[i],
      run_type         = run_type,
      calc_ldd         = calc_ldd_bool
    )
  }
}

# 5.5 LISEM run files --------------------------------------------------------
message("\n--- Step 5: Creating LISEM run files ---")

source("sources/r_scripts/create_lisem_run.R")

if (run_mode_label == "cal") {
  # calibration: one run per subcatchment × resolution
  for (i in seq_along(points_id)) {
    for (j in seq_along(reso)) {
      message(sprintf("  create_lisem_run: catchment %d, %d m, run_type = cal",
                      points_id[i], reso[j]))
      create_lisem_run(
        resolution  = reso[j],
        catch_num   = points_id[i],
        swatre_file = swatre_file,
        run_type    = "cal",
        do_runfile  = TRUE,
        NBS_num     = 0,
        cpu_cores   = ncpu,
        inith_cal   = inith_cal
      )
    }
  }

} else {
  # NBS or scenario: loop over NBS numbers as well (0 = base, no NBS)
  for (k in seq_along(nbs_ids)) {
    for (i in seq_along(points_id)) {
      for (j in seq_along(reso)) {
        message(sprintf("  create_lisem_run: catchment %d, %d m, NBS_num = %d",
                        points_id[i], reso[j], nbs_ids[k]))
        create_lisem_run(
          resolution  = reso[j],
          catch_num   = points_id[i],
          swatre_file = swatre_file,
          run_type    = "base",
          do_runfile  = TRUE,
          NBS_num     = nbs_ids[k],
          cpu_cores   = ncpu,
          dir_name    = config$dir_name
        )
      }
    }
  }
}

# ---------------------------------------------------------------------------- #
# 6. Done                                                                       #
# ---------------------------------------------------------------------------- #

message("\n# ------------------------------------------------------------------- #")
message("#  Interactive workflow complete!                                      #")
message("#                                                                      #")
message("#  Run files are stored in: LISEM_runs/                               #")
message("#  Open the .run files with the OpenLISEM GUI or run in batch mode.   #")
message("# ------------------------------------------------------------------- #\n")
