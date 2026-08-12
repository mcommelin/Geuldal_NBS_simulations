# interactive_run.R
#
# Purpose:
#   Interactive guided entry point for the LISEM Geuldal manual workflow.
#   Run this script in an RStudio session to be guided through all major
#   settings via a series of questions. It always rebuilds ./config.yaml
#   from config_template.yaml, then executes exactly the same functions as
#   LISEM_Geuldal_full_workflow.R based on your answers.
#
# How to run:
#   Open the project in RStudio and execute:
#     source("sources/r_scripts/interactive_run.R")
#   Or open the file and press Ctrl+Shift+Enter to source it.
#
#   The script also works non-interactively (Rscript --vanilla), but in that
#   case all prompts will use the template defaults without asking.
#
# Depends on:
#   yaml (already used elsewhere in the project), base R only.

# ---------------------------------------------------------------------------- #
# 0. Helpers                                                                    #
# ---------------------------------------------------------------------------- #

# load yaml - install only if missing (same pattern as configuration.R)
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

if (!file.exists(template_path)) {
  stop("config_template.yaml not found. ",
       "Make sure you are running from the project root directory.")
}

# Read template lines (for comment-preserving write-back).
# The template is always used as the structural base for writing config.yaml,
# so that comments are preserved.  Default values shown to the user, however,
# are taken from an existing config.yaml when one is present, so that previous
# session choices are remembered.
template_lines <- readLines(template_path)
if (file.exists(config_path)) {
  config <- yaml.load_file(config_path)
  message("Using existing config.yaml as default values.")
} else {
  config <- yaml.load_file(template_path)
  message("No config.yaml found - using config_template.yaml defaults.")
}

# Helper: update a scalar key's value in the template lines, preserving comments.
.set_scalar <- function(lines, key, value) {
  pat <- paste0("^", key, "\\s*:")
  idx <- grep(pat, lines)
  if (length(idx) == 0) return(lines)
  if (is.character(value)) {
    formatted <- paste0('"', value, '"')
  } else {
    formatted <- as.character(value)
  }
  lines[idx[1]] <- paste0(key, ": ", formatted)
  lines
}

# Helper: update a list key in the template lines, preserving comments.
.set_list <- function(lines, key, values) {
  pat <- paste0("^", key, "\\s*:")
  idx <- grep(pat, lines)
  if (length(idx) == 0) return(lines)
  lines[idx[1]] <- paste0(key, ": [", paste(values, collapse = ", "), "]")
  lines
}

# ---------------------------------------------------------------------------- #
# 2. One-time setup questions (Q1-Q4) and data preparation                     #
# These questions concern the environment / installation and are asked once.    #
# ---------------------------------------------------------------------------- #

cat("\n")
cat("# ------------------------------------------------------------------- #\n")
cat("#          LISEM Geuldal - Interactive workflow setup                  #\n")
cat("# ------------------------------------------------------------------- #\n")
cat("Press Enter to accept the default value shown in [brackets].\n\n")
cat("Enter the number of the option you want to execute")

# --- Q1: install packages ---------------------------------------------------
cat("-- Step 1: Package installation --\n")
inst_choice <- .menu(
  choices = c("Y - automatically install required packages",
              "N - skip (make sure packages are already installed)"),
  title   = "Do you want to automatically install required packages?"
)
install_packages <- if (inst_choice == 2) "N" else "Y"
template_lines   <- .set_scalar(template_lines, "install_packages", install_packages)

# --- Q2: debug messages -----------------------------------------------------
cat("\n-- Step 2: Debug messages --\n")
debug_choice <- .menu(
  choices = c("Y - show debug messages during execution",
              "N - nu messages - less output on the console"),
  title   = "Do you want debug messages during execution of functions?"
)
debug_messages <- if (debug_choice == 2) "N" else "Y"
template_lines <- .set_scalar(template_lines, "debug_messages", debug_messages)

# --- Q3: Miniconda path -----------------------------------------------------
cat("\n-- Step 3: Miniconda path --\n")
default_miniconda <- config$miniconda_path
miniconda_path    <- .ask(
  sprintf("  Path to your local Miniconda installation [%s]: ", default_miniconda),
  default = default_miniconda
)
template_lines <- .set_scalar(template_lines, "miniconda_path", miniconda_path)

# --- Q4: conda environment --------------------------------------------------
cat("\n-- Step 4: Conda environment --\n")
default_conda <- config$conda_env
conda_env     <- .ask(
  sprintf("  Name of the conda environment where PCRaster is installed [%s]: ",
          default_conda),
  default = default_conda
)
template_lines <- .set_scalar(template_lines, "conda_env", conda_env)

# ---------------------------------------------------------------------------- #
# 3. Initialisation (runs once, before any run loop)                           #
# ---------------------------------------------------------------------------- #

# Write a temporary config so that configuration() has all environment settings.
writeLines(template_lines, config_path)

source("sources/r_scripts/configuration.R")
configuration(file = config_path)

# Load lookup tables once – reused in every loop iteration.
points_tbl <- tryCatch(
  read.csv("sources/setup/outpoints_description.csv", stringsAsFactors = FALSE),
  error = function(e) NULL
)
nbs_tbl <- tryCatch(
  read.csv("sources/setup/tables/lu_NBS_tbl.csv", stringsAsFactors = FALSE),
  error = function(e) NULL
)
valid_pts <- if (!is.null(points_tbl)) unique(points_tbl$point) else NULL
nbs_options_tbl <- if (!is.null(nbs_tbl)) {
  subset(nbs_tbl, !is.na(lu_nr) & lu_nr >= 10)
} else {
  NULL
}
valid_nbs <- if (!is.null(nbs_options_tbl)) nbs_options_tbl$lu_nr else NULL

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
    cat("  (outpoints_description.csv not found - cannot list catchments)\n")
  }
}

.show_nbs_list <- function() {
  if (!is.null(nbs_options_tbl)) {
    cat("  Available NBS numbers:\n")
    for (i in seq_len(nrow(nbs_options_tbl))) {
      cat(sprintf("    %3d  %s\n",
                  nbs_options_tbl$lu_nr[i], nbs_options_tbl$description[i]))
    }
    cat("    0    no NBS (base run)\n")
  } else {
    cat("  (lu_NBS_tbl.csv not found - cannot list NBS options)\n")
  }
}

# ---------------------------------------------------------------------------- #
# 4. Data preparation (runs once per session)                                  #
# ---------------------------------------------------------------------------- #

.available_resolutions <- c(5L, 10L, 20L)

.prepared_resolutions <- function(resolutions = .available_resolutions) {
  prepared <- integer(0)
  for (r in resolutions) {
    data_prep_dir <- sprintf("LISEM_data/Geul_%dm/maps", r)
    ready <- dir.exists(data_prep_dir) &&
      length(list.files(data_prep_dir, pattern = "\\.map$")) > 0
    if (ready) prepared <- c(prepared, r)
  }
  unique(prepared)
}

.run_data_prep <- function(resolutions) {
  message("\n--- Step 1: Data preparation ---")
  source("sources/r_scripts/source_to_base_maps.R")
  copy_spatial_data()
  catch_maps_res()
  spatial_data_to_pcr(res = resolutions)
  ldd_subcatch(force_ldd = FALSE, res = resolutions)
}

# ---------------------------------------------------------------------------- #
# 5. Run loop – repeated for each additional run the user wants to make        #
# Q5-Q8 and Q10 are asked at the start of every loop iteration.                #
# Existing run files are NEVER deleted; new runs are added alongside them.     #
# ---------------------------------------------------------------------------- #

.run_loop_start <- TRUE   # controls whether we are in the first pass

repeat {

  if (.run_loop_start) {
    .run_loop_start <- FALSE
  } else {
    # ---- Between-run menu -------------------------------------------------- #
    cat("\n")
    cat("# ------------------------------------------------------------------- #\n")
    cat("#                  Run complete - what next?                          #\n")
    cat("# ------------------------------------------------------------------- #\n")
    cat("  Note: existing run files in LISEM_runs/ are NOT deleted.\n")
    cat("  New runs are always added alongside the existing ones.\n\n")

    next_action <- .menu(
      choices = c(
        "Make another run   (repeat run settings only, keep current setup)",
        "Redo whole setup   (restart from the beginning, incl. Q1-Q4)",
        "Stop               (exit the interactive workflow)"
      ),
      title = "What would you like to do next?"
    )

    if (next_action == 3 || next_action == 0) {
      message("\nWorkflow finished. Run files are in LISEM_runs/")
      break
    }

    if (next_action == 2) {
      message("\nRestarting the full setup...")
      source("sources/r_scripts/interactive_run.R")
      break
    }

    # next_action == 1: fall through to ask run-specific questions again.
    # Reload config defaults from the last-written config.yaml so that
    # the previous choices appear as the new defaults.
    if (file.exists(config_path)) config <- yaml.load_file(config_path)
    cat("\n")
    cat("# ------------------------------------------------------------------- #\n")
    cat("#                  New run - run settings                             #\n")
    cat("# ------------------------------------------------------------------- #\n")
    cat("Press Enter to accept the default value shown in [brackets].\n\n")
  }

  # ---- Q5: Run mode -------------------------------------------------------- #
  cat("-- Run mode --\n")
  run_mode <- .menu(
    choices = c(
      "Data preparation only  (make base catchment maps, no LISEM run)",
      "Calibration run        (database for 3 selected rainfall events)",
      "NBS/base run           (simulations of single NBS, or base run)",
      "NBS scenario run       (simulations of scenarios of NBS)"
    ),
    title = "What type of LISEM simulation do you want to prepare"
  )

  if (run_mode == 0) stop("No selection made - aborting.")

  run_mode_label <- c("data_prep", "cal", "nbs", "scenario")[run_mode]

  # ---- Q6: Subcatchment number(s) ----------------------------------------- #
  cat("\n-- Subcatchment(s) --\n")
  cat("Which subcatchments do you want to simulate?")
  cat("\nEnter the number(s) of the subcatchments or type 'list' for options")
  default_pts <- paste(config$subcatchments, collapse = ", ")

  repeat {
    raw <- .ask(
      sprintf("  Enter subcatchment number(s), comma-separated [%s]: ", default_pts),
      default = default_pts
    )
    if (tolower(trimws(raw)) == "list") { .show_subcatch_list(); next }
    pts_vec <- suppressWarnings(as.integer(trimws(strsplit(raw, ",")[[1]])))
    if (any(is.na(pts_vec))) {
      cat("  Invalid input - please enter integer(s), or type 'list' for options.\n"); next
    }
    if (!is.null(valid_pts) && !all(pts_vec %in% valid_pts)) {
      bad <- pts_vec[!pts_vec %in% valid_pts]
      cat(sprintf("  Unknown subcatchment number(s): %s\n", paste(bad, collapse = ", ")))
      cat("  Type 'list' to see valid options.\n"); next
    }
    break
  }
  points_id      <- pts_vec
  template_lines <- .set_list(template_lines, "subcatchments", points_id)

  # ---- Q7: Resolution ------------------------------------------------------ #
  cat("\n-- Resolution --\n")
  cat("Which resolution do you want to run the simulations?")
  cat("\nChoose from 5, 10, 20 m, default = 10")
  default_res <- paste(config$resolution, collapse = ", ")

  repeat {
    raw <- .ask(
      sprintf("  Enter resolution(s) in metres (5, 10, 20), comma-separated [%s]: ",
              default_res),
      default = default_res
    )
    res_vec <- suppressWarnings(as.integer(trimws(strsplit(raw, ",")[[1]])))
    if (any(is.na(res_vec)) || !all(res_vec %in% c(5L, 10L, 20L))) {
      cat("  Invalid input - please enter one or more of: 5, 10, 20.\n"); next
    }
    break
  }
  reso           <- res_vec
  template_lines <- .set_list(template_lines, "resolution", reso)

  # ---- Data preparation for selected resolution(s) ------------------------- #
  cat("\n-- Data preparation check --\n")
  prepared_res <- .prepared_resolutions()

  if (length(prepared_res) > 0) {
    cat(sprintf("  Found prepared dataset for resolution(s): %s m\n",
                paste(prepared_res, collapse = ", ")))
  } else {
    cat("  No prepared datasets found yet.\n")
  }

  missing_res <- setdiff(reso, prepared_res)
  do_prepare  <- FALSE
  prep_res    <- integer(0)
  skip_run    <- FALSE

  if (length(missing_res) > 0) {
    prep_choice <- .menu(
      choices = c(
        sprintf("Prepare missing selected resolution(s): %s m",
                paste(missing_res, collapse = ", ")),
        sprintf("Redo preparation for selected resolution(s): %s m",
                paste(reso, collapse = ", ")),
        "Cancel this run"
      ),
      title = "Some selected resolutions are not prepared yet."
    )
    if (prep_choice == 1) {
      do_prepare <- TRUE
      prep_res   <- missing_res
    } else if (prep_choice == 2) {
      do_prepare <- TRUE
      prep_res   <- reso
    } else {
      skip_run <- TRUE
    }
  } else {
    prep_choice <- .menu(
      choices = c(
        "Use existing prepared data",
        "Prepare other resolution(s)",
        sprintf("Redo preparation for selected resolution(s): %s m",
                paste(reso, collapse = ", ")),
        "Cancel this run"
      ),
      title = "Selected resolutions are already prepared."
    )

    if (prep_choice == 2) {
      repeat {
        raw_extra <- .ask(
          "  Enter other resolution(s) to prepare (5, 10, 20), comma-separated: "
        )
        extra_res <- suppressWarnings(as.integer(trimws(strsplit(raw_extra, ",")[[1]])))
        if (length(extra_res) == 0 || any(is.na(extra_res)) ||
            !all(extra_res %in% .available_resolutions)) {
          cat("  Invalid input - please enter one or more of: 5, 10, 20.\n"); next
        }
        do_prepare <- TRUE
        prep_res   <- unique(extra_res)
        break
      }
    } else if (prep_choice == 3) {
      do_prepare <- TRUE
      prep_res   <- reso
    } else if (prep_choice == 4 || prep_choice == 0) {
      skip_run <- TRUE
    }
  }

  if (do_prepare) .run_data_prep(prep_res)

  if (skip_run) {
    message("Run cancelled before execution.")
    next
  }

  if (run_mode_label == "data_prep") {
    message("\nData preparation completed/confirmed for selected settings.")
    message("Results are stored in LISEM_data/Geul_<res>m/maps/")
    next
  }

  # ---- Q8: NBS_num --------------------------------------------------------- #
  # In both nbs and scenario modes the number goes to NBS_num.
  # When NBS_num > 100, load_scenario_maps() is called before create_lisem_run().
  NBS_num    <- 0
  lu_classes <- NULL

  if (run_mode_label %in% c("nbs", "scenario")) {

    if (run_mode_label == "nbs") {
      cat("\n-- NBS number(s) --\n")
      cat("Which NBS do you want to simulate?")
      cat("\n  Enter integer(s), or type 'list' for options.\n")
      default_nbs <- paste(config$NBS_num, collapse = ", ")

      repeat {
        raw <- .ask(
          sprintf("  Enter NBS number(s), comma-separated (0 = base/no NBS) [%s]: ",
                  default_nbs),
          default = default_nbs
        )
        if (tolower(trimws(raw)) == "list") { .show_nbs_list(); next }
        nbs_vec <- suppressWarnings(as.integer(trimws(strsplit(raw, ",")[[1]])))
        if (any(is.na(nbs_vec))) {
          cat("  Invalid input - enter integer(s), or type 'list' for options.\n"); next
        }
        bad_nbs <- nbs_vec[nbs_vec != 0 & !is.null(valid_nbs) & !(nbs_vec %in% valid_nbs)]
        if (length(bad_nbs) > 0) {
          cat(sprintf("  Unknown NBS number(s): %s\n  Type 'list' to see valid options.\n",
                      paste(bad_nbs, collapse = ", "))); next
        }
        if (any(nbs_vec > 100)) {
          cat("  NBS numbers <= 100 expected for 'NBS run'. Use 'NBS scenario run' for values > 100.\n"); next
        }
        break
      }
      NBS_num <- nbs_vec

    } else {
      # NBS scenario run: NBS_num > 100
      cat("\n-- NBS scenario number --\n")

      repeat {
        raw     <- .ask("  Enter scenario number (must be > 100) [101]: ", default = "101")
        NBS_num <- suppressWarnings(as.integer(trimws(raw)))
        if (is.na(NBS_num) || NBS_num <= 100) {
          cat("  Invalid - scenario number must be an integer greater than 100.\n"); next
        }
        break
      }

      repeat {
        cat("  What is the lu_class of the added scenario? \n  See manual for explanation.")
        raw        <- .ask("  Enter lu_classes ('wrl' or 'def') [def]: ", default = "def")
        lu_classes <- tolower(trimws(raw))
        if (!lu_classes %in% c("wrl", "def")) {
          cat("  Invalid - choose 'wrl' or 'def'.\n"); next
        }
        break
      }
    }
  }

  template_lines <- .set_list(template_lines, "NBS_num", NBS_num)

  # ---- Q9: CPU cores ------------------------------------------------------- #
  cat("\n-- CPU cores for LISEM --\n")
  default_cpu <- as.character(config$cpu_cores)

  repeat {
    raw  <- .ask(sprintf("  Number of CPU cores (0 = all) [%s]: ",
                         default_cpu),
                 default = default_cpu)
    ncpu <- suppressWarnings(as.integer(trimws(raw)))
    if (is.na(ncpu)) { cat("  Invalid - please enter an integer.\n"); next }
    break
  }
  template_lines <- .set_scalar(template_lines, "cpu_cores", ncpu)

  # ---- Q10: inith_cal (calibration runs only) ------------------------------ #
  inith_cal <- as.numeric(config$inithcal)

  if (run_mode_label == "cal") {
    cat("\n-- Initial soil moisture calibration factor --\n")
    default_inith <- as.character(config$inithcal)

    repeat {
      raw       <- .ask(sprintf("  inith_cal (numeric, e.g. 0.65) [%s]: ", default_inith),
                        default = default_inith)
      inith_cal <- suppressWarnings(as.numeric(trimws(raw)))
      if (is.na(inith_cal)) { cat("  Invalid - please enter a number.\n"); next }
      break
    }
  }
  template_lines <- .set_scalar(template_lines, "inithcal", inith_cal)

  # Set run_type in the template lines
  run_type_val   <- if (run_mode_label == "cal") "cal" else "base"
  template_lines <- .set_scalar(template_lines, "run_type", run_type_val)

  # ---- Write updated config.yaml ------------------------------------------- #
  writeLines(template_lines, config_path)
  message("config.yaml updated with current run settings.\n")

  # ---- Summary + confirmation ---------------------------------------------- #
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
    cat(sprintf("  NBS number(s)    : %s\n", paste(NBS_num, collapse = ", ")))
  }
  if (run_mode_label == "scenario") {
    cat(sprintf("  NBS_num (scenario): %d\n", NBS_num))
    cat(sprintf("  lu_classes       : %s\n", lu_classes))
  }
  cat(sprintf("  CPU cores        : %d\n", ncpu))
  if (run_mode_label == "cal") {
    cat(sprintf("  inith_cal        : %.4f\n", inith_cal))
  }
  cat("\n")

  confirm <- .menu(
    choices = c("Yes - proceed", "No  - cancel (config.yaml already updated)"),
    title   = "Run the workflow with the settings above?"
  )

  if (confirm != 1) {
    message("Run cancelled. config.yaml has been updated but no steps were executed.")
    next   # go back to the between-run menu
  }

  # ---- Execution ----------------------------------------------------------- #

  # 5.2 Landuse and soil tables
  message("\n--- Landuse and SWATRE tables ---")

  source("sources/r_scripts/prepare_landuse_table.R")
  landuse_table_cal()

  if (run_mode_label %in% c("nbs", "scenario")) {
    landuse_table_nbs()
  }

  source("sources/r_scripts/swatre_input.R")

  if (run_mode_label %in% c("nbs", "scenario")) {
    swatre_file <- "swatre_NBS.csv"
    soil_landuse_to_swatre(
      file       = "sources/setup/swatre/UBC_texture.csv",
      swatre_out = paste0("sources/setup/calibration/", swatre_file),
      do_NBS     = TRUE
    )
  } else {
    # swatre_file is set to "cal_OM_swatre.csv" by configuration() above
    soil_landuse_to_swatre(
      file       = "sources/setup/swatre/UBC_texture.csv",
      swatre_out = paste0("sources/setup/calibration/", swatre_file)
    )
  }

  # 5.3 Load combined NBS scenario maps when any selected NBS_num > 100
  if (run_mode_label %in% c("scenario") && any(NBS_num > 100)) {
    message("\n--- Loading NBS scenario maps ---")
    source("sources/r_scripts/source_to_base_maps.R")
    for (j in seq_along(reso)) {
      load_scenario_maps(scen_num   = NBS_num,
                         lu_classes = lu_classes,
                         res        = reso[j])
    }
  }

  # 5.4 Subcatchment databases
  message("\n--- Building subcatchment databases ---")

  source("sources/r_scripts/create_subcatch_db.R")

  run_type <- if (run_mode_label == "cal") "cal" else "base"

  for (i in seq_along(points_id)) {
    for (j in seq_along(reso)) {
      message(sprintf("  base_maps_subcatchment: catchment %d, %d m", points_id[i], reso[j]))
      base_maps_subcatchment(
        cell_size        = reso[j],
        sub_catch_number = points_id[i],
        run_type         = run_type
      )
    }
  }

  # 5.5 LISEM run files
  message("\n--- Creating LISEM run files ---")

  source("sources/r_scripts/create_lisem_run.R")

  if (run_mode_label == "cal") {
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
    # NBS run or NBS scenario run: NBS_num is passed directly to create_lisem_run.
    # When NBS_num > 100 the scenario maps are already loaded in step 5.3 above.
    nbs_ids <- NBS_num
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
            cpu_cores   = ncpu
          )
        }
      }
    }
  }

  message("\n# ------------------------------------------------------------------- #")
  message("#  Run complete!                                                       #")
  message("#  Run files are stored in: LISEM_runs/                               #")
  message("#  Open the .run files with the OpenLISEM GUI.                        #")
  message("# ------------------------------------------------------------------- #\n")

}  # end repeat (run loop)
