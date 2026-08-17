# configuration

configuration <- function(file = "config.yaml") {

if (exists("do_hpc")) {
if (do_hpc == TRUE) {
  config <- ini
}} else {
  if (!require("yaml")) install.packages("yaml", repos='https://cloud.r-project.org')
  library(yaml)
config <- yaml.load_file(file)
}
ins <- config$install_packages

# load all packages
if (ins == "Y") {
#if (!require("hydroGOF")) install.packages("hydroGOF", repos='https://cloud.r-project.org')
if (!require("gdalUtilities")) install.packages("gdalUtilities", repos='https://cloud.r-project.org')
if (!require("terra")) install.packages("terra", repos='https://cloud.r-project.org')
if (!require("remotes")) install.packages("remotes", repos='https://cloud.r-project.org')
if (!require("RSAGA")) install.packages("RSAGA", repos='https://cloud.r-project.org')
#if (!require("raster")) install.packages("raster", repos='https://cloud.r-project.org')
#if (!require("cowplot")) install.packages("cowplot", repos='https://cloud.r-project.org')
if (!require("sf")) install.packages("sf", repos='https://cloud.r-project.org')
if (!require("conflicted")) install.packages("conflicted", repos='https://cloud.r-project.org')
if (!require("tidyverse")) install.packages("tidyverse", repos='https://cloud.r-project.org')
#if (!require("sensobol")) install.packages("sensobol", repos='https://cloud.r-project.org')
#if (!require("foreach")) install.packages("foreach", repos='https://cloud.r-project.org')
#if (!require("doParallel")) install.packages("doParallel", repos='https://cloud.r-project.org')
if (!require("reticulate")) install.packages("reticulate", repos='https://cloud.r-project.org')
  library(reticulate)

  # --- Automatic conda / environment provisioning ---
  # All conda setup is wrapped in tryCatch so a broken conda installation
  # gives a clear error rather than a confusing downstream failure.

  # 1. Ensure Miniconda itself exists; install silently if not.
  if (!dir.exists(config$miniconda_path)) {
    message("Miniconda not found at '", config$miniconda_path,
            "' – installing Miniconda now (this may take a few minutes)...")
    tryCatch(
      reticulate::install_miniconda(path = config$miniconda_path),
      error = function(e) stop(
        "Miniconda installation failed. Please install Miniconda manually to '",
        config$miniconda_path, "' and re-run.\nOriginal error: ", conditionMessage(e))
    )
  }

  # 2. List existing conda environments (used for idempotent creation below).
  existing_envs <- tryCatch(
    reticulate::conda_list()$name,
    error = function(e) stop(
      "Could not list conda environments. Is conda/Miniconda correctly installed at '",
      config$miniconda_path, "'?\nOriginal error: ", conditionMessage(e))
  )

  # 3. Create a dedicated 'rosetta' conda environment for rosettaPTF / rosetta-soil.
  #    Kept separate from the PCRaster env to avoid ABI conflicts between
  #    conda-forge C++ binaries (PCRaster/GDAL) and pip-installed scientific packages.
  rosetta_env_name <- "rosetta"
  if (!(rosetta_env_name %in% existing_envs)) {
    message("Creating dedicated '", rosetta_env_name,
            "' conda environment for rosettaPTF (Python only)...")
    tryCatch(
      reticulate::conda_create(envname = rosetta_env_name, python_version = "3.11"),
      error = function(e) stop(
        "Failed to create '", rosetta_env_name, "' conda environment.\n",
        "Original error: ", conditionMessage(e))
    )
  }

  # 4. Create the PCRaster conda environment if it does not already exist.
  if (!(config$conda_env %in% existing_envs)) {
    message("Creating '", config$conda_env,
            "' conda environment and installing PCRaster from conda-forge",
            " (this may take a few minutes)...")
    tryCatch({
      reticulate::conda_create(envname = config$conda_env, python_version = "3.11")
      reticulate::conda_install(envname = config$conda_env,
                                packages = "pcraster",
                                channel  = "conda-forge")
    }, error = function(e) stop(
      "Failed to create '", config$conda_env, "' conda environment with PCRaster.\n",
      "Original error: ", conditionMessage(e))
    )
  }

  # 5. Activate the dedicated rosetta environment for reticulate / rosettaPTF calls.
  rosetta_env_path <- paste0(config$miniconda_path, "/envs/", rosetta_env_name)
  use_condaenv(condaenv = rosetta_env_path, required = TRUE)

  # 6. Self-healing rosettaPTF + rosetta-soil install:
  #    Try the *latest* versions first; only fall back to known-good pinned versions
  #    (rosettaPTF@8e81f4e + numpy==1.26.4 + rosetta-soil==0.1.2) if a smoke-test
  #    fails.  The fallback pins numpy==1.26.4 together with rosetta-soil so pip
  #    resolves them jointly, avoiding the numpy ABI / .npz array-reading error
  #    that arises when numpy is installed as an unpinned transitive dependency and
  #    later upgraded to an incompatible version.

  # Helper: run a smoke-test and return TRUE only when rosetta returns a valid,
  # non-empty result with at least one non-NA numeric value.
  rosetta_smoke_test <- function() {
    tryCatch({
      test <- rosettaPTF::run_rosetta(list(c(30, 30, 40, 1.5)), rosetta_version = 3)
      (is.data.frame(test) || is.list(test)) &&
        length(test) > 0 &&
        any(!is.na(unlist(test[sapply(test, is.numeric)])))
    }, error = function(e) FALSE)
  }

  if (!require("rosettaPTF", quietly = TRUE)) {
    message("Installing latest rosettaPTF from GitHub...")
    remotes::install_github("ncss-tech/rosettaPTF")
    message("Installing latest rosetta-soil into '", rosetta_env_name, "' environment...")
    py_install("rosetta-soil", pip = TRUE, envname = rosetta_env_name)
  }

  library(rosettaPTF)

  # Smoke-test: verify the install actually works end-to-end.
  rosetta_ok <- rosetta_smoke_test()

  if (!rosetta_ok) {
    message("Latest rosetta-soil/rosettaPTF failed validation (possible numpy ABI mismatch) – ",
            "falling back to known-good pinned versions...")
    remotes::install_github(
      "ncss-tech/rosettaPTF@8e81f4e98d6e1e0758e5b076a1c7321ea26ea676",
      force = TRUE)
    # Install numpy==1.26.4 and rosetta-soil==0.1.2 together so pip resolves
    # them jointly and the numpy ABI matches what the bundled .npz files expect.
    py_install(c("numpy==1.26.4", "rosetta-soil==0.1.2"),
               pip = TRUE, envname = rosetta_env_name)
    library(rosettaPTF)

    # Re-validate after the fallback install.
    rosetta_ok <- rosetta_smoke_test()

    if (!rosetta_ok) {
      stop("rosettaPTF / rosetta-soil installation failed even with known-good pinned ",
           "versions (rosettaPTF@8e81f4e, numpy==1.26.4, rosetta-soil==0.1.2).\n",
           "Please check your conda/Python setup and review the install block in ",
           "'sources/r_scripts/configuration.R'.")
    }
  } else {
    if (isTRUE(config$debug_messages == "Y")) message("rosettaPTF validation passed – install looks good.")
  }

} else {
  print("Make sure all packages required are installed, see 'sources/r_scripts/configuration.R'")
  Sys.sleep(1)
}

#library(hydroGOF)
library(gdalUtilities)
library(terra)
library(raster)
#library(cowplot)
library(sf)
library(conflicted)
library(tidyverse)
#library(sensobol)
#library(foreach)
#library(doParallel)
library(reticulate)
library(rosettaPTF)


# load configuration
DEBUGm = if(config$debug_messages == "Y") {TRUE} else {FALSE}
assign("DEBUGm", DEBUGm,envir=parent.env(environment()))


# make global choices for conflicting functions
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::summarise)
conflicts_prefer(dplyr::arrange)
conflicts_prefer(dplyr::rename)

# unload RSAGA gives function conflicts
unloadNamespace("RSAGA")
unloadNamespace("plyr")

# load pcraster functions
source("sources/r_scripts/pcrasteR.R")
set_pcraster(env = config$conda_env, miniconda = config$miniconda_path)

#set digits to 10 for detail in coordinates
options(digits = 10)

# load helper functions coded for this project
source("sources/r_scripts/aux_functions.R")

#! Always load the following data - adjust if needed for custom settings
points_id <- config$subcatchments 
assign("points_id", points_id,envir=parent.env(environment()))
reso <- config$resolution
assign("reso", reso,envir=parent.env(environment()))

# load subcatchment points csv file
points <- read_csv("sources/setup/outpoints_description.csv", show_col_types = FALSE)
assign("points", points,envir=parent.env(environment()))

# swatre file
swatre_file <- "cal_OM_swatre.csv"
assign("swatre_file", swatre_file, envir=parent.env(environment()))

# cpu cores
#TODO this doesn't work well - solve
ncpu <- config$cpu_cores
if (ncpu == -1) {
  ncpu <- floor(num_cores() / 2)
}
assign("ncpu", ncpu,envir=parent.env(environment()))
}

