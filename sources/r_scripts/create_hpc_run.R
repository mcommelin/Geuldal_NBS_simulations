


#' Wrapper around [create_lisem_run()] to make an input dataset which can be 
#' send to an hpc
#'
#'@description This function can only for subcatchments that are already prepared
#' with [create_subcatch_db()].
#'
#' @param subset if NULL then the whole Geuldal will be prepared. Otherwise a 
#' subset of subcatchment number in the Geuldal.
#' @param swatre_file  character string with the name of the used swatre input file.
#' Should be stored in ./sources/setup/calibration
#' @param NBS_num Number of the NBS you want to add to the simulation. 0 = no nbs simulated. 
#' The number corresponds to the landuse number in ./sources/setup/tables/lu_NBS_tbl.csv
#' @param resolution Number indicating the resolution of the dataset. Currently 
#' 5, 10 and 20 meter are available
#' @param do_runfile Boolean. Do you want to make all runfile again? Default = TRUE
#' @param cpu_cores Number of cores which are assigned to the OpenLISEM run.
#' @param dir_name Character. Additional folder name to place the produced data.
#' Will be placed at ./LISEM_runs/hpc_runs/**dir_name** Should end with a "/"! 
#' @param run_type Either "cal" or "base". cal = calibration run with date specific 
#' maps and rainfall events. base = standard conditions for scenario testing
#' @param inith_cal Calibration factor multiplying inithead for the whole 
#' catchment. Only used in hpc setup.
#'
#' @returns creates a map and runfile dataset to run OpenLISEM for (a subset of)
#' the Geulcatchment
#'

create_hpc_run <- function(subset = NULL,
                           swatre_file = "",
                           NBS_num = 0,    
                           resolution = NULL,
                           dir_name = "",
                           run_type = "",
                           do_runfile = TRUE,
                           cpu_cores = 0,
                           inith_cal = 1
                           ) {
  
  # check if a subset is done of the Geul, otherwise make everything
  if (is.null(subset)) {
    #load the csv file to identify all sub catch numbers
    hpc_ids <- read_csv("sources/setup/hpc/subcatch_id_link.csv", show_col_types = FALSE)
    
    #produce all subcatchments base maps
    subnums <- hpc_ids$LISEM_ID
  } else {
    subnums <- subset
  }
  
  source("sources/r_scripts/create_lisem_run.R")
  
  for (i in seq_along(subnums)) {
    create_lisem_run(resolution = resolution, catch_num = subnums[i], swatre_file = swatre_file,
                     run_type = run_type, do_hpc = TRUE, cpu_cores = cpu_cores, NBS_num = NBS_num,
                     dir_name = dir_name, inith_cal = inith_cal)
  }
  
  # check if it is a base run, or simulation a NBS
  if (NBS_num != 0) {
    do_NBS = TRUE
  } else {
    do_NBS = FALSE
  }
  
  #make the swatre tables only ones
  dir_swatre <- "LISEM_runs/hpc_runs/swatre/"

  source("sources/r_scripts/swatre_input.R")
  make_swatre_tables(cal_file = swatre_file,
                     swatre_dir = dir_swatre,
                     do_NBS = do_NBS)
  
}
