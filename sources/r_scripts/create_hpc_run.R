# remove swatre building for every sub catch
# make one main function, include option to subset
# use ID csv file to identify subcatch numbers

create_hpc_run <- function(subset = NULL,
                           swatre_file = "",
                           NBS_num = 0
                           ) {
  # check if it is a base run, or simulation a NBS
  if (NBS_num != 0) {
    do_NBS = TRUE
  } else {
    do_NBS = FALSE
  }
  
  
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
    create_lisem_run(resolution = 10, catch_num = subnums[i], swatre_file = swatre_file,
                     run_type = "base", do_hpc = TRUE, cpu_cores = ncpu, NBS_num = NBS_num)
  }
  
  #make the swatre tables only ones
  dir_swatre <- "LISEM_runs/hpc_runs/swatre/"

  source("sources/r_scripts/swatre_input.R")
  make_swatre_tables(cal_file = swatre_file,
                     swatre_dir = dir_swatre,
                     do_NBS = do_NBS)
  
}
