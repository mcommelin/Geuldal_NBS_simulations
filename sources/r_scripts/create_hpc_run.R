# remove swatre building for every sub catch
# make one main function, include option to subset
# use ID csv file to identify subcatch numbers

create_hpc_run <- function(subset = NULL) {
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
                     run_type = "base", do_hpc = TRUE, cpu_cores = ncpu)
  }
  
}