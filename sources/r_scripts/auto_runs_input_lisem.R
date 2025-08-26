# General options --------------------------------------------------------------
#'Code to prepare X directories with inputs for OL simulations
#' default system is Ubuntu remote
#'  
#'To run this code smoothly, make choices on the following settings:
#'1. Do you want to get verbose feedback on reading data files?
#'default = FALSE, change to TRUE if you want this data in the console
show_data_info <- FALSE

#'2. set the location where PCRaster is installed.
#'For installation instructions see: 
#'https://pcraster.geo.uu.nl/pcraster/4.4.0/documentation/pcraster_project/install.html
#' e.g. Windows = "C:/MyPrograms/Miniconda3", Linux = "~/miniconda3"
pcr <- "~/ProgramFiles/miniconda3/envs/lisem/bin/"

# 3. IMPORTANT!!! File with event dates for the full input database!
# give the file that contains the date, and start and end timestamp for
# the events that will be analysed. For this study this is:
file_events <- "sources/selected_events.csv"

# adjust this to subcatchments. 1 or multiple? how to approach

#'4. to prepare the input database, many data files are needed, to keep the folder
#' structure tidy, we store all these input file in a specific folder outside 
#' the generated lisem databases, give the location here:
base_dir <- "LISEM_data/setup/"

#' 5. give the name and path of the parameters file
params_file <- "LISEM_data/params/sim_runoff_params.csv"

#' 6. give the name and path of the folder were all QRN runs will be created.
#' This should be inside the project directory. Also make sure there is 
#' enough hard disk space available
sim_dir <- "LISEM_runs/sim_runoff/"

#' 7. give the name of the runfile template used. This can vary with the purpose
#' of the simulation - e.g. runoff, sediment or pesticide simulations. But also
#' the number of used cores and other general settings must be set in this
#' template runfile - which is used as basis for all simulations
#' the template should be stored in: './LISEM_data/setup/'
run_template <- "runfile_template_exploration.run"

# Initialization ---------------------------------------------------------------
# load packages
library(foreach)
library(doMC)
library(lubridate)
library(hms)
library(sf)
library(tidyverse)
library(conflicted)

#general settings
#1.
options(readr.show_col_types = show_data_info)
#2.
source("sources/r_scripts/pcrasteR.R")
set_pcraster(full_path = pcr)
#3.
ev_dates <- read_csv(file_events) 

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#' parallel processing settings
#' set number of core to 2 less than available cores on system
registerDoMC(detectCores()-2)
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


# load functions within this study
# these source files contain much of the main calculations and choices, for a
# better understanding check these functions in detail!
source("sources/r_scripts/aux_functions.R") # all kind of general functions
source("sources/r_scripts/lisem_auto_functions.R")

# other settings
# make global choices for conflicting functions
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
#set digits to 10 for detail in coordinates
options(digits = 10)

# total QRN sample 
all_params <- read_csv(params_file)

# prepare X runs, x is the length of the all_params file
x <- nrow(all_params)

# calibrate for event and subcatch?
ev_dates <- ev_dates %>%
  mutate(ts_start = ymd_hms(event_start),
         ts_end = ymd_hms(event_end)) %>%
  filter(use == "cal")
# find event param from all_params and create event vector
ev_num <- all_params$event
event_sel <- ev_dates$date[ev_num]

# vector with run folder names for all batches
main_dirs <- paste0(sim_dir, "run", (1:x), "/")

# copy files to lisem_runs
proj_wd <- getwd()
# foreach = parallel, for = normal mode
foreach(j=1:length(main_dirs)) %dopar% {
#for (j in seq_along(main_dirs)) {
  fcopy <- dir(paste0("lisem_runs/", year(event_sel[j]),  "/", 
                      str_remove_all(as.character(event_sel[j]), "-")), full.names = T)
  # remove possible result folder
  fcopy <- fcopy[!grepl(".*res$", fcopy)]
  # check if lisem directories exist, otherwise make
  dir.create(main_dirs[j])
  file.copy(fcopy, main_dirs[j], overwrite = TRUE, recursive = TRUE)
}
# this function makes the input raster maps and runfile
  params <- all_params
  rainf <- params$rain
  # check if pesticides should be included based on used template (option 7)
  incl <- if(str_detect(run_template, "pesticide")) {
    TRUE  } else {FALSE}
  # this function is also parallel, change the foreach to for loop to run normal
  sed_water_cal_input_lisem_QRN(run_tmpl = run_template,
                                include_pesticide = incl)
  
# finished