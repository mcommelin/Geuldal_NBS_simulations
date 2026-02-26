# code to prepare lisem input for 5 and 20 m based on the prcessed input maps
# this code takes the following steps:
# 1. run the pcraster script to make the input database

# Initialization --------------------------------------------------------------
#1. Fill runfile template ------------------------------------------------------

make_runfile_lisem <- function(work_dir = NULL,
                               rain_dir = "LISEM_runs/rain",
                               infil_dir = NULL,
                               inp_file = NULL,
                               evdate = NULL,
                               start_time = NULL,
                               end_time = NULL,
                               resolution = 5,
                               do_ndvi_run = TRUE,
                               run_type = "",
                               theta_cal = NULL
                               ) 
{
  
  # select run type
  if (run_type == "cal") {
    do_ndvi = TRUE
  } else if (run_type == "base") {
    do_ndvi = FALSE
  } else {
    print("ERROR: wrong run_type. Choose from: cal OR base")
    return()
  }
  
  # Adjust runfile lisem 
  run_template <- readLines("sources/setup/runfile_template.run")
  
  #load template
  run_temp <- run_template
  proj_wd <- getwd()
  
  # adjust paths based on system
  # home directory
  run_temp <- str_replace_all(run_temp, "^Map Directory=<<map_dir>>", 
                              paste0("Map Directory=", proj_wd, "/", work_dir, "maps"))
  # result directory
  run_temp <- str_replace_all(run_temp, "^Result Directory=<<res_dir>>", 
                              paste0("Result Directory=", proj_wd, "/", work_dir, "res/"))
  # rain files
  if (run_type == "cal") {
  rain_file <- paste0("rain_5min_",str_remove_all(as.character(evdate), "-"), ".txt")
  } else {
    rain_file <- paste0("rain_",str_remove_all(as.character(evdate), "-"), ".txt")
    # set ID map to 1 zone
    run_temp <- str_replace_all(run_temp, "ID=ID.map",
                                paste0("ID=one.map"))
  }
  run_temp <- str_replace_all(run_temp, "<<rain_dir>>",
                              paste0(proj_wd, "/", rain_dir))
  run_temp <- str_replace_all(run_temp, "<<rain_file>>",
                              rain_file)
  
  # infiltration files
  run_temp <- str_replace(run_temp, "<<swatre_inp>>",
                          paste0(proj_wd, "/", inp_file))
  
  run_temp <- str_replace_all(run_temp, "<<swatre_dir>>", 
                              paste0(proj_wd, "/", infil_dir))
  
  # initial head
  if (run_type == "cal") {
  # set correct inithead for event
  runname <- str_remove_all(as.character(evdate), "-")
  ih_ev <- str_remove(runname, "^\\d\\d")
  
  run_temp <- str_replace_all(run_temp, "<<ih>>", 
                              paste0("i", ih_ev, "head"))
  } else {
    # run with standard rain
    
    # for now we use a homogeneous inithead in the base runs.
    # TODO update to corrected inithead profiles
    runname <- evdate
    run_temp <- str_replace_all(run_temp, "<<ih>>", 
                                paste0("ih"))
    #set homogeneous init head
    inihead <- -100
    run_temp <- str_replace_all(run_temp, "Use one matrix potential=0", 
                                paste0("Use one matrix potential=1"))
    run_temp <- str_replace_all(run_temp, "Initial matrix potential=-100", 
                                paste0("Initial matrix potential=", inihead))
    
  }
  
  # flow solution
  if (resolution > 10) {
    run_temp <- str_replace_all(run_temp, "Flood solution=0", "Flood solution=1") # MUSCL on at 20 m
  }
  
  # set timestep
  if (resolution < 20)
    dt = 5 # makkelijker voor grafieken en berekeningen
  else    
    dt = 10 # makkelijker voor grafieken en berekeningen
  
  ts <- str_pad(as.character(dt), width = 3,
                side = "left", pad = "0")
  run_temp <- str_replace_all(run_temp, "<<dt>>", paste0(ts, ".0")) # Timestep model
  
  # set start time
  run_temp <- str_replace_all(run_temp, "<<start_time>>", paste0(start_time)) # 
  
  # set end time
  run_temp <- str_replace_all(run_temp, "<<end_time>>", paste0(end_time)) #  
  
  if (run_type == "cal") {
  # set baseflowmap
  run_temp <- str_replace(run_temp, "<<baseflow_map>>",
                          paste0("baseflow_", runname, ".map"))

  datestr <- substr(runname, 3, 8)
  # set ndvi related maps
  if (do_ndvi_run == TRUE) {
    run_temp <- str_replace_all(run_temp, "cover=per.map",
                                paste0("cover=per", datestr, ".map"))
    run_temp <- str_replace_all(run_temp, "lai=lai.map",
                                paste0("lai=lai", datestr, ".map"))
    run_temp <- str_replace_all(run_temp, "smax=smax.map",
                                paste0("smax=smax", datestr, ".map"))
    run_temp <- str_replace_all(run_temp, "manning=n.map",
                                paste0("manning=n", datestr, ".map"))
  }

  
  } else {
    # no baseflow
    # set dummy value
    run_temp <- str_replace(run_temp, "<<baseflow_map>>",
                            paste0("nobaseflow.map"))
    # set baseflow method
    run_temp <- str_replace(run_temp, "Channel baseflow method=2",
                            paste0("Channel baseflow method=0"))
  }
 
  # set theta calibration
  if (!is.null(theta_cal)) {
    run_temp <- str_replace(run_temp, "Theta calibration=1.00",
                            paste0("Theta calibration=", theta_cal))
  }
  
  # save the runfile
   writeLines(run_temp, paste0(work_dir, "runfiles/", runname, ".run"))
   
} # end function make_runfile_lisem()

#2. Make LISEM run ----------------------------------------------------


# function create_lisem_run
create_lisem_run <- function(
  resolution = NULL,
  catch_num = NULL,
  swatre_file = "base_swatre_params.csv",
  run_type = "",
  do_runfile = TRUE,
  NBS_num = 0) 
{

  # set some triggers
  # select run type
  if (run_type == "cal") {
    do_ndvi = TRUE
  } else if (run_type == "base") {
    do_ndvi = FALSE
  } else {
    print("ERROR: wrong run_type. Choose from: cal OR base")
    return()
  }
  
  # check if it is a base run, or simulation a NBS
  if (NBS_num != 0) {
    do_NBS = TRUE
  } else {
    do_NBS = FALSE
  }
  
  
  ### prepare and/or copy all maps and table in the run dir/maps
  points <- read_csv("sources/setup/outpoints_description.csv")
  
  catch_info <- points %>%
    filter(point == catch_num) %>%
    filter(cell_size == resolution)
  
  # copy basemaps to a lisem_runs folder
  catch_dir <- paste0(catch_info$subcatch_name, "_", catch_info$cell_size, "m/")
  base_dir <- paste0("LISEM_data/", catch_dir)
  
  # if catch_num > 1 add subcatchments after LISEM_data/
  if (catch_num > 1) {
    base_dir <- paste0("LISEM_data/subcatchments/", catch_dir)
  }
  
  #adjust folder name when simulating NBS
  if (NBS_num != 0) {
    NBS_desc <- read_csv("sources/setup/tables/lu_NBS_tbl.csv") %>%
      filter(lu_nr == NBS_num)
    NBS_name <- NBS_desc$description
    catch_dir <- paste0(catch_info$subcatch_name, "_", catch_info$cell_size, 
                        "m_", NBS_name, "/")
  } 
    
  run_dir <- paste0("LISEM_runs/", catch_dir)

  # create subdir for the run
  if (!dir.exists(run_dir)) {
    dir.create(run_dir, recursive = TRUE)
  }
  
  # create the following folders in the run_dir: maps, rain, res, runfiles
  dirs <- c("maps", "swatre", "res", "runfiles")
  for (dir in dirs) {
    dir_path <- paste0(run_dir, dir)
    if (!dir.exists(dir_path)) {
      dir.create(dir_path)
    }
  }

  base_maps <- readLines("sources/base_maps.txt")
  
  # Add NBS maps if simulating NBS
  if (NBS_num != 0) {
    nbs_map <- dir(paste0(base_dir, "maps/"), paste0("^", NBS_num, ".*"))
    base_maps <- c(base_maps, nbs_map)
  }
    
  # copy the maps to the run_dir
  subdir <- paste0(run_dir, "maps/")
  for (map in base_maps) {
    file.copy(paste0(base_dir, "maps/", map), paste0(subdir, map), 
              overwrite = TRUE)
  }
 
   # copy all inithead files
  # TODO adjust for cal or base run
  if (run_type == "cal") {
  ih_maps <- dir(paste0(base_dir, "maps/"), pattern = "i2")
  for (map in ih_maps) {
    file.copy(paste0(base_dir, "maps/", map), paste0(subdir, map), 
              overwrite = TRUE)
  }
  }
  
  #copy landuse and channel table to subdir
  # make a difference based on NBS simulations
  if (NBS_num != 0) {
    file.copy(from = "sources/setup/calibration/lu_nbs.tbl", to = paste0(subdir, "lu.tbl"), overwrite = T)
  } else {
    file.copy(from = "sources/setup/calibration/lu.tbl", to = subdir, overwrite = T)
  }

  file.copy(from = "sources/setup/tables/chan.tbl", to = subdir, overwrite = T)
  
  # create landuse calibration table: used in prepare_db.map AND prepare_ndvi.mod
  cal_lu <- read_csv("sources/setup/calibration/calibration_landuse.csv") %>%
    select(-cal_comment)
  nms <- as.character(seq(0, ncol(cal_lu) - 1))
  names(cal_lu) <- nms
  write.table(cal_lu, file = "sources/setup/calibration/cal_lu.tbl",
              sep = " ", row.names = FALSE,
              quote = FALSE)
  file.copy(from = "sources/setup/calibration/cal_lu.tbl", to = subdir, overwrite = T)
  
  # optional: copy NDVI maps to the dir 
  if (do_ndvi == TRUE) {
    ndvi_maps<- dir(paste0(base_dir, "maps/"), pattern = "ndvi")
    for (map in ndvi_maps) {
      file.copy(paste0(base_dir, "maps/", map), paste0(subdir, map), 
                overwrite = TRUE)
    }
  }
  
  ### start running scripts
  # update the landuse map, to include the NBS
  if (NBS_num != 0) {
    # rename the map
    file.rename(paste0(subdir, nbs_map), paste0(subdir, "nbs.map"))
    file.copy(paste0(subdir, "landuse.map"), paste0(subdir, "landuse_base.map"))
    pcr_script(
      script = paste0("prepare_nbs.mod ", NBS_num),
      script_dir = "sources/pcr_scripts",
      work_dir = subdir
    )
    file.rename(paste0(subdir, "nbs.map"), paste0(subdir, nbs_map))
  }
  # run pcraster script to finalize run database.
  pcr_script(
    script = "prepare_db.mod",
    script_dir = "sources/pcr_scripts",
    work_dir = subdir
  )
  
  # optional run NDVI related script for event based ndvi, per, lai, n
  if (do_ndvi == TRUE) {
    cal_events <- read_csv("sources/selected_events.csv") %>%
      filter(use == "cal")
    events <- str_extract(cal_events$event_start, "\\d*")
    ndvi_ev <- str_remove(events, "^\\d\\d")
    for (j in seq_along(events)) {
      pcr_script(
        script = paste0("prepare_ndvi.mod ",ndvi_ev[j]),
        script_dir = "sources/pcr_scripts",
        work_dir = subdir
      )
    }
  }
  
  # run pcraster script to make storm drains.
  pcr_script(
    script = "storm_drains.mod",
    script_dir = "sources/pcr_scripts",
    work_dir = subdir
  )
    
  # run pcraster script to make buffer features.
  pcr_script(
    script = "prepare_buffer_features.mod",
    script_dir = "sources/pcr_scripts",
    work_dir = subdir
  )
  
  if (run_type == "cal") {
  # add runfiles for selected events
  events <- read_csv("sources/selected_events.csv", show_col_types = FALSE) %>%
    filter(use == "cal") %>%
    mutate(ts_start = ymd_hms(event_start),
           ts_end = ymd_hms(event_end),
           str_start = paste0(str_pad(as.character(yday(ts_start)), width = 3,
                               side = "left", pad = "0"), ":",
                              str_pad(as.character(hour(ts_start) * 60 + minute(ts_start)), width = 4,
                                      side = "left", pad = "0")),
           str_end = paste0(str_pad(as.character(yday(ts_end)), width = 3,
                                    side = "left", pad = "0"), ":",
                            str_pad(as.character(hour(ts_end) * 60 + minute(ts_end)), width = 4,
                                    side = "left", pad = "0")))
  # load theta_cal file
  cn = catch_num
  theta_factors <- read_csv("sources/setup/calibration/calibration_theta.csv") %>%
    filter(catch_num == cn)
  
  for (i in seq_along(events$event_start)) {
    #make baseflow
    date_event <- str_remove_all(as.character(date(events$ts_start[i])), "-")
    baseqtbl <- read_csv("sources/base_flow_cal_events.csv",show_col_types = FALSE) %>%
      filter(date == str_remove_all(as.character(date(events$ts_start[i])), "-")) %>%
      select(-date)
    nms <- as.character(seq(0, ncol(baseqtbl) - 1))
    names(baseqtbl) <- nms
    #write space delimited tbl with colnumbers instead of names
    write.table(baseqtbl, file = paste0(subdir, "baseq.tbl"),
                sep = " ", row.names = FALSE,
                quote = FALSE)
    # run pcraster script to make baseflow map.
    pcr_script(
      script = "baseflow_calibration.mod",
      script_dir = "sources/pcr_scripts",
      work_dir = subdir
    )

    file.rename(paste0(subdir, "baseflow.map"),
                paste0(subdir, "baseflow_", date_event, ".map"))
    
    # get theta_cal
    if (nrow(theta_factors) == 0) {
      theta_cal <-  1.00
    } else {
    theta_cal <- theta_factors %>%
      filter(date == date_event)
    
    theta_cal <- theta_cal$theta_cal}
  
        # make runfile  
    if (do_runfile == TRUE) {
      
      message("Making run file")
      make_runfile_lisem(
        work_dir = run_dir,
        infil_dir = paste0(run_dir, "swatre/tables/"),  
        inp_file = paste0(run_dir, "swatre/profile.inp"),
        evdate = date(events$ts_start[i]),
        start_time = events$str_start[i],
        end_time = events$str_end[i],
        resolution = resolution,
        do_ndvi_run = do_ndvi,
        run_type = run_type,
        theta_cal = theta_cal
      )
    }
  } # end date specific loop
  } # end run_type = "cal"
  
  if (run_type == "base") {
    if (do_runfile == TRUE) {
    # loop over standard events in stead of dates
    standard_ev <- c("T50", "T100", "T500", "T500_uur")
    
    
     # make runfile  
    message("Making run file")
    
    for (i in seq_along(standard_ev)) {
      make_runfile_lisem(
        work_dir = run_dir,
        infil_dir = paste0(run_dir, "swatre/tables/"),  
        inp_file = paste0(run_dir, "swatre/profile.inp"),
        evdate = standard_ev[i],
        start_time = "001:0000", #fixed for all stadard events
        end_time = "001:1440", #fixed for all stadard events
        resolution = resolution,
        do_ndvi_run = do_ndvi,
        run_type = run_type
      )
    }
    }
    
    
    
    
  }
  #delete intermediate files

  source("sources/r_scripts/swatre_input.R")
  make_swatre_tables(cal_file = swatre_file,
                     swatre_dir = paste0(run_dir, "swatre/"),
                     do_NBS = do_NBS)
  
  message("finished run data creation.")
  
} # end create_lisem_run




