# code to prepare lisem input for 5 and 20 m based on the prcessed input maps
# this code takes the following steps:
# 1. run the pcraster script to make the input database

# Initialization --------------------------------------------------------------
#1. Fill runfile template ------------------------------------------------------

make_runfile_lisem <- function(work_dir = NULL,
                               rain_dir = NULL,
                               infil_dir = NULL,
                               inp_file = NULL,
                               evdate = NULL,
                               start_time = NULL,
                               end_time = NULL,
                               resolution = 5,
                               do_ndvi_run = TRUE
                               ) 
{
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
  rain_file <- paste0("rain_5min_",str_remove_all(as.character(evdate), "-"), ".txt")
  run_temp <- str_replace_all(run_temp, "<<rain_dir>>",
                              paste0(proj_wd, "/", rain_dir))
  run_temp <- str_replace_all(run_temp, "<<rain_file>>",
                              rain_file)
  
  # infiltration files
  run_temp <- str_replace(run_temp, "<<swatre_inp>>",
                          paste0(proj_wd, "/", inp_file))
  
  run_temp <- str_replace_all(run_temp, "<<swatre_dir>>", 
                              paste0(proj_wd, "/", infil_dir))
  
  # set correct inithead for event
  runname <- str_remove_all(as.character(evdate), "-")
  ih_ev <- str_remove(runname, "^\\d\\d")
  
  run_temp <- str_replace_all(run_temp, "<<ih>>", 
                              paste0("ih", ih_ev))
  # flow solution
  if (resolution > 10) {
    run_temp <- str_replace_all(run_temp, "Flood solution=0", "Flood solution=1") # MUSCL on at 20 m
  }
  
  # set timestep
  #dt <- ceiling(resolution * 0.75)
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
  
  writeLines(run_temp, paste0(work_dir, "runfiles/", runname, ".run"))
  
  # runfile with buffers
  run_temp <- str_replace_all(run_temp, "Include Mitigation/Conservation=0",
                              "Include Mitigation/Conservation=1")
  run_temp <- str_replace_all(run_temp, "Include buffers=0",
                              "Include buffers=1")
  
  # replace channel buffer maps
  run_temp <- str_replace_all(run_temp, "chanwidth=chanwidth.map",
                              "chanwidth=chanwidthbuf.map")
  run_temp <- str_replace_all(run_temp, "chanwidth=chanwidth.map",
                              "chanwidth=chanwidthbuf.map")
  run_temp <- str_replace_all(run_temp, "chandepth=chandepth.map",
                              "chandepth=chandepthbuf.map")
  run_temp <- str_replace_all(run_temp, "chanside=zero.map",
                              "chanside=chansidebuf.map")
  
  writeLines(run_temp, paste0(work_dir, "runfiles/", runname, "buf.run"))
  

} # end function make_runfile_lisem()

# # temp code
# # check if lisem result directory exists, otherwise make
# if (!dir.exists(paste0(main_dirs[i], "res"))) {
#   dir.create(paste0(main_dirs[i], "res"))
# }
# # if it exists, remove and make new
# if (dir.exists(paste0(main_dirs[i], "res"))) {
#   unlink(paste0(main_dirs[i], "res"), recursive = TRUE)
#   dir.create(paste0(main_dirs[i], "res"))
# }



#2. Run pcraster db script----------------------------------------------------
#points <- read_csv("LISEM_data/setup/outpoints_descriptionN.csv")

# settings
# <- 5 # fill resolution here
#catch_num <- 18 # fill catchment number here (see points table)

# function create_lisem_run
create_lisem_run <- function(
  resolution = NULL,
  catch_num = NULL,
  swatre_file = "base_swatre_params.csv",
  do_ndvi = TRUE,
  do_runfile = TRUE,
  NBS_num = 0) 
{

  ### prepare and/or copy all maps and table in the run dir/maps
  
  points <- read_csv("sources/setup/outpoints_description.csv")
  
  catch_info <- points %>%
    filter(point == catch_num) %>%
    filter(cell_size == resolution)
  
  # copy basemaps to a lisem_runs folder
  catch_dir <- paste0(catch_info$subcatch_name, "_", catch_info$cell_size, "m/")
  base_dir <- paste0("LISEM_data/", catch_dir)
  
  # if catch_num > 1 add subcatchements after LISEM_data/
  if (catch_num > 1) {
    base_dir <- paste0("LISEM_data/subcatchments/", catch_dir)
  }
  
  
  #adjust folder name when simulation NBS
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
 
  #TODO if cal use date, otherwise base situation
  
   # copy all inithead files
  ih_maps <- dir(paste0(base_dir, "maps/"), pattern = "ih2")
  for (map in ih_maps) {
    file.copy(paste0(base_dir, "maps/", map), paste0(subdir, map), 
              overwrite = TRUE)
  }
  
  #TODO if cal or base nbs
  #copy landuse and channel table to subdir
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
    message(ndvi_maps)
    for (map in ndvi_maps) {
      file.copy(paste0(base_dir, "maps/", map), paste0(subdir, map), 
                overwrite = TRUE)
    }
  }
  
  ### start running scripts
  
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
    
    # make runfile  
    if (do_runfile == TRUE) {
      message("Making run file")
      make_runfile_lisem(
        work_dir = run_dir,
        rain_dir = "LISEM_runs/rain/",   #<= more logical, just one level higher then the runs as common dataset
        infil_dir = paste0(run_dir, "swatre/tables/"),  
        inp_file = paste0(run_dir, "swatre/profile.inp"),
        evdate = date(events$ts_start[i]),
        start_time = events$str_start[i],
        end_time = events$str_end[i],
        resolution = resolution,
        do_ndvi_run = do_ndvi
      )
    }
  } # end date specific loop
    
  #delete intermediate files
  # leave the tables for later reference
  #file.remove(paste0(subdir, "chan.tbl"))
  #file.remove(paste0(subdir, "lu.tbl"))
  #file.remove(paste0(subdir, "soil.tbl"))
  
  source("sources/r_scripts/swatre_input.R")
  make_swatre_tables(cal_file = swatre_file,
                     swatre_dir = paste0(run_dir, "swatre/"))
  
} # end create_lisem_run




