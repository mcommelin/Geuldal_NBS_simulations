# function to convert a source raster into pcraster format

# Initialisation --------------------------------------------------------------
# done in the main workflow code

# Source 2 base ---------------------------------------------------------------

# map_in = "data/processed_data/lulc_osm_10m_region.tif"
# map_out = "landuse"
# resample_method = "near"


source_to_base_maps <- function(
    map_in = "",
    map_out = "",
    resample_method = "near") {
  
  resolution <- c(5, 20)
  srs = "EPSG:28992"
  for (i in seq_along(resolution)) {
    
    # set the main directory
    main_dir <- paste0("LISEM_data/Geul_", resolution[i], "m/")
    
    # get the extent of the mask.map
    map2asc(
      map_in = paste0(main_dir, "maps/mask.map"),
      map_out = paste0(main_dir, "mask.asc")
    )
    
    # calculate extent from ascii header
    # read the header
    header <- readLines(paste0(main_dir, "mask.asc"), n = 5)
    # extract only digits or '.' from the strings in header
    header <- gsub("[^0-9.]", "", header)
    
    # convert to numeric
    header <- as.numeric(header, digits = 10)
    
    # get the extent
    extent <- c(
      header[3], # xllcorner
      header[4], # yllcorner
      header[3] + header[1] * header[5], # xurcorner
      header[4] + header[2] * header[5]  # yurcorner
    )
    
    #remove the mask.asc
    file.remove(paste0(main_dir, "mask.asc"))
    
    tempfile <- paste0(main_dir, "maps/", map_out, ".asc")
    message("gdalwarp: ",map_in)
    # GDAL warp the source map
    gdalwarp(
      srcfile = map_in,
      dstfile = tempfile,
      s_srs = srs,
      t_srs = srs,
      te_srs = srs,
      tr = rep(resolution[i], 2),
      of = "AAIgrid",
      te = extent,
      r = resample_method,
      dryrun = F,
      overwrite = T
    )
    
    # convert the ascii to pcraster
    asc2map(clone = paste0(main_dir, "maps/mask.map"),
            map_in = tempfile,
            map_out = paste0(main_dir, "maps/", map_out, ".map"),
            options = "-S")
    
    # clean up
    # remove the ascii file
    file.remove(tempfile)
    file.remove(paste0(main_dir, "maps/", map_out, ".prj"))
    # remove all aux.xml files
    subdir <- paste0(main_dir, "maps/")
    aux_files <- list.files(subdir, pattern = "aux.xml", full.names = TRUE)
    if (length(aux_files) > 0) {
      file.remove(aux_files)
    }
    
  } # end of resolution loop

} # end of function
