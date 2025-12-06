# make lisem input for sub catchment

# Initialization --------------------------------------------------------------
# Function to create subcatchment base maps -----------------------------------
base_maps_subcatchment <- function(
    cell_size = NULL,
    sub_catch_number = NULL, # adjust the number to select the subcatchment you want
    calc_ldd = FALSE
    )
{
  
  # general settings
  res = cell_size
  srs = "EPSG:28992"
  resample_method = "near"
  
  # load subcatchment points csv file
  points <- read_csv("sources/setup/outpoints_description.csv")
  
  # select subcatchment
  subcatch <- points %>%
    filter(point == sub_catch_number) %>% 
    filter(cell_size == res)
  subcatch_name <- subcatch$subcatch_name # give best describing name for the subcatchment

  # create dir for subcatch
  sub_catch_dir <- paste0("LISEM_data/subcatchments/", subcatch_name, "_", res, "m/maps/")
  main_dir <- paste0("LISEM_data/Geul_", res, "m/maps/")

  if (!dir.exists(sub_catch_dir)) {
    dir.create(sub_catch_dir, recursive = TRUE)
  }
  
  # copy base maps from main_dir to new subcatch dir
  base_maps <- readLines("sources/base_maps.txt")
  base_maps <- base_maps[!grepl("^\\s*#", base_maps) & nzchar(trimws(base_maps))]

  # add "base" suffix to the base maps names in the subcatch dir
  for (i in seq_along(base_maps)) {
    file.copy(
      from = paste0(main_dir, base_maps[i]),
      to = paste0(sub_catch_dir, "base_", base_maps[i]),
      overwrite = TRUE
    )
  }
  
  if (DEBUGm) message(sub_catch_dir)
  
  # delineate the subcatchment
  
  
  # pcraster create map with selected outpoint
  pcrcalc(
    work_dir = sub_catch_dir,
    options = paste0("sub_point.map=boolean(if(base_outpoints.map eq ", subcatch$point, ", 1))")
  )
  # delineate the subcatchment, results in sub.map with catchment(ldd, point) 
  pcr_script(
    script = "delineate_catchment.mod",
    script_dir = "sources/pcr_scripts",
    work_dir = sub_catch_dir
  )

  map_clone = paste0(sub_catch_dir, "sub.map")
  map_clone_tif = paste0(sub_catch_dir, "sub.tif")
  map_clone_cut_tif = paste0(sub_catch_dir, "subc.tif")

  #make sub.map into a tif to read the header
  gdal_translate(
    src_dataset = map_clone,
    dst_dataset = map_clone_tif,
    of = "GTiff"
  )
  
  #use raster library to crop MVs
  r <- rast(map_clone_tif)
  cropped_r <- trim(r)
  writeRaster(cropped_r, map_clone_cut_tif, overwrite = TRUE)
  # use gdaltranslate to create a PCRaster map  
  gdal_translate(
    src_dataset = map_clone_cut_tif,
    dst_dataset = paste0(sub_catch_dir, "catchment.map"),
    ot = "Float32",
    of = "PCRaster",
    mo = "PCRASTER_VALUESCALE=VS_SCALAR"
  )
  
  # Extract extent, resolution, etc. from the reference raster
  ref <- raster(map_clone_cut_tif)
  xmin <- xmin(ref)
  ymin <- ymin(ref)
  xmax <- xmax(ref)
  ymax <- ymax(ref)
  ncol <- ncol(ref)
  nrow <- nrow(ref)
  #if (DEBUGm) message(ref)
  
  # #remove ldd map because cannot be resampled.
  base_maps <- gsub("^ldd\\.map$", "", base_maps)
  # # remove catchment because it is already correct size
  base_maps <- gsub("^catchment\\.map$", "", base_maps)
  base_maps <- base_maps[base_maps != ""]  # Remove empty lines  
  #base_maps[24] <- "sub_point.map"
  base_maps[[length(base_maps) + 1]] <- "sub_point.map"
  
  for (i in seq_along(base_maps)) {
    map_in = paste0(sub_catch_dir,"base_", base_maps[i])
    if (base_maps[i] == "sub_point.map") {
    map_in = paste0(sub_catch_dir, base_maps[i])}
    map_out_name = paste0(sub_catch_dir, base_maps[i])
    tmp_tif = paste0(sub_catch_dir, "tmp.tif")
    if (DEBUGm) message("in ",map_in)
    
    # gdalwarp makes a temp tif
    gdalwarp(
      srcfile = map_in,
      dstfile = tmp_tif,
      t_srs   = srs,         
      te      = c(xmin, ymin, xmax, ymax),
      ts      = c(ncol, nrow),         
      r       = resample_method,    
      overwrite = TRUE
    )
    print(map_out_name)
    # use gdaltranslate to create a PCRaster map  
    gdal_translate(
      src_dataset = tmp_tif,
      dst_dataset = map_out_name,
      ot = "Float32",
      of = "PCRaster",
      mo = "PCRASTER_VALUESCALE=VS_SCALAR"
    )
    if (DEBUGm) message("out ",map_out_name)
    
  }
  # # run pcraster script to create base maps for subcatch
  if (calc_ldd == TRUE) {
    pcr_script(
      script = "ldd_subcatch.mod",
      script_dir = "sources/pcr_scripts",
      work_dir = sub_catch_dir
    )
  }
  
  pcr_script(
    script = "base_maps_subcatch.mod",
    script_dir = "sources/pcr_scripts",
    work_dir = sub_catch_dir
  )
  
  # initial head per subcatch
  # initial head maps
  # if resolution = 20, copy from base to Geul
  # if different, copy and resample
  cal_events <- read_csv("sources/selected_events.csv") %>%
    filter(use == "cal")
  events <- str_extract(cal_events$event_start, "\\d*")
  ih_ev <- str_remove(events, "^\\d\\d")
  ih_dir <- paste0("spatial_data/inithead/inith_", events, "_20m/")
  ih_maps <- dir(ih_dir[1], pattern = "\\d$")
  ih_end <- str_extract(ih_maps, "\\d*$")

  # per event
  for (j in seq_along(events)) {
  for (i in seq_along(ih_maps)) {
    map_in = paste0(ih_dir[j], ih_maps[i])
    map_out_name = paste0(sub_catch_dir, "ih", ih_ev[j], ".", ih_end[i])
    tmp_tif = paste0(sub_catch_dir, "tmp.tif")
    if (DEBUGm) message("IH in ",map_in)
    if (DEBUGm) message("IH out ",map_out_name)
    
    # gdalwarp makes a temp tif
    gdalwarp(
      srcfile = map_in,
      dstfile = tmp_tif,
      t_srs   = srs,         
      te      = c(xmin, ymin, xmax, ymax),
      ts      = c(ncol, nrow),         
      r       = resample_method,    
      overwrite = TRUE
    )
    
    # use gdaltranslate to create a PCRaster map  
    gdal_translate(
      src_dataset = tmp_tif,
      dst_dataset = map_out_name,
      ot = "Float32",
      of = "PCRaster",
      mo = "PCRASTER_VALUESCALE=VS_SCALAR"
    )
    
  } # end init head files loop
  } # end event loop
  
  # 10m NDVI maps, called NDVI.tif in a dir with an event date
  ndvi_dir <- paste0("spatial_data/ndvi/ndvi_", events, "_10m/")
  # per event
  for (j in seq_along(events)) {
      map_in = paste0(ndvi_dir[j], "ndvi.tif")
      map_out_name = paste0(sub_catch_dir, "ndvi", ih_ev[j], ".map")
      tmp_tif = paste0(sub_catch_dir, "tmp.tif")
      if (DEBUGm) message("NDVI in ",map_in)
      if (DEBUGm) message("NDVI out ",map_out_name)
      
      # gdalwarp makes a temp tif
      gdalwarp(
        srcfile = map_in,
        dstfile = tmp_tif,
        t_srs   = srs,         
        te      = c(xmin, ymin, xmax, ymax),
        ts      = c(ncol, nrow),         
        r       = resample_method,    
        overwrite = TRUE
      )
      
      # use gdaltranslate to create a PCRaster map  
      gdal_translate(
        src_dataset = tmp_tif,
        dst_dataset = map_out_name,
        ot = "Float32",
        of = "PCRaster",
        mo = "PCRASTER_VALUESCALE=VS_SCALAR"
      )

  } # end event loop

  # clean up
  file.remove(
    list.files(
      path = sub_catch_dir,
      pattern = "sub|base_|xml|tif",
      full.names = TRUE
    )
  )
  
}  # end function - create subcatch
  
  




