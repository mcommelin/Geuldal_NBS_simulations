# function to convert a source raster into pcraster format

# Initialisation --------------------------------------------------------------
# done in the main workflow code

# Source 2 base ---------------------------------------------------------------

source_to_base_maps <- function(
    map_in = "",
    map_out = "",
    resample_method = "max"
    ) 
{
  
  resolution <- c(5, 20)
  for (i in seq_along(resolution)) {
    
    # set the main directory
    main_dir <- paste0("LISEM_data/Geul_", resolution[i], "m/")
    map_clone = paste0(main_dir, "maps/mask.map")
    map_out_name = paste0(main_dir, "maps/", map_out, ".map")
    tmp_tif = paste0(main_dir, "maps/tmp.tif")
    srs = "EPSG:28992"

    if (DEBUGm) message("creating => ",map_out_name)
    
    # Extract extent, resolution, etc. from the reference raster
    ref <- raster(map_clone)
    xmin <- xmin(ref)
    ymin <- ymin(ref)
    xmax <- xmax(ref)
    ymax <- ymax(ref)
    ncol <- ncol(ref)
    nrow <- nrow(ref)
    
    # gdalwarp makes a tif, PCRaster cannot be done directly because of valuescale
    gdalwarp(
      srcfile = map_in,
      dstfile = tmp_tif,
      t_srs   = srs,         
      te      = c(xmin, ymin, xmax, ymax),
      ts      = c(ncol, nrow),
      r       = resample_method,    
      overwrite = TRUE
    )
    
    # use gdaltranslate to create PCRaster map  
    if (file.exists(map_out_name)) file.remove(map_out_name)
    gdal_translate(
      src_dataset = tmp_tif,
      dst_dataset = map_out_name,
      ot = "Float32",
      of = "PCRaster",
      mo = "PCRASTER_VALUESCALE=VS_SCALAR"
    )

  } # end of resolution loop

} # end of function


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# function to resample 5 m and 20 meter maps for the base dataset
# base sources:
# 1. all maps are in 5 meter resolution
# 2. initial head maps are in 20m resolution.

resample_base_maps <- function(resolution = 5) {
  # base_maps
  main_dir <- paste0("LISEM_data/Geul_", resolution, "m/")
  base_dir <- paste0("LISEM_data/Base_Geul_5m/maps/")
  base_maps <- dir(base_dir, recursive = TRUE)

  # check if main directory exist, otherwise make it
  if (!dir.exists(main_dir)) {
    dir.create(main_dir, recursive = TRUE)
    dir.create(paste0(main_dir, "maps/"), recursive = TRUE)
  }
  
  for (i in seq_along(base_maps)) {
    # if resolution = 5, copy from base to Geul
    if (resolution == 5) {
      file = paste0(main_dir, "maps/", base_maps[i])
    # if different, copy and resample
    } else {
      file = paste0(main_dir, "maps/base_", base_maps[i])
    }
    file.copy(
      from = paste0(base_dir, base_maps[i]),
      to = file,
      overwrite = TRUE
    )
  }

  # warning! need an existing mask.map! now only available for 20 and 5 m
  # should be placed in /LISEM_data/Base_Geul_xxm/maps/mask.map
  if (resolution != 5) {

  map_clone = paste0("LISEM_data/Base_Geul_", resolution, "m/maps/mask.map")
  srs = "EPSG:28992"
  tmp_tif = paste0(main_dir, "maps/tmp.tif")
  if (DEBUGm) message("resampling => ", resolution, "m maps")
  
  # Extract extent, resolution, etc. from the reference raster
  ref <- raster(map_clone)
  xmin <- xmin(ref)
  ymin <- ymin(ref)
  xmax <- xmax(ref)
  ymax <- ymax(ref)
  ncol <- ncol(ref)
  nrow <- nrow(ref)
  
  map_in <- dir(paste0(main_dir, "maps"), full.names = T)
  for (i in seq_along(map_in)) {
  # gdalwarp makes a tif, PCRaster cannot be done directly because of valuescale
  gdalwarp(
    srcfile = map_in[i],
    dstfile = tmp_tif,
    t_srs   = srs,         
    te      = c(xmin, ymin, xmax, ymax),
    ts      = c(ncol, nrow),
    r       = "near",
    overwrite = TRUE
  )
  
  map_out_name <- paste0(main_dir, "maps/", base_maps[i])
  # use gdaltranslate to create PCRaster map  
  if (file.exists(map_out_name)) file.remove(map_out_name)
  gdal_translate(
    src_dataset = tmp_tif,
    dst_dataset = map_out_name,
    ot = "Float32",
    of = "PCRaster",
    mo = "PCRASTER_VALUESCALE=VS_SCALAR"
  )
    
  }
  
  file.remove(
    list.files(
      path = paste0(main_dir, "maps/"),
      pattern = "base_|xml|tif",
      full.names = TRUE
    )
  )
  # place the correct mask.
  file.remove(paste0(main_dir, "maps/mask.map"))
  file.copy(from = map_clone,
            to = paste0(main_dir, "maps/"))
  
   }
}
