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
  
  resolution <- c(5) #, 20)
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

