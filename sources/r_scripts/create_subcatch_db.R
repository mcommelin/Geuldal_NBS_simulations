# make lisem input for sub catchment

# Initialization --------------------------------------------------------------
# Function to create subcatchment base maps -----------------------------------
base_maps_subcatchment <- function(
    cell_size = NULL,
    sub_catch_number = NULL, # adjust the number to select the subcatchment you want
    calc_ldd = FALSE,
    parallel = TRUE
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
  message(ref)
  
  # #remove ldd map because cannot be resampled.
  base_maps <- gsub("^ldd\\.map$", "", base_maps)
  # # remove catchment because it is already correct size
  base_maps <- gsub("^catchment\\.map$", "", base_maps)
  base_maps <- base_maps[base_maps != ""]  # Remove empty lines  
  
  for (i in seq_along(base_maps)) {
    map_in = paste0(sub_catch_dir,"base_", base_maps[i])
    map_out_name = paste0(sub_catch_dir, base_maps[i])
    tmp_tif = paste0(sub_catch_dir, "tmp.tif")
  if (DEBUGm) message("in ",map_in)
  if (DEBUGm) message("out ",map_out_name)
    
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
  
  file.remove(
   list.files(
     path = sub_catch_dir,
     pattern = "sub|base_|xml|tif",
     full.names = TRUE
   )
  )
}  
  
  
  
  
  
  
  
  
  
  
  
# 
# # map2asc
# map2asc(
#   map_in = "sub.map",
#   map_out = "sub.asc",
#   sub_dir = sub_catch_dir
# )
# 
# # vectorize subcatchment
# ras <- rast(paste0(sub_catch_dir, "sub.asc"))
# pol <- as.polygons(ras)
# writeVector(pol, paste0(sub_catch_dir, "sub.shp"), overwrite = TRUE)
# 
# # use polygon of subcatchment to cut the raster
# 
# # gdal warp with cutline
# gdalwarp(
#   srcfile = paste0(sub_catch_dir, "sub.asc"),
#   dstfile = paste0(sub_catch_dir, "catchment.asc"),
#   s_srs = srs,
#   t_srs = srs,
#   tr = rep(res, 2),
#   cutline = paste0(sub_catch_dir, "sub.shp"),
#   crop_to_cutline = T,
#   of = "AAIrid",
#   r = method,
#   dryrun = F,
#   overwrite = T
# )


# #remove ldd map because cannot be resampled.
# base_maps <- gsub("^ldd\\.map$", "", base_maps)
# # remove catchment because it is already correct size
# base_maps <- gsub("^catchment\\.map$", "", base_maps)
# base_maps <- base_maps[base_maps != ""]  # Remove empty lines
# 
# # resample the base maps to the new mask.map
# 
# 
#   for (i in seq_along(base_maps)) {
#     resample(
#       clone = "mask.map",
#       map_in = paste0("base_", base_maps[i]),
#       map_out = base_maps[i],
#       dir = sub_catch_dir
#     )
#   }
# }

# # run pcraster script to create base maps for subcatch
# if (calc_ldd == TRUE) {
#   pcr_script(
#     script = "ldd_subcatch.mod",
#     script_dir = "sources/pcr_scripts",
#     work_dir = sub_catch_dir
#   )
# }

# pcr_script(
#   script = "base_maps_subcatch.mod",
#   script_dir = "sources/pcr_scripts",
#   work_dir = sub_catch_dir
# )

# 
# # remove all intermediate maps and files
# file.remove(
#   list.files(
#     path = sub_catch_dir,
#     pattern = "base_|sub|catchment\\.asc|\\.prj",
#     full.names = TRUE
#   )
# )



