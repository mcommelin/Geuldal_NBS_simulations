# functions to convert the base data from ./spatial_data to Geul input

## spatial_data to Geul_xm 
spatial_data_to_pcr <- function(only_NBS = FALSE) {
  
  res <- c(5, 10, 20)
  maps_list <- read_csv("sources/transformations_maps.csv")
  dir_sd <- "spatial_data/"
  
  NBS_maps <- dir("spatial_data/NBS_maps/", pattern = ".tif$")
  NBS_out <- str_replace_all(NBS_maps, "tif$", "map")

  # load outline of catchment to reduce data load
  #catch_poly <- st_read(paste0(dir_sd,"catchment.gpkg"), layer = "catch_buffered_250")
  #prepare resolution specific base data
  res_dir <- paste0("LISEM_data/Geul_", res, "m/maps/")
  mask <- vector("list", length = length(res))
  for (r in seq_along(res)) {
    mask[[r]] <- rast(paste0(dir_sd, "mask_", res[r], "m.map"))
  }
  
  if(only_NBS == FALSE) {
  
  for (i in seq_along(maps_list$name)) { #loop over base maps
  #for (i in 1:17) {
    # two options: input map is vector or raster
    if(maps_list$type[i] == "vector") {
      map <- st_read(paste0(dir_sd, maps_list$file[i], ".gpkg"), 
                     layer = maps_list$name_in[i], quiet = T)
      #clip map to catchment
      #map <- st_intersection(map, catch_poly)
      # make spatvector
      map <- vect(map)
      #rasterize for each resolution
      for (r in seq_along(res)) {
        # load specific field if needed
        f <- ifelse(!is.na(maps_list$field[i]), maps_list$field[i], "")
        # the roads fraction as only map needs the option 'cover = TRUE'
        c <- F
        cover_maps <- c("roads_fraction", "buildings", "hard_surface")
        if (maps_list$name[i] %in% cover_maps)
          c = T
        # if (maps_list$name[i] == "buildings")
        #   c = T
        # if (maps_list$name[i] == "hard_surface")
        #   c = T
        
        # resample 
        map_res <- terra::rasterize(map, mask[[r]], field = f, cover = c, fun = maps_list$fun[i])
        map_out <- paste0(res_dir[[r]], maps_list$name[i], ".map")
        writeRaster(map_res, map_out , filetype = "PCRaster", NAflag = -9999,
                    overwrite = TRUE, gdal = "PCRASTER_VALUESCALE = VS_SCALAR")
      }
    } else if(maps_list$type[i] == "raster") {
      map <- rast(paste0(dir_sd, maps_list$name_in[i]))
      for (r in seq_along(res)) {
        map_res <- terra::resample(map, mask[[r]], method = maps_list$method[i])
        map_out <- paste0(res_dir[[r]], maps_list$name[i], ".map")
        writeRaster(map_res, map_out , filetype = "PCRaster", NAflag = -9999,
                    overwrite = TRUE, gdal = "PCRASTER_VALUESCALE = VS_SCALAR")
      }
    } else (return(message("ERROR: wrong type!")))
    
  } #end maps list loop
  

  # run a PCRaster script to do some additional step for all resolutions
  # 1: remove ponds from dhydro domain
  # 2: fill nodata values in soil UBC profile.map
  # 3: burn dem corrections sections into dem
  for (r in seq_along(res)) {
    pcr_script("maps_prepare.mod", script_dir = "sources/pcr_scripts",
               work_dir = res_dir[r])
    #clean up
    file.remove(paste0(res_dir[[r]], "p_mv.map")) # soil profile with MVs
    # remove all aux.xml files
    aux_files <- list.files(res_dir[[r]], pattern = "aux.xml", full.names = TRUE)
    if (length(aux_files) > 0) {
      file.remove(aux_files)
    }
  }
    
  } # end section only_NBS == FALSE
  
  # loop over NBS_maps
  for (i in seq_along(NBS_maps)) { 
    map <- rast(paste0(dir_sd, "NBS_maps/", NBS_maps[i]))
    for (r in seq_along(res)) {
      map_res <- terra::resample(map, mask[[r]], method = "near")
      map_out <- paste0(res_dir[[r]], NBS_out[i])
      writeRaster(map_res, map_out , filetype = "PCRaster", NAflag = -9999,
                  overwrite = TRUE, gdal = "PCRASTER_VALUESCALE = VS_SCALAR")
    }
  }
  
} #end function spatial_data_to_pcr

##  catchment based dem etc 
# function to delineate the catchment and resample maps to correct resolutions

catch_maps_res <- function() {
  
  points <- read_csv("sources/setup/outpoints_description.csv", show_col_types = FALSE)
# load the base_catchment_20m
base_catch_20m <- rast("spatial_data/base_catchment_20m.map")
base_dem_5m <- rast("spatial_data/dem_region_5m.map")
# we use 3 resolutions for the dataset
cell_size <- unique(points$cell_size)

# create maps for base database if they do not exist

for(i in seq_along(cell_size)) {
  # make folder structure
  res_dir <- paste0("LISEM_data/Geul_", cell_size[i], "m/")
  map_dir <- paste0(res_dir, "maps/")
  if(!dir.exists(res_dir)) {
    dir.create(res_dir)
    dir.create(map_dir)
  }
  
  #write maps
  #mask
  mask <- rast(paste0("spatial_data/mask_", cell_size[i],"m.map"))
  out <- paste0(res_dir, "maps/mask.map")
  writeRaster(mask, out, filetype = "PCRaster", NAflag = -9999,
              overwrite = TRUE, gdal = "PCRASTER_VALUESCALE = VS_SCALAR")
  #dem
  out <- paste0(res_dir, "maps/dem.map")
  dem <- terra::resample(base_dem_5m, mask, method = "average")
  writeRaster(dem, out, filetype = "PCRaster", NAflag = -9999,
              overwrite = TRUE, gdal = "PCRASTER_VALUESCALE = VS_SCALAR")
  
  #catchment
  out <- paste0(res_dir, "maps/catchment.map")
  catch <- terra::resample(base_catch_20m, mask, method = "near")
  writeRaster(catch, out, filetype = "PCRaster", NAflag = -9999,
              overwrite = TRUE, gdal = "PCRASTER_VALUESCALE = VS_SCALAR")
  
}
} #end function catch_maps_res

## make map with subcatchments and ldd
#based on csv file with outpoint coordinates

ldd_subcatch <- function(force_ldd = FALSE) {
# load the outpoints csv file
# if more subcatchment or outpoints are required, these can manually be added
# to this file

  points <- read_csv("sources/setup/outpoints_description.csv", show_col_types = FALSE)
# loop over resolutions
cell_size <- unique(points$cell_size)

for(i in seq_along(cell_size)) {
  subdir <- paste0("LISEM_data/Geul_", cell_size[i], "m/maps/")
  res <- cell_size[i]
  
  # filter the correct resolution
  points_res <- points %>%
    filter(cell_size == res) %>%
    select(x, y, point)
  
  #message("outpoints: ",points_res) #VJ
  
  # write csv table
  write_csv(points_res, file = paste0(subdir, "outpoints.txt"),
            col_names = FALSE)
  # run col2map
  col2map(col_in = "outpoints.txt", map_out = "outpoints.map",
          sub_dir = subdir, options = "-N")
  
  # generate ldd
  if(!file.exists(paste0(subdir,"ldd.map")) | force_ldd == T){
  pcr_script("base_ldd.mod", script_dir = "sources/pcr_scripts/",
             work_dir = subdir)
  }
  # make the subcatchment map
  pcrcalc(
    work_dir = subdir,
    options = paste0("subcatch.map=subcatchment(ldd.map, outpoints.map)")
  )
  
  # clean up
  # remove outpoints.txt
  file.remove(paste0(subdir, "outpoints.txt"))
  # remove all aux.xml files
  aux_files <- list.files(subdir, pattern = "aux.xml", full.names = TRUE)
  if (length(aux_files) > 0) {
    file.remove(aux_files)
  }
}

}

# OLD FUNCTIONS NOT USED ANYMORE 2025-12-03 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# source_to_base_maps <- function(
#     map_in = "",
#     map_out = "",
#     resample_method = "max"
#     ) 
# {
#   
#   resolution <- c(5, 20)
#   for (i in seq_along(resolution)) {
#     
#     # set the main directory
#     main_dir <- paste0("LISEM_data/Geul_", resolution[i], "m/")
#     map_clone = paste0(main_dir, "maps/mask.map")
#     map_out_name = paste0(main_dir, "maps/", map_out, ".map")
#     tmp_tif = paste0(main_dir, "maps/tmp.tif")
#     srs = "EPSG:28992"
# 
#     if (DEBUGm) message("creating => ",map_out_name)
#     
#     # Extract extent, resolution, etc. from the reference raster
#     ref <- raster(map_clone)
#     xmin <- xmin(ref)
#     ymin <- ymin(ref)
#     xmax <- xmax(ref)
#     ymax <- ymax(ref)
#     ncol <- ncol(ref)
#     nrow <- nrow(ref)
#     
#     # gdalwarp makes a tif, PCRaster cannot be done directly because of valuescale
#     gdalwarp(
#       srcfile = map_in,
#       dstfile = tmp_tif,
#       t_srs   = srs,         
#       te      = c(xmin, ymin, xmax, ymax),
#       ts      = c(ncol, nrow),
#       r       = resample_method,    
#       overwrite = TRUE
#     )
#     
#     # use gdaltranslate to create PCRaster map  
#     if (file.exists(map_out_name)) file.remove(map_out_name)
#     gdal_translate(
#       src_dataset = tmp_tif,
#       dst_dataset = map_out_name,
#       ot = "Float32",
#       of = "PCRaster",
#       mo = "PCRASTER_VALUESCALE=VS_SCALAR"
#     )
# 
#   } # end of resolution loop
# 
# } # end of function
# 
# 
# #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# # function to resample 5 m and 20 meter maps for the base dataset
# # base sources:
# # 1. all maps are in 5 meter resolution
# # 2. initial head maps are in 20m resolution.
# 
# resample_base_maps <- function(resolution = 5) {
#   # base_maps
#   main_dir <- paste0("LISEM_data/Geul_", resolution, "m/")
#   base_dir <- paste0("LISEM_data/Base_Geul_5m/maps/")
#   base_maps <- dir(base_dir, recursive = TRUE)
# 
#   # check if main directory exist, otherwise make it
#   if (!dir.exists(main_dir)) {
#     dir.create(main_dir, recursive = TRUE)
#     dir.create(paste0(main_dir, "maps/"), recursive = TRUE)
#   }
#   
#   for (i in seq_along(base_maps)) {
#     # if resolution = 5, copy from base to Geul
#     if (resolution == 5) {
#       file = paste0(main_dir, "maps/", base_maps[i])
#     # if different, copy and resample
#     } else {
#       file = paste0(main_dir, "maps/base_", base_maps[i])
#     }
#     file.copy(
#       from = paste0(base_dir, base_maps[i]),
#       to = file,
#       overwrite = TRUE
#     )
#   }
# 
#   # warning! need an existing mask.map! now only available for 20 and 5 m
#   # should be placed in /LISEM_data/Base_Geul_xxm/maps/mask.map
#   # other resolution specific maps: outpoints, dem, ldd
#   if (resolution != 5) {
#   
#   maps_manual <- dir(paste0("LISEM_data/Base_Geul_", resolution, "m/maps"), full.names = T)
#   map_clone = maps_manual[3]
#   srs = "EPSG:28992"
#   tmp_tif = paste0(main_dir, "maps/tmp.tif")
#   if (DEBUGm) message("resampling => ", resolution, "m maps")
#   
#   # Extract extent, resolution, etc. from the reference raster
#   ref <- raster(map_clone)
#   xmin <- xmin(ref)
#   ymin <- ymin(ref)
#   xmax <- xmax(ref)
#   ymax <- ymax(ref)
#   ncol <- ncol(ref)
#   nrow <- nrow(ref)
#   
#   map_in <- dir(paste0(main_dir, "maps"), full.names = T)
#   
#   for (i in seq_along(map_in)) {
#   # gdalwarp makes a tif, PCRaster cannot be done directly because of valuescale
#   gdalwarp(
#     srcfile = map_in[i],
#     dstfile = tmp_tif,
#     t_srs   = srs,         
#     te      = c(xmin, ymin, xmax, ymax),
#     ts      = c(ncol, nrow),
#     r       = "near",
#     overwrite = TRUE
#   )
#   
#   map_out_name <- paste0(main_dir, "maps/", base_maps[i])
#   # use gdaltranslate to create PCRaster map  
#   if (file.exists(map_out_name)) file.remove(map_out_name)
#   gdal_translate(
#     src_dataset = tmp_tif,
#     dst_dataset = map_out_name,
#     ot = "Float32",
#     of = "PCRaster",
#     mo = "PCRASTER_VALUESCALE=VS_SCALAR"
#   )
#     
#   }
#   
#   file.remove(
#     list.files(
#       path = paste0(main_dir, "maps/"),
#       pattern = "base_|xml|tif",
#       full.names = TRUE
#     )
#   )
#   # add the files that are resolution specific
#   for (k in seq_along(maps_manual)) {
#   file.copy(from = maps_manual[k],
#             to = paste0(main_dir, "maps/"),
#             overwrite = TRUE)
#   }
#    }
# }

