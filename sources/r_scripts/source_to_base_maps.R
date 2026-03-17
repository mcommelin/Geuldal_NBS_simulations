# functions to convert the base data from ./spatial_data to Geul input

## spatial_data to Geul_xm 
spatial_data_to_pcr <- function(only_NBS = FALSE,
                                res = NULL) {
  if (is.null(res)) {
  res <- c(5, 10, 20)
  } 
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
          {c = T}
        
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
  # 4: calculate flowboundarys for hpc subcatchments
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

ldd_subcatch <- function(force_ldd = FALSE,
                         res = NULL) {
# load the outpoints csv file
# if more subcatchment or outpoints are required, these can manually be added
# to this file

  points <- read_csv("sources/setup/outpoints_description.csv", show_col_types = FALSE)
# loop over resolutions
  if (is.null(res)) {
cell_size <- unique(points$cell_size)
  } else {
  cell_size <- res
  }
  
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


copy_spatial_data <- function() {
  # make folder structure
  if(!dir.exists("./LISEM_data")) {
    dir.create("./LISEM_data")
  }
  if(!dir.exists("./LISEM_runs")) {
    dir.create("./LISEM_runs/rain", recursive = TRUE)
  }
  
  # copy files
file.copy(list.files("./spatial_data/prepared/LISEM_data", full.names = T),
          "./LISEM_data", recursive = T)
file.copy(list.files("./spatial_data/prepared/rain", full.names = T),
          "./LISEM_runs/rain", recursive = T)
}