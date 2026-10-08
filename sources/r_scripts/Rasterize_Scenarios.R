library(terra)


# function to load all shapefiles containing NBS measures in a map as SpatVector, adding them to a list,
# and sorting the list by priority.
# This function serves as preparation for the function rasterize_NBS_measures.
load_NBS_files <- function(folder_path){
  # Find all shapefiles in the given map
  files <- list.files(
    folder_path,
    pattern = "\\.shp$",
    full.names = TRUE
  )
  # load all shapefiles in the map with terra as SpatVectors
  vectors <- lapply(files, terra::vect)
  # check the priority of SpatVectors: line elements are given priority over polygons.
  priority <- sapply(vectors, function(x){
    if ('lines' %in% geomtype(x)) {
      2
    } else if ('polygons' %in% geomtype(x)) {
      1
    } else {
      0
    }
  })
  # sort the list according to priority
  vectors <- vectors[order(priority, decreasing = TRUE)]
}


# function to rasterize shapefiles of NBS measures
rasterize_NBS_measures <- function(vectors, mask){
  
  # for each vector in list, rasterize and add to new list containing the rasters.
  rasters <- lapply(seq_along(vectors), function(i){
    rasterize(
      vectors[[i]],
      mask,
      background = NA,
      # Rasterize by the assigned NBS value --> has to be the same as in NBS lookup table.
      field = "NBS_ID",
      touches = TRUE # for line vectors, to make sure they are connected in raster.
    )})
  
  # add mask raster to list at the end. This will make sure the entire geul catchment is covered.
  rasters <- c(rasters, list(mask))
  
  # combine rasters 1 by 1 using cover function, first raster in list goes first.
  # cover function only fills NA cells of the first supplied raster with values from the second supplied raster.
  # So most inportant layer has to go first!
  combined <- cover(rasters[[1]], rasters [[2]])
  if (length(rasters) > 2) {
    for (i in 3:length(rasters)){
      combined <- cover(combined, rasters[[i]])
    }
  }
  # clip the combined raster to the mask
  combined_clipped <- mask(combined, mask)
  
  return(combined_clipped)
}

# load 5m mask of Geul catchment
mask <- rast("E:/GitHub/Geuldal_NBS_simulations/spatial_data/mask_5m.map") # point to wherever 5m mask layer is stored
mask[mask == 1] <- 0
# function to load NBS shapefiles and add to list sorted by priority
folder_path = "E:/Work/WUR/LandEX/GIS/Pesakerdal_Snelweg_2"

vectorlist <- load_NBS_files(folder_path)
final_raster <- rasterize_NBS_measures(vectorlist, mask)

writeRaster(final_raster, "E:/Work/WUR/LandEX/GIS/Rasters/Pesakerdal_Snelweg_2.tif")
