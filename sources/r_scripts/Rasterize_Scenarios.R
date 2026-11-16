#' This script is meant as preprocessing before the actual workflow which
#' is available in this repository.
#' The goal is to prepare raster data for NBS scenarios from vector layers
#' 

# 1. package and functions -----------------------------------------------------
#library(terra) # -> not needed when part of workflow - loaded in configuration.R

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
  
  # MC - maybe better to hardcode the order - or even give a order option in the function.
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
rasterize_NBS_measures <- function(vectors, mask, dem, 
                                   swale_spacing = NULL,
                                   graft_spacing = NULL){
  
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
  
  # loop over rasters to see if swales or graften are present
  # for swales and graften the area assigned must be updated to show the actual line elements
  for (i in seq_along(rasters)) {
  nbs_num <- unique(rasters[[i]]$NBS_ID)
  
  
  # can use same apporach for both LE
  if (nbs_num %in% c(17, 19)) {
    if (nbs_num == 17) {
      spacing <- swale_spacing
    } else {
      spacing <- graft_spacing
    }
    
  dem_nbs <- terra::resample(dem, rasters[[i]])
  
  # make contour lines base don the dem in the polygons
  blocks_poly <- vectors[[i]]
  dem_b       <- mask(crop(dem, blocks_poly), blocks_poly)
  # spacing is based on user defined vertical distance
  lev         <- seq(floor(min(dem_b[], na.rm = TRUE)),
                     ceiling(max(dem_b[], na.rm = TRUE)), by = spacing)
  contours    <- as.contour(dem_b, levels = lev)     # SpatVector lines
  contours$NBS_ID <- nbs_num
  
  # rasterize
  line_rast <- terra::rasterize(contours, rasters[[i]], field = "NBS_ID", 
                                touches = TRUE, background = NA)
  # replace polygon raster
  rasters[[i]] <- line_rast
  }
 
  }
  
  # add mask raster to list at the end. This will make sure the entire geul catchment is covered.
  rasters <- c(rasters, list(mask))
  
  # combine rasters 1 by 1 using cover function, first raster in list goes first.
  # cover function only fills NA cells of the first supplied raster with values from the second supplied raster.
  # So most important layer has to go first!
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

# 2. preprosessing scripts -----------------------------------------------------

# swales and graften need to be filled in on the polygons assigned.


# 3. make raster of scenario ---------------------------------------------------
# load 5m mask of Geul catchment
# example path: "E:/GitHub/Geuldal_NBS_simulations/spatial_data/mask_5m.map"
mask <- rast("spatial_data/mask_5m.map") # point to wherever 5m mask layer is stored
mask[mask == 1] <- 0

# also load the dem, needed for preprocessing of some line element NBS.
# run LISEM data preparation functions on 5m first!
dem <- rast("LISEM_data/Geul_5m/maps/dem.map")

# function to load NBS shapefiles and add to list sorted by priority
#example path: "E:/Work/WUR/LandEX/GIS/Pesakerdal_Snelweg_2"
folder_path <- "data/data_landex/Bocholtz_Middenweg_2/"

# run the functions to make everything
vectorlist <- load_NBS_files(folder_path)
final_raster <- rasterize_NBS_measures(vectorlist, mask, dem,
                                       swale_spacing = 5,
                                       graft_spacing = 10)

# store final maps directly in the correct ./spatial_data_folder
# alternative path: "E:/Work/WUR/LandEX/GIS/Rasters/Pesakerdal_Snelweg_2.tif"
# set some properties:

scen_num <- 141 # [>100] choose a scenario number which does not yet exist in the 
                #folder ./spatial_data/NBS_maps/scenarios
scen_name <- "test" # give a name that cover sthe content - will be used to 
                    # give names to the LISEM simulation folders

# TODO can we combine small scale maps (e.g groeneweg 1 from pesaken en Bocholtz) into 1?

writeRaster(final_raster, paste0("spatial_data/NBS_maps/scenarios/", scen_num, "_", scen_name, ".tif"))
