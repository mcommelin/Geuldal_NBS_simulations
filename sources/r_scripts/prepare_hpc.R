# convert dhydro domain to pcraster format

#load dhydro domain
dhydro <- st_read("spatial_data/hpc_maps.gpkg", layer = "dhydro_boven_kelmis")
#make spatvector
d <- vect(dhydro)

# load basic maps etc 
res <- c(5, 10, 20)

#maps_list <- read_csv("sources/transformations_maps.csv")
dir_sd <- "spatial_data/"

# load outline of catchment to reduce data load
catch_poly <- st_read(paste0(dir_sd,"catchment.gpkg"), layer = "catch_buffered_250")
#prepare resolution specific base data
res_dir <- paste0("LISEM_data/Geul_", res, "m/maps/")
mask <- vector("list", length = length(res))
for (r in seq_along(res)) {
  mask[[r]] <- rast(paste0(dir_sd, "mask_", res[r], "m.map"))


#rasterize
map_res <- terra::rasterize(d, mask[[r]])
# write as pcr map
map_out <- paste0(res_dir[[r]], "dhydro", ".map")
writeRaster(map_res, map_out , filetype = "PCRaster", NAflag = -9999,
            overwrite = TRUE, gdal = "PCRASTER_VALUESCALE = VS_SCALAR")
}

# call pcr script to make the subcatchments
pcr_script(script = "hpc_subcatch.mod",
           script_dir = "sources/pcr_scripts/",
           work_dir = res_dir[r])

# make vector