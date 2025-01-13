#! --clone mask.map --lddin --matrixtable
#------------------------------
#clean up base maps for Geuldal NBS

binding

# load maps
catch_r = catch_region_20m_mask.map;
dem_r = dem_region_20m_mask.map;
soil_r = soil_region_20m_mask.map;
lu_r = lu_20m_region.map;


#newmaps
catch = catch.map;
dem = dem.map;
soil = soil.map;
lu = lu.map;

initial

# add 0 to all nodata of catchment map
catch = cover(boolean(catch_r), 0);

#clip maps to catchment
dem = if(catch, dem_r);
soil = if(catch, soil_r);
lu = if(catch, lu_r);